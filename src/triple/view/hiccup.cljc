(ns triple.view.hiccup
  (:require ["react" :as react]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [triple.util.memo :as memo]
            [triple.util.string :as string]
            [triple.util.cond :as cond]
            [triple.util.props :as props])
  (:require-macros triple.view.hiccup))

(defn map->js-camel
  "Return javascript object with camelCase keys (shallow)"
  [v]
  (->> v
       (reduce-kv (fn [obj k v]
                    (j/!set obj (string/camel-case (name k)) v)) (j/obj))))

(defn prop-map->js [o k v]
  (j/!set o k (cond-> v (map? v) (map->js-camel))))

(def ^:private prop-camel
  "Converts string attribute to camelCase unless prefixed by data- or aria-"
  (memo/by-string
    (fn [k]
      (cond-> k
              (not (or (str/starts-with? k "data-")
                       (str/starts-with? k "aria-")))
              (string/-camel-case)))))

(def ^:private dot-pattern #?(:cljs (js/RegExp "\\." "g")
                              :clj  "."))

(defn dots->spaces [s]
  (string/replace-pattern s dot-pattern " "))

(defn join-classes
  "Handles class strings and vectors"
  [s]
  (if (vector? s)
    (str/join " " (mapv join-classes s))
    s))

(def ^:private prop-handlers
  (j/obj
    :for (fn [o k v] (j/!set o "htmlFor" v))
    :class (fn [o _ v]
             (j/!update o "className"
                        (fn [tag-classes]
                          (cond-> (join-classes v)
                                  (cond/defined? tag-classes)
                                  (str " " tag-classes)))))
    :id (fn [o k v] o)
    :style prop-map->js
    :dangerouslySetInnerHTML prop-map->js
    :default (fn [o k v] (j/!set o (prop-camel k) v))))

(defn set-prop-handler!
  "Register a custom prop handler. prop-name should be a string,
   handler a function of [object, name, value] and is responsible
   for returning a mutated object.

   Special cases:

   class     => value is only from clj :class, merges result with :className
   classname => clj :class merged with the tag's class (eg. :div.my-class)
   "
  [prop-name handler]
  (j/!set prop-handlers prop-name handler))

(defn- prop->js [o k v]
  (if (qualified-keyword? k)
    o
    (let [attr-name (name k)
          handler (cond/if-defined [handler (j/!get prop-handlers attr-name)]
                    handler
                    (j/!get prop-handlers "default"))]
      (handler o attr-name v))))

(defn props->js
  "Returns a React-conformant javascript object. An alternative to clj->js,
  allowing for key renaming without an extra loop through every prop map."
  [match props primitive?]
  (cond (object? props) props
        primitive?
        (j/let [^:js [tag id classes] match
                js-props (reduce-kv prop->js (j/obj :id id
                                                    :className classes) props)
                id (j/!get js-props :id)
                className (j/!get js-props :className)]
          (cond-> js-props
                  (some? id)
                  (prop->js "id" id)

                  (some? className)
                  (prop->js "className" (dots->spaces className))))
        :else
        (let [key (get props :key)
              ref (get props :ref)]
          (cond-> (js-obj)
                  (some? key) (j/!set :key key)
                  (some? ref) (j/!set :ref ref)))))

(defn -parse-tag
  "Returns array of [tag-name, id, classes] from a tag-name like div#id.class1.class2"
  [tag-name]
  (let [pattern #"([^#.]+)?(?:#([^.]+))?(?:\.(.*))?"]
    #?(:cljs (-> (.exec pattern tag-name) (.slice 1 4))
       :clj  (rest (re-find pattern tag-name)))))

(def parse-tag (memo/by-string -parse-tag))

(defprotocol IElement
  (-to-element [this] "Returns a React element representing `this`"))

(declare to-element)

;; WIP
;; Deciding on API for different cases:
;; - Clojure view: pass "props" as child, only extract key and ref
;; - Primitive view (React, React Native): convert props to JS, convert child vectors to hiccup
;; - External JS view: convert props to JS, convert child vectors to hiccup
;; ...how to differentiate between Clojure views and external JS views?
;;    ...a marker protocol?
;;    ...pass a javascript object in the props position?

(defn make-element
  "Returns a React element. `tag` may be a string or a React component (a class or a function).
   Children will be read from `form` beginning at index `start`."
  ([element-type form ^boolean primitive?]
   (let [props (props/get-props form 1)
         props? (cond/defined? props)
         primitive? (or primitive? (and props? (object? props)))]
     (make-element element-type
                   (when props?
                     (props->js nil props primitive?))
                   form
                   (if (and props? primitive?) 2 1)
                   primitive?)))
  ([element-type js-props form children-start primitive?]
   (let [form-count (count form)
         to-element (if primitive? to-element identity)]
     (case (- form-count children-start)                    ;; fast cases for small numbers of children
       0 (react/createElement element-type js-props)
       1 (let [first-child (nth form children-start)]
           (if (seq? first-child)
             ;; a single seq child should not create intermediate fragment
             (make-element element-type js-props (vec first-child) 0 primitive?)
             (react/createElement element-type js-props (to-element first-child))))
       (let [out #js[element-type js-props]]
         (loop [i children-start]
           (if (== i form-count)
             (.apply react/createElement nil out)
             (do
               (.push out (to-element (nth form i)))
               (recur (inc i))))))))))

(defonce tag-handlers
         (j/obj "#" react/Suspense))

(defn to-element
  "Converts Hiccup form into a React element"
  [form]
  (cond (vector? form) (let [tag (-nth form 0)]
                         (if (keyword? tag)
                           (case tag
                             :<> (make-element react/Fragment form true)
                             (j/let [^:js [tag-name :as match] (parse-tag (name tag))
                                     tag (j/!get tag-handlers
                                                 tag-name
                                                 tag-name)
                                     props (props/get-props form 1)
                                     props? (cond/defined? props)
                                     ;; keyword elements are considered React primitives.
                                     ;; - props are converted to js,
                                     ;; - children are passed through `to-element`
                                     primitive? true]
                               (make-element tag
                                             (props->js match (when props? props) primitive?)
                                             form
                                             (if props? 2 1)
                                             primitive?)))
                           (make-element tag form false)))
        (seq? form) (make-element react/Fragment nil form 0 true)
        (satisfies? IElement form) (-to-element form)
        (array? form) (make-element (aget form 0) form true)
        :else form))

