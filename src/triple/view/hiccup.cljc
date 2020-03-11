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

(j/defn props->js
  "Returns a React-conformant javascript object. An alternative to clj->js,
  allowing for key renaming without an extra loop through every prop map."
  [^:js [tag id classes] props]
  (let [js-props (reduce-kv prop->js (j/obj :id id
                                            :className classes) props)
        id (j/!get js-props :id)
        className (j/!get js-props :className)]
    (cond-> js-props
            (cond/defined? id)
            (prop->js "id" id)

            (cond/defined? className)
            (prop->js "className" (dots->spaces className)))))

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

(defn make-element
  "Returns a React element. `tag` may be a string or a React component (a class or a function).
   Children will be read from `form` beginning at index `start`."
  ([element-type form]
   (let [props (props/get-props form 1)
         props? (cond/defined? props)]
     (make-element element-type
                   (when props?
                     (props->js nil props))
                   form
                   (if props? 2 1))))
  ([element-type js-props form start]
   (let [form-count (count form)]
     (case (- form-count start)                             ;; fast cases for small numbers of children
       0 (react/createElement element-type js-props)
       1 (let [first-child (nth form start)]
           (if (seq? first-child)
             ;; a single seq child should not create intermediate fragment
             (make-element element-type js-props (vec first-child) 0)
             (react/createElement element-type js-props (to-element first-child))))
       (let [out #js[element-type js-props]]
         (loop [i start]
           (if (== i form-count)
             (.apply react/createElement nil out)
             (do
               (.push out (to-element (nth form i)))
               (recur (inc i))))))))))

(defonce tag-handlers
         (j/obj "#" react/Suspense))

(defn to-element [form]
  (cond (vector? form) (let [tag (-nth form 0)]
                         (cond (keyword? tag)
                               (case tag
                                 :<> (make-element react/Fragment nil form 1)
                                 (j/let [^:js [tag-name :as match] (parse-tag (name tag))
                                         tag (j/!get tag-handlers
                                                     tag-name
                                                     tag-name)
                                         props (props/get-props form 1)
                                         props? (cond/defined? props)]
                                   (make-element tag
                                                 (props->js match (when props? props))
                                                 form
                                                 (if props? 2 1))))
                               :else (make-element tag form)))
        (seq? form) (make-element react/Fragment nil form 0)
        (satisfies? IElement form) (-to-element form)
        (array? form) (make-element (aget form 0) form)
        :else form))

(defn element
  "Converts Hiccup form into a React element. If a non-vector form
   is supplied, it is returned untouched. Attribute and style keys
   are converted from `dashed-names` to `camelCase` as spec'd by React.

   - optional -
   :create-element (fn) overrides React.createElement."
  [form]
  (to-element form))

