(ns triple.view.react.native-classnames
  (:require [applied-science.js-interop :as j]
            [triple.view.hiccup :as hiccup]
            [triple.util.memo :as memo]
            [goog.object :as gobj]
            ["react-native" :as RN])
  #?(:cljs (:require-macros triple.view.react.native-classnames)))

(goog/exportSymbol "RN" RN)

(defonce class-registry #js{})
(defonce class-functions #js[])

(def ^:private resolve-class-string
  (memo/by-string
    (fn [^string class-string]
      (let [out #js{}]
        (doseq [class-name (.split class-string #" +")] ;; loop through every classname
          (doseq [class-fn class-functions
                  ;; stop at first function that returns something
                  :while (nil? (class-fn out class-name))]))
        (when-not (gobj/isEmpty out)
          out)))))

(defn- registry-lookup [styles class-name]
  (some->> (j/!get class-registry class-name)
           (j/extend! styles)))

(defn register-class-fn!
  "Adds `f` to list of class lookup functions.

  `f` should be a function of [style-obj, class-name] which returns
   the mutated style-obj IF lookup is successful, otherwise nil."
  [key f]
  (let [identifier (str key)
        ident-key (str ::key)]
    (memo/clear! resolve-class-string)
    ;; remove previous function with same identifier
    (.filter class-functions (fn [f] (not= identifier (j/get f ident-key))))
    ;; attach identifier to function
    (j/!set f ident-key identifier)
    ;; add function to registry
    (j/unshift! class-functions f)))

(register-class-fn! ::registry-lookup registry-lookup)

(defn register-class! [class-name style-obj]
  (memo/clear! resolve-class-string)
  (j/!set class-registry class-name style-obj))

(defn add-styles! [styles new-styles]
  (cond (nil? styles) new-styles
        (nil? new-styles) styles
        (array? styles) (j/push! styles new-styles)
        :else #js[styles new-styles]))

(hiccup/set-prop-handler!
  "style"
  (fn prop-map->js [o k v]
    (j/!set o k (if (vector? v)
                  (reduce #(j/push! %1 (hiccup/map->js-camel %2)) #js[] v)
                  (hiccup/map->js-camel v)))))

(hiccup/set-prop-handler!
  "className"
  (j/fn [^:js js-props _ className]
    (js-delete js-props "className")
    (j/!update js-props :style add-styles! (resolve-class-string className))))