(ns triple.view.react.native-classnames
  (:require [applied-science.js-interop :as j]
            [triple.view.hiccup :as hiccup]
            [triple.util.memo :as memo]
            ["react-native" :as RN])
  #?(:cljs (:require-macros triple.view.react.native-classnames)))

(goog/exportSymbol "RN" RN)

(defonce class-registry #js{})

(defn- registry-lookup [styles class-name]
  (some->> (j/!get class-registry class-name)
           (j/extend! styles)))

(defonce class-functions #js[registry-lookup])

(def ^:private resolve-class-string
  (memo/by-string
    (fn [class-string]
      (let [out #js{}]
        (doseq [class-name (.split class-string #" +")
                class-fn class-functions
                :let [res (class-fn out class-name)]
                ;; continue until class-fn returns something
                :while (nil? res)])
        out))))

(defn register-class-fn!
  "Adds `f` to list of class lookup functions.

  `f` should be a function of [style-obj, class-name] which returns
   the mutated style-obj IF lookup is successful, otherwise nil."
  [key f]
  (let [identifier (str key)
        ident-key (str ::key)]
    (j/call resolve-class-string :clear)
    ;; remove previous function with same identifier
    (.filter class-functions (fn [f] (not= identifier (j/get f ident-key))))
    ;; attach identifier to function
    (j/!set f ident-key identifier)
    ;; add function to registry
    (j/unshift! class-functions f)))

(register-class-fn! ::registry-lookup registry-lookup)

(defn register-class! [class-name style-obj]
  (j/!set class-registry class-name style-obj))

(hiccup/set-prop-handler!
  "className"
  (j/fn [^:js js-props _ className]
    (js-delete js-props "className")
    (j/!update js-props :style
               (fn [existing-styles]
                 (cond->> (resolve-class-string className)
                          (array? existing-styles)
                          (j/push! existing-styles))))))