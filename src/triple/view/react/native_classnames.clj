(ns triple.view.react.native-classnames
  (:require [applied-science.js-interop :as j]
            [clojure.string :as str]))

(defn ->camel [^String k]
  (-> (name k)
      (str/replace #"-(\w)" (comp str/upper-case second))
      (keyword)))

(defn litc
  "Like j/lit, but converts keys to camelCase"
  [m]
  (j/lit* {:keyfn #(cond-> % (keyword? %) ->camel)} m))

(defmacro styles
  "Creates validated stylesheet object from a literal Clojure map
   (converts keys to camelCase)"
  [m]
  `(let [obj# ~(litc m)]
     (if ~'goog/DEBUG
       ;; validate the style object
       (-> (j/call-in js/RN [:StyleSheet :create] (j/obj :x obj#))
           (j/get :x))
       obj#)))

(defmacro stylesheet!
  "Returns Clojure map containing validated style objects

  Usage:
  (classes
    :class-1 {...styles}
    :class-2 {...styles})"
  [& classes]
  (assert (even? (count classes)))
  `(do ~@(for [[k v] (partition 2 classes)]
           `(~'triple.view.react.native-classnames/register-class!
              ~(name k)
              (styles ~v)))))

(defmacro defstyle [the-name styles]
  (if (keyword? the-name)
    `(~'triple.view.react.native-classnames/register-class!
       ~(name the-name)
       (styles ~styles))
    `(def ~the-name (styles ~styles))))