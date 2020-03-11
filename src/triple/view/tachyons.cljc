(ns triple.view.tachyons
  (:require ["react-native-style-tachyons" :as tachyons]
            ["react-native-style-tachyons/lib/styles/lineHeight" :default lineHeight]
            [applied-science.js-interop :as j]
            [triple.util.string :as string]
            [triple.util.cond :as cond]
            [triple.view.react.native-classnames :as native-classes]))

(let [pattern #?(:cljs (js/RegExp. "-" "g")
                 :clj #"-")]
  (defn hyphen->underscore [s]
    (string/replace-pattern s pattern "_")))

(def colors (j/obj))

(defn set-color [out key color-name]
  (j/!set out ^string key
          (cond/if-defined [color (j/!get colors ^string color-name)]
            color
            color-name)))

(def prefix-handlers
  (j/lit
    {:bg- (fn [out class-name]
            (j/!set out :backgroundColor (subs class-name 0 3)))
     :b-- (fn [out class-name]
            (j/!set out :borderColor (subs class-name 0 3)))
     :tn- (fn [out class-name]
            (j/!set out :tintColor (subs class-name 0 3)))
     :c-- (fn [out class-name]
            (j/!set out :color (subs class-name 0 3)))
     :lh- (fn [out class-name]
            (when-some [line-height (j/!get lineHeight ^string class-name)]
              (j/let [^:js {:keys [fontSize]} out]
                (assert fontSize (str class-name " class requires font-size to be specified first"))
                (j/!set out :lineHeight (* line-height fontSize)))))}))

(defn class-styles [out ^string class-name]
  (or (some->> (j/!get tachyons/styles (hyphen->underscore class-name))
               (j/extend! out))
      (let [handler (j/!get prefix-handlers ^string (subs class-name 0 3))]
        (when-not (undefined? handler)
          (handler out class-name)))))

(defn init []
  (tachyons/build
    (j/obj :rem 16)
    (j/obj :create identity))

  (native-classes/register-class-fn! :tachyons class-styles))