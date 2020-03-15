(ns triple.util.memo
  (:require [applied-science.js-interop :as j]))

(defn by-string
  "Memoizes f, using cljs object lookups (for perf)"
  [f]
  #?(:clj
     (memoize f)

     :cljs
     (let [mem (volatile! #js{})]
       (-> (fn [x]
             (j/!get @mem ^string x
                     (let [v (f x)]
                       (j/!set @mem x v)
                       v)))

           ;; clear the memory
           (j/assoc! :clear #(vreset! mem #js{}))))))

(defn clear! [memoized-fn]
  (j/call memoized-fn :clear))