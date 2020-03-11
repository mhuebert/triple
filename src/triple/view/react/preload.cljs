(ns triple.view.react.preload
  (:require ["react-refresh/runtime" :as refresh]))

(defn inject-hook!
  []
  (refresh/injectIntoGlobalHook goog/global))

(goog/exportSymbol "ReactRefresh" refresh)

(defn ^:dev/after-load refresh!
  []
  (->> (refresh/performReactRefresh)
       (js/console.log "refreshed:")))