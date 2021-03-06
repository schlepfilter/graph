(ns graph.core
  (:require [aid.core :as aid]
            [cljs-node-io.fs :as fs]
            [frp.core :as frp]
            [graph.helpers :as helpers]))

(def electron
  (js/require "electron"))

(def path
  (js/require "path"))

(def window-state-keeper
  (js/require "electron-window-state"))

(frp/defe file-path)

(def app
  electron.app)

(.on app
     "ready"
     (fn [_]
       (let [window-state (window-state-keeper. {})
             window (electron.BrowserWindow. window-state)]
         (doto window
           (.on "close" (fn [event*]
                          (.preventDefault event*)
                          (.webContents.send window
                                             helpers/channel
                                             (clj->js #js ["close"]))))
           (.webContents.on "did-finish-load"
                            (fn []
                              (frp/run #(.webContents.send window
                                                           helpers/channel
                                                           #js ["open" %])
                                       file-path)
                              (frp/activate)))
           (.loadURL
             (->> "index.html"
                  (helpers/get-path helpers/public)
                  (path.join (aid/if-else (comp (partial =
                                                         helpers/resources)
                                                fs/basename)
                                          (comp fs/dirname
                                                fs/dirname)
                                          js/__dirname))
                  (str "file://")))
           window-state.manage))))

(.on app "will-finish-launching" #(.on app "open-file" (comp file-path
                                                             last
                                                             vector)))
