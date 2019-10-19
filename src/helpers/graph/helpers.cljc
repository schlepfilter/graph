(ns graph.helpers
  (:require [clojure.string :as str]))

(def app-name
  "graph")

(def channel
  "channel")

(def get-path
  (comp (partial str/join "/")
        vector))

(def resources
  "resources")

(def public
  "public")
