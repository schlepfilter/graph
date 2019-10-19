(ns repl
  (:require [figwheel-sidecar.repl-api :as repl-api]
            [graph.helpers :as helpers]))

(def argument
  (first *command-line-args*))

(def id
  "app")

(def entry
  "main.js")

(def get-resources
  (partial helpers/get-path helpers/resources))

(def js-directory
  "js")

(def get-js
  (partial get-resources helpers/public js-directory))

(def builder
  "builder")

(def main
  "main")

(def renderer
  "renderer")

(def compiler
  (merge
    {:main                 "graph.core"
     :preloads             ['devtools.preload]
     :source-map-timestamp true
     ;TODO use npm-deps when npm-deps becomes stable
     :foreign-libs         [{:file           (get-js "index_bundle.js")
                             :provides       ["ace"
                                              "ace-editor"
                                              "katex"
                                              "measure"
                                              "react"
                                              "react-dom"]
                             :global-exports {'ace        'ace
                                              'ace-editor 'AceEditor
                                              'katex      'katex
                                              'measure    'Measure
                                              'react      'React
                                              'react-dom  'ReactDOM}}]
     :external-config      {:devtools/config {:features-to-install :all}}}
    ({builder  {:output-to (helpers/get-path "target" entry)
                :target    :nodejs}
      main     {:output-to (get-resources entry)
                :target    :nodejs}
      renderer {:output-to  (get-js entry)
                :asset-path (helpers/get-path js-directory "out")}}
      argument)))

(def build
  {:id           id
   :source-paths (map (partial helpers/get-path "src") [argument "helpers"])
   :compiler     compiler
   :figwheel     true})

(repl-api/start-figwheel! {:all-builds       [build]
                           :build-ids        [id]
                           :figwheel-options ({builder  {}
                                               main     {:server-port 3450}
                                               renderer {:server-port 3451}}
                                               argument)})

(repl-api/cljs-repl)
