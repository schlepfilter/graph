(defproject graph "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.439"]
                 [org.clojure/data.avl "0.1.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [cljs-node-io "1.1.2"]
                 [cljsjs/mousetrap "1.6.2-0"]
                 [frp "0.1.4"]
                 [me.raynes/fs "1.4.6"]
                 [reagent "0.8.1" :exclusions [cljsjs/react]]
                 [thi.ng/geom "1.0.0-RC3"]]
  :plugins [[lein-ancient "0.6.15"]
            [lein-cljsbuild "1.1.7"]]
  :source-paths ["src/helpers"]
  :profiles {:dev      {:dependencies [[binaryage/devtools "0.9.10"]
                                       [figwheel-sidecar "0.5.19"]]}
             ;:renderer profile works around the following error in REPL.
             ;----  Could not Analyze  src/renderer/graph/core.cljs  ----
             ;
             ;  Could not locate graph/core__init.class, graph/core.clj or graph/core.cljc on classpath. Please check that namespaces with dashes use underscores in the Clojure file name.
             ;
             ;----  Analysis Error : Please see src/renderer/graph/core.cljs  ----
             :renderer {:source-paths ["src/renderer"]}}
  :cljsbuild {:builds
              {:builder   {:source-paths ["src/builder" "src/helpers"]
                           :compiler     {:output-to "target/main.js"
                                          :main      graph.core
                                          :target    :nodejs}}
               :main-dev  {:source-paths ["src/helpers" "src/main"]
                           :compiler     {:output-to     "resources/main.js"
                                          :optimizations :simple
                                          :main          graph.core}}
               :main-prod {:source-paths ["src/helpers" "src/main"]
                           :compiler     {:output-to       "resources/main.js"
                                          :optimizations   :simple
                                          :main            graph.core
                                          :closure-defines {goog.DEBUG false}}}
               :renderer  {:source-paths ["src/helpers" "src/renderer"]
                           :compiler     {:output-to       "resources/public/js/main.js"
                                          :optimizations   :simple
                                          :main            graph.core
                                          :foreign-libs    [{:file           "resources/public/js/index_bundle.js"
                                                             :provides       ["ace"
                                                                              "ace-editor"
                                                                              "katex"
                                                                              "measure"
                                                                              "react"
                                                                              "react-dom"]
                                                             :global-exports {ace        ace
                                                                              ace-editor AceEditor
                                                                              katex      katex
                                                                              measure    Measure
                                                                              react      React
                                                                              react-dom  ReactDOM}}]
                                          :closure-defines {goog.DEBUG false}}}}})
