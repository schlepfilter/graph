;TODO delete this namespace when digraph gets fixed
(ns graph.loom
  (:require [aid.core :as aid]
            [loom.graph :as graph]))

(defn build-graph
  [g & inits]
  (letfn [(build [g init]
            (cond
              ;; graph
              (graph/graph? init)
              (if (and (graph/weighted? g) (graph/weighted? init))
                (assoc
                  (reduce graph/add-edges
                          (graph/add-nodes* g (graph/nodes init))
                          (for [[n1 n2] (graph/edges init)]
                            [n1 n2 (graph/weight init n1 n2)]))
                  :attrs (merge (:attrs g) (:attrs init)))
                (-> g
                    (graph/add-nodes* (graph/nodes init))
                    (graph/add-edges* (graph/edges init))
                    (assoc :attrs (merge (:attrs g) (:attrs init)))))
              ;; adacency map
              (map? init)
              (aid/casep init
                empty? g
                (let [es (if (map? (val (first init)))
                           (for [[n nbrs] init
                                 [nbr wt] nbrs]
                             [n nbr wt])
                           (for [[n nbrs] init
                                 nbr nbrs]
                             [n nbr]))]
                  (-> g
                      (graph/add-nodes* (keys init))
                      (graph/add-edges* es))))
              ;; edge
              (sequential? init) (graph/add-edges g init)
              ;; node
              :else (graph/add-nodes g init)))]
    (reduce build g inits)))

(defn digraph
  [& inits]
  (with-redefs [graph/build-graph build-graph]
               (apply graph/digraph inits)))

(def predecessors
  (comp (aid/if-then nil?
                     (constantly #{}))
        graph/predecessors))

(def successors
  (comp (aid/if-then nil?
                     (constantly #{}))
        graph/successors))
