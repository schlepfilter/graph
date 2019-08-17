(ns graph.parse.derived
  (:refer-clojure :exclude [= not= some])
  (:require [clojure.core :as core]
            [aid.core :as aid]
            [com.rpl.specter :as s]
            [graph.parse.primitive :as primitive]))

;TODO delete this definition when Clojure supports non-strict evaluation
;this definition gives StackOverflowError
;many can be theoretically derived from <>, some, pure, and lift-a without using a constructor
;using a constructor is a workaround for strict evaluation
;(declare some)
;
;(defn many
;  [parser]
;  (primitive/->Parser #((m/<> (some parser)
;                              primitive/mempty) %)))

;this definition avoids StackOverflowError
(aid/defcurried step
  [parser [state coll]]
  (->> coll
       parser
       (s/transform [s/ALL s/FIRST]
                    (comp (partial s/setval* s/BEGINNING state)
                          list))))

(defn many
  [parser]
  (primitive/->Parser (comp #(let [coll (-> parser
                                            step
                                            (mapcat %)
                                            (concat %)
                                            distinct)]
                               (aid/case-eval %
                                 coll %
                                 (recur coll)))
                            vector
                            (partial vector '()))))

(def some
  (aid/build (aid/lift-a cons)
             identity
             many))

(def parse
  (comp (partial map first)
        (partial filter (comp empty?
                              second))
        (partial apply aid/funcall)
        vector))

(def any
  (primitive/satisfy (constantly true)))

(def =
  (comp primitive/satisfy
        (aid/curry 2 core/=)))

(def not=
  (comp primitive/satisfy
        (aid/curry 2 core/not=)))
