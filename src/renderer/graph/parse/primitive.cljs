(ns graph.parse.primitive
  (:require [aid.core :as aid]
    ;TODO replace cats when Clojure supports type inference and non-strict evaluation
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.util :as util]
            [com.rpl.specter :as s]))

(declare context)

(defrecord Parser
  [f]
  IFn
  (-invoke [_ x]
    (f x))
  p/Contextual
  (-get-context [_]
    context)
  p/Printable
  (-repr [_]
    (str "#parser[" f "]")))

(def pure
  #(->Parser (comp vector
                   (partial vector %)
                   (partial apply list))))

(def join
  #(->Parser (comp (partial mapcat (partial apply aid/funcall))
                   %)))

(def context
  (reify
    p/Context
    p/Functor
    (-fmap [_ f fa]
      (->Parser (comp (partial s/transform* [s/ALL s/FIRST] f)
                      fa)))
    p/Applicative
    (-pure [_ v]
      (pure v))
    (-fapply [_ af av]
      (aid/ap af av))
    p/Monad
    (-mreturn [_ v]
      (pure v))
    (-mbind [_ mv f]
      (->> mv
           (m/<$> f)
           join))
    p/Semigroup
    (-mappend [_ left-parser right-parser]
      (->Parser (aid/build (comp distinct
                                 concat)
                           left-parser
                           right-parser)))))

(util/make-printable Parser)

(def satisfy
  #(->Parser (aid/if-then-else (aid/build and
                                          not-empty
                                          (comp %
                                                first))
                               (comp vector
                                     (juxt first
                                           rest))
                               (constantly []))))
(def mempty
  (satisfy (constantly false)))
