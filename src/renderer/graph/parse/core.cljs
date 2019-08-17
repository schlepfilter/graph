(ns graph.parse.core
  (:refer-clojure :exclude [= not= some])
  (:require [graph.parse.derived :as derived]
            [graph.parse.primitive :as primitive]))

(def pure
  primitive/pure)

(def mempty
  primitive/mempty)

(def satisfy
  primitive/satisfy)

(def many
  derived/many)

(def some
  derived/some)

(def parse
  derived/parse)

(def any
  derived/any)

(def =
  derived/=)

(def not=
  derived/not=)
