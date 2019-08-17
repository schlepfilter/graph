(ns graph.core)

(defmacro c
  [& more]
  `(partial vector (memoize-one (fn ~@more))))

(defmacro defc
  [function-name & more]
  `(def ~function-name
     (c ~@more)))
