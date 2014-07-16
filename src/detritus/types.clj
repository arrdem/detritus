(ns detritus.types)

(defn atom? [x]
  (instance? clojure.lang.Atom x))
