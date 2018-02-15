(ns detritus.named
  (:refer-clojure :exclude [namespace name]))

(defprotocol Named
  (name [obj])
  (namespace [obj]))

(extend-protocol Named
  nil
  (name [_] nil)
  (namespace [_] nil)

  clojure.lang.Named
  (name [^clojure.lang.Named obj]
    (.getName obj))
  (namespace [^clojure.lang.Named obj]
    (name (.getNamespace obj)))

  clojure.lang.Namespace
  (name [^clojure.lang.Namespace ns]
    (name (.name ns)))
  (namespace [^clojure.lang.Namespace ns]
    nil)

  clojure.lang.Var
  (name [^clojure.lang.Var v]
    (some-> v .sym name))
  (namespace [^clojure.lang.Var v]
    (some-> v .ns name))

  java.lang.String
  (name [^String str] str)
  (namespace [_] nil)

  java.lang.Class
  (name [^Class c]
    (.getSimpleName c))
  (namespace [^Class c]
    (.getName (.getPackage c))))
