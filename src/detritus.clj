(ns detritus
  (:refer-clojure :exclude [namespace name]))

(defn zip
  "Zips the argument collections together, returning striped sequences
  of the nth element from each sequence.

  ```
  user> (zip [1 2 3]
             [4 5 6])
  ([1 4]
   [2 5]
   [3 6])
  ```"
  [& colls]
  (map vector colls))

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
    (name (.sym v)))
  (namespace [^clojure.lang.Var v]
    (name (.ns v)))

  java.lang.String
  (name [^String str]
    str)
  (namespace [_] nil)

  java.lang.Class
  (name [^Class c]
    (.getSimpleName c))
  (namespace [^Class c]
    (.getName (.getPackage c))))

(defn eprintf
  "`#'printf` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply printf args)))

(defn eprint
  "`#'print` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply print args)))

(defn eprintln
  "`#'println` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn epr
  "`#'pr` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply pr args)))

(defn eprn
  "`#'prn` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply prn args)))
