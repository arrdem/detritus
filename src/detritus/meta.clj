(ns detritus.meta
  (:import clojure.lang.IMeta))

(defn massoc [^IMeta o & kvs]
  (with-meta o
    (apply assoc (meta o) kvs)))

(defn mdissoc [^IMeta o & ks]
  (with-meta o
    (apply dissoc (meta o) ks)))

(defn mupdate [^IMeta o k f & args]
  (with-meta o
    (apply update (meta o) k f args)))
