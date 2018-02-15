(ns detritus.exceptions)

(defn throwf
  [fmt & args]
  (throw (Exception. (apply format fmt args))))
