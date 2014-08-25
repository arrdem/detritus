(ns detritus.pred)

(defn ->ensure
  ([v pred]
     (->ensure v pred "->assert failed!"))

  ([v pred error]
     (if (pred v) v
         (assert false error))))


(defn ->>ensure
  ([pred v]
     (->>ensure pred "->>assert failed!" v))

  ([pred error v]
     (if (pred v) v
         (assert false error))))


(defn maybe-fix [pred resolution]
  (fn [x]
    (if-not (pred x)
      (resolution x)
      x)))
