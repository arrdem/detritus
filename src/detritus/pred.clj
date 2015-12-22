(ns detritus.pred
  "A collection of predicates and predicate helpers.")

(defn ->assert
  "(λ T → ((λ T) → Bool)) → T
  (λ T → ((λ T) → Bool) → String) → T

  Function of a value, a predicate and optionally a string. If the
  predicate is not true of the value, this function will assert false
  giving the optional string or \"->assert failed!\" as the failure
  message. Otherwise returns the argument value.

  Example:
    (-> 1 inc inc dec
       (->assert number? \"oh god the types\"))"
  ([v pred]
     (->assert v pred "->assert failed!"))

  ([v pred error]
     (if (pred v) v
         (assert false error))))


(defn ->>assert
  "(λ ((λ T) → Bool) → T) → T
  (λ ((λ T) → Bool) → T → String) → T

  Function of a predicate, optionally a string and a value. If the
  predicate is not true of the value, this function will assert false
  giving the optional string or \"->assert failed!\" as the failure
  message. Otherwise returns the argument value.

  Example:
    (->> 1 inc inc dec
      (->>assert number? \"oh god the types\"))"
  ([pred v]
     (->>assert pred "->>assert failed!" v))

  ([pred error v]
     (if (pred v) v
         (assert false error))))


(def ! complement)


;; Missing type predicates
;;--------------------------------------------------------------------

(defn seqable?
  {:depricated  "0.3.1"
   :replaced-by t/seqable?}
  [x]
  (.write *err* "Calling deprecated detritus.pred/seqable?\n")
  (t/seqable? x))

(defn atom?
  {:depricated  "0.3.1"
   :replaced-by t/atom?}
  [x]
  (.write *err* "Calling deprecated detritus.pred/atom?\n")
  (t/atom? x))
