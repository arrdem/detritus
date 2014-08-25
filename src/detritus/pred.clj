(ns detritus.pred)

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
     (->ensure v pred "->assert failed!"))

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
     (->>ensure pred "->>assert failed!" v))

  ([pred error v]
     (if (pred v) v
         (assert false error))))
