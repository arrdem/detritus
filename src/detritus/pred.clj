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
;; Missing type predicates
;;--------------------------------------------------------------------

(defn seqable? [x]
  (or (instance? clojure.lang.ISeq x)
     (instance? clojure.lang.Seqable x)
     (instance? Iterable x)
     (instance? CharSequence x)
     (instance? java.util.Map x)
     (nil? x)
     (.. x getClass isArray)))


(defn atom? [x]
  (instance? clojure.lang.Atom x))
