(ns detritus.types
  (:refer-clojure :exclude [map-entry?]))

;; Concrete type predicates
;;--------------------------------------------------------------------
(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defn ^:deprecated map-entry? [x]
  (clojure.core/map-entry? x))

(defn lazy-seq? [x]
  (instance? clojure.lang.LazySeq x))

(defn pattern? [x]
  (instance? java.util.regex.Pattern x))

(defn uuid? [x]
  (instance? java.util.UUID x))

(defn uri? [x]
  (instance? java.net.URI x))

(defn boolean? [x]
  (instance? java.lang.Boolean x))

(defn long? [x]
  (instance? java.lang.Long x))

(defn int? [x]
  (instance? java.lang.Integer x))

(defn big-int? [x]
  (or (instance? java.math.BigInteger x)
      (instance? clojure.lang.BigInt x)))

(defn file? [x]
  (instance? java.io.File x))

;; Interface predicates
;;--------------------------------------------------------------------
(defn ref? [x]
  (instance? clojure.lang.IRef x))

(defn deref? [x]
  (instance? clojure.lang.IDeref x))

(defn named? [x]
  (instance? clojure.lang.Named))

(defn throwable? [x]
  (instance? java.lang.Throwable x))

(defn seqable? [x]
  (or (instance? clojure.lang.ISeq x)
      (instance? clojure.lang.Seqable x)
      (instance? Iterable x)
      (instance? CharSequence x)
      (instance? java.util.Map x)
      (nil? x)
      (.. x getClass isArray)))
