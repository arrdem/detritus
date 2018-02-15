(ns detritus.types
  "Type predicates, some of which are in core and some of which aren't."
  (:refer-clojure :exclude [map-entry? uuid? uri? int? seqable? boolean?]))

(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defn ^:deprecated map-entry? [x]
  (clojure.core/map-entry? x))

(defn lazy-seq? [x]
  (instance? clojure.lang.LazySeq x))

(defn pattern? [x]
  (instance? java.util.regex.Pattern x))

(def ^:deprecated uuid?
  #'clojure.core/uuid?)

(defn uri? [x]
  (instance? java.net.URI x))

(def ^:deprecated boolean?
  #'clojure.core/boolean?)

(defn long? [x]
  (instance? java.lang.Long x))

(def ^:deprecated int?
  #'clojure.core/int?)

(defn big-int? [x]
  (or (instance? java.math.BigInteger x)
      (instance? clojure.lang.BigInt x)))

(defn file? [x]
  (instance? java.io.File x))

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
