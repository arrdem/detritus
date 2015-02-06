(ns detritus.variants
  (:require [detritus.update :refer [take-when]]))

(defprotocol ITaggedVal
  (-tag [_])
  (-val [_]))

(deftype ATaggedVal [t v]
  ITaggedVal
  (-tag [self]
    (.t self))
  (-val [self]
    (.v self))

  clojure.lang.Indexed
  (nth [self i] (nth self i nil))
  (nth [self i o]
    (case i
      (0)      (.t self)
      (1)      (.v self)
      o))
  
  clojure.lang.ISeq
  (next [this] (seq this))
  (first [this] (.t this))
  (count [this] 2)
  (equiv [this obj] (= (seq this) (seq obj)))
  (seq [self]
    (cons (.t self)
          (cons (.v self)
                nil)))

  clojure.lang.Associative
  (entryAt [self key]
    (.entryAt (.v self) key))
  (assoc [self k v]
    (ATaggedVal. (.t self)
                 (.assoc (.v self) k v)))

  clojure.lang.ILookup
  (valAt [self k]
    (.valAt (.v self) k))
  (valAt [self k o]
    (.valAt (.v self) k o)))

(defn tagged?
  "Predicate indicating whether the argument value is a tagged value or
  not. Returns true if and only if the argument is all of #{ITaggedVal, Indexed,
  ISeq}."
  [x]
  (and (satisfies? ITaggedVal x)
       (instance?  clojure.lang.Indexed x)
       (instance?  clojure.lang.ISeq x)))

(defn tag
  "Returns the tag on a tagged value, returning nil if the value is not tagged."
  [x]
  (when (tagged? x)
    (-tag x)))

(defn val
  "Returns the value of a tagged value, returning nil if the value is not
  tagged."
  [x]
  (when (tagged? x)
    (-val x)))

(defmacro deftag
  "Defines a tagged value constructor with a namespace qualified keyword tag,
  and a body map with keyword named members. Preconditions on members may be
  specified by the pre-map as for clojure.core/defn.

  Ex. (deftag test \"A demo variant\" [a b]
        {:pre [(number? a) (vector? b)]})"
  {:arglists '([name doc-string? attr-map? members pre-map?])}
  [vname & args]
  (let [;; Parse direct args
        [?docstring args] (take-when string? "" args)
        [?attr-map args]  (take-when map? {} args)
        [members args]    (take-when vector? nil args)
        [?pre-map args]   (take-when map? {} args)
        
        ;; FIXME inline guards are a bad habit of mine
        _                 (assert (vector? members) "Members is not a vector!")
        _                 (assert (every? symbol? members) "Members may contain only symbols!")

        ;; Build used datastructures
        kw-members        (mapv (comp keyword name) members)
        kw-tag            (keyword (name (ns-name *ns*))
                                   (name vname))
        ?attr-map         (assoc ?attr-map
                                 :variants/tag true
                                 :tag/members  kw-members
                                 :tag/tag      kw-tag)]
    `(do (defn ~vname ~?docstring ~?attr-map ~members
           ~?pre-map
           [~kw-tag (hash-map ~@(interleave kw-members members))])
         (defn ~(symbol (str (name vname) "?"))
           ([x#]
            (and (vector? x#)
                 (= 2 (count x#))
                 (= ~kw-tag (first x#))
                 (or (map? (second x#))
                     (nil? (second x#))))))
         nil)))

(defmacro defvariant
  "Defines a function over an open variant and a predicate returning true if the
  function is implemented for a given tagged value.

  Ex. => (defvariant foo)
      nil
      => (deftag a [a b c])
      nil
      => (foo (a 1 2 3))
      ;; No Such Method exception
      => (foo? (a 1 2 3))
      false"
  [variant-name]
  (let [pred-name    (symbol (str (name variant-name) "?"))
        default-case (keyword (name (gensym)))]
    `(do (defmulti ~pred-name first
           :default ~default-case)
         (defmulti ~variant-name first)
         (alter-meta! (var ~variant-name)
                      merge {:variants/variant true
                             :variant/predicate (quote ~(symbol (name (ns-name *ns*))
                                                                (name pred-name)))})
         (defmethod ~pred-name ~default-case [& _#] false)
         nil)))

(defmacro extend-variant
  "Extends a previously defined function over a variant adding a new method for
  the given tag to its body and extending its predicate to indicate that the tag
  for which support was just added is an element of the set of tags for which
  there are dispatch values.

  Ex. => (deftag a [a])
      nil
      => (defvariant aable)
      nil
      => (extend-variant aable a [{:a a-val}] (inc a-val))
      nil
      => (aable? (a 1))
      true
      => (aable (a 1))
      2"
  [name tag args & body]
  (let [variant-var  (resolve name)
        variant-meta (meta variant-var)
        _            (assert (get variant-meta :variants/variant) "Tried to extend a non-variant!")
        variant-pred (:variant/predicate variant-meta)
        _            (assert variant-pred "Tried to extend a variant with no predicate!")
        _            (resolve variant-pred)
        tag-var      (resolve tag)
        tag-meta     (meta tag-var)
        _            (assert (:variants/tag tag-meta) "Tried to extend a variant on an unknown var!")
        tag-keyword  (:tag/tag tag-meta)
        _            (assert (keyword? tag-keyword) "Could not resolve the keyword for the given tag!")]
    `(do (defmethod ~name         ~tag-keyword [[_# ~@args]] ~@body)
         (defmethod ~variant-pred ~tag-keyword [& _#] true)
         nil)))

(comment
  (do (deftag foo [a])
      (deftag qux [b])
      (defvariant f-of-foo)
      (extend-variant f-of-foo foo [{:a a}] (println a))
      (f-of-foo? (foo 1)) ;; -> true
      (f-of-foo  (foo 1)) ;; prints 1
      (f-of-foo? (bar 1)) ;; -> false
      ))
