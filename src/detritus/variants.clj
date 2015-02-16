(ns detritus.variants
  (:refer-clojure :exclude [val])
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
  "Generates a tagged value smart constructor `->T` where T is the name of the
  tag. The constructor accepts an arglist identical to the list of members and
  returns an instance of ATaggedVal with the defined tag and a map constructed
  from keywordized member names to given values. Preconditions on members may be
  specified by the pre-map as for clojure.core/defn.

  Also generates a predicate `T?` where T is the name of the tag. The predicate
  will return true if and only if the argument value is `tagged?`, an instance
  of the given tag type, and satisfies the `:pre` conditions in the tag
  constructor if any.

  Ex. => (deftag test \"A demo variant\" [a b]
           {:pre [(number? a) (vector? b)]})
      nil
      => (->test 1 [1])
      (:user/test {:a 1 :b [1]})
      => (test? (->test 1 [1]))
      true
      => (tagged? (->test 1 [1]))
      true
      => (tag (->test 1 [1]))
      :user/test
      => (val (->test 1 [1]))
      {:a 1 b [1]}"
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
        ?attr-map         (merge ?attr-map
                                 ?pre-map
                                 (when ?docstring
                                   {:doc ?docstring}))]
    `(do (defn ~(symbol (str "->" (name vname)))
           ~(str "Generated constructor for the " vname " type.")
           ~members
           ~?pre-map
           (->ATaggedVal ~kw-tag (hash-map ~@(interleave kw-members members))))
         (def ~vname
           ~?docstring
           (detritus.variants/->TagDescriptor
            ,,(quote ~?attr-map)
            ,,(quote ~kw-members)
            ,,(quote ~kw-tag)))
         (defn ~(symbol (str (name vname) "?"))
           ~(str "Generated predicate for the " vname " type.")
           ([x#]
            (and (tagged? x#)
                 (= ~kw-tag (tag x#))
                 (or (map? (val x#))
                     (nil? (val x#)))
                 (let [[_ {:keys ~members}] x#]
                   (and ~@(:pre ?pre-map))))))
         nil)))

(deftag TagDescriptor [attrs members tag])

;; FIXME: def closed variant?
;; FIXME: def open variant?
;; FIXME: variant dispatch?
