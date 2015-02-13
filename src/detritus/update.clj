(ns detritus.update
  "A collection of helper functions useful for updating and
  conditionally updating datastructures.")


;; Mapping update operations
;;--------------------------------------------------------------------

(defn map-vals
  "λ {A → B} → (λ B → more* → C) → more* → {A → C}

  Computes a new map from m preserving the keys of m, but mapping the
  keys of m to (apply f (get m k) args)."
  [m f & args]
  (->> (for [[k v] m]
       [k (apply f v args)])
     (into {})))

(defn map-entry [x y]
  [x y])

(defn map-keys
  "Create a new map from m by calling function f on each key to get a
  new key."
  [m f & args]
  (when m
    (into {}
          (for [[k v] m]
            (map-entry (apply f k args) v)))))

(defn map-vals-with-keys
  "Create a new map from m by calling function f, with two
  arguments (the key and value) to get a new value."
  [m f & args]
  (when m
    (into {}
          (for [[k v] m]
            (map-entry k (apply f k v args))))))

(defn map-keys-and-vals
  "Create a new map from m by calling function f on each key & each
  value to get a new key & value"
  [m f & args]
  (when m
    (into {}
          (for [[k v] m]
            (map-entry (apply f k args) (apply f v args))))))

(defn fix
  "λ (fn T → T) → T → T

  Eagerly computes the fixed point combinator of the input function
  and value. As this computation is eager, it will terminate only when
  a fixed point is reached which may be never."
  [f dat]
  (let [dat' (f dat)]
    (if (= dat dat')
      dat
      (recur f dat'))))

(defn update
  "λ {A → B} → A → (λ B → args* → C) → args* → {A → C}

  Updates a key in the map by applying f to the value at that key more
  arguments, returning the resulting map."
  [map key f & args]
  (assoc map key
         (apply f (get map key) args)))

;; Conditional update operations
;;--------------------------------------------------------------------

(defn ->when-update
  "Function of a value, a predicate, an updater and optional
  varargs. If the predicate is true of the value, returns (apply f x
  args), otherwise returns x.

  Example:
    (-> 1 (->when-update #(<= 0 %) inc))"
  [x pred f & args]
  (if (pred x)
    (apply f x args)
    x))

(defn ->>when-update
  "Function of a predicate, an updater, optional varargs and a
  value. If the predicate is true of the value, returns (apply f x
  args), otherwise returns x.

  Example:
    (->> 1 (->>when-update #(<= 0 %) inc))"
  [pred f & args]
  (let [x    (last args)
        args (butlast args)]
    (if (pred x)
      (apply f x args)
      x)))

(defn take-when
  "Helper useful for parsing regular function argument seqeunces. If a predicate
  is satisfied, returns a pair [(first col), (rest col)] otherwise returns the pair
  [empty, col].

  Ex. (let [[?docstring args] (take-when string? \"\" args)
            [?attr-map  args] (take-when map? {} args)]
        ..)"
  [f empty col]
  (let [[x & rest] col]
    (if (f x) [x rest] [empty col])))

;; Stolen from zmaril (with permission)

(ns update-in*)

;; Got tired of falling back to a map when I needed to multiple things
;; update things the same way in nested data structures. update-in* is
;; like update-in but expanded to allow for iteration over all the
;; elements in a collection. Inspired by the pull api in datomic.

(defn keys* [m]
  ;; Only associative things can be assoc'd on and have keys
  (when (associative? m)
    ;; FIXME: this is closed dispatch, but c.l.Associative doesn't
    ;; provide some sort of key sequence operation much to my
    ;; surprise and as noted clojure.core/keys isn't complete over
    ;; associative things.
    ;;
    ;; Vector is the only Associative defined in core which doesn't
    ;; comply with the implicit kv seq contract depended upon by
    ;; clojure.lang.RT/keys
    (if (vector? m)
      (range 0 (count m))
      (keys m))))

(defn update-in*
  ([m [k & ks] f & args]
   (if-not (associative? m)
     ;; Don't even try
     m

     ;; Maps, vectors etc.
     (cond
       (and (not (nil? ks))
            (= k :all))
       ,,(if (vector? m)
           ;; special case vectors because transient vectors aren't associative
           (->> m
                keys*
                (map (fn [k] (apply update-in* (get m k) ks f args)))
                (into []))

           (as-> (transient (empty m)) t
             (reduce (fn [m k] (assoc! m k (apply update-in* (get m k) ks f args)))
                     t (keys* m))
             (persistent! t)))

       (and (nil? ks)
            (= k :all))
       ,,(if (vector? m)
           ;; special case vectors because transient vectors aren't associative
           (->> m
                keys*
                (map (fn [k] (apply f (get m k) args)))
                (into []))

           ;; general case of transient maps
           (as-> (transient (empty m)) t
             (reduce (fn [t k] (assoc t k (apply f (get m k) args)))
                     t (keys* m))
             (persistent! t)))

       (not (nil? ks))
       ,,(assoc m k (apply update-in* (get m k) ks f args))

       (nil? ks)
       ,,(assoc m k (apply f (get m k) args))))))

(assert
 (= (update-in* [1 2] [:all] inc)
    [2 3]))

(assert
 (= (update-in* [[1 2] [3 4]] [:all :all] inc)
    [[2 3] [4 5]]))

(assert
 (= (update-in* {:a [1 2]} [:a :all] inc)
    {:a [2 3]}))

(assert
 (= (update-in* {:a [{:id 1} {:id 2}]} [:a :all :id] inc)
    {:a [{:id 2} {:id 3}]}))

(let [l (list 1 2 3)]
  (assert
   (= (update-in* l [:all] inc)
      l)))

(let [n 15]
  (assert
   (= (update-in* n [:all] inc)
      n)))

(defn breakout
  ([vs] (breakout -1 vs))
  ([n vs]
   (->> (group-by first vs)
        (map (fn [[k v]]
               [k (let [rst (distinct (map rest v))]
                    (if (or (= 1 (count (first rst)))
                            (zero? n))
                      (apply concat rst)
                      (breakout (dec n) rst)))]))
        (into {}))))


(defn prune [m]
  (->> m
       (map
        (fn [[k vs]]
          (if (seq? vs)
            (if (= 1 (count vs))
              [k (first vs)]
              [k vs])
            [k (prune vs)])))
       (into {})))

(prune (breakout
        [[1 1 1]
         [1 2 1]
         [2 1 1]
         [2 3 1]]))
