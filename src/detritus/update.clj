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
