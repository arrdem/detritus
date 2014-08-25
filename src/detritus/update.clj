(ns detritus.update
  "A collection of helper functions useful for updating and
  conditionally updating datastructures.")

(defn map-vals
  "λ {A → B} → (λ B → more* → C) → more* → {A → C}

  Computes a new map from m preserving the keys of m, but mapping the
  keys of m to (apply f (get m k) args)."
  [m f & args]
  (->> (for [[k v] m]
       [k (apply f v args)])
     (into {})))


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
