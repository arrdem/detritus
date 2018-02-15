(ns detritus.sequences)

(defn indexed [seq]
  (map vector (range) seq))

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

(defn lazy-group-by-p
  "group-by specialized to predicates.

  Returns a pair of lazy sequences `[ts fs]`."
  [pred coll]
  ((juxt (partial filter pred)
         (partial filter (complement pred)))
   coll))

(defn group-by-p
  "group-by specialized to predicates.

  Returns a pair of eager sequences `[ts fs]`."
  [pred coll]
  (loop [[head & tail :as coll] coll
         t                      []
         f                      []]
    (if (empty? coll)
      [t f]
      (let [[t* f*] (if (pred head)
                      [(conj t head) f]
                      [t (conj f head)])] 
        (recur tail t* f*)))))

(def separate
  "The old better name for `#'group-by-p` from contrib."
  #'group-by-p)

(defn zip
  "Zips the argument collections together, returning striped sequences
  of the nth element from each sequence.

  ```
  user> (zip [1 2 3]
             [4 5 6])
  ([1 4]
   [2 5]
   [3 6])
  ```"
  [& colls]
  (map vector colls))
