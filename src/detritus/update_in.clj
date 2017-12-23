(ns detritus.update-in
  "Stolen from zmaril (with permission)")

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
