(ns detritus.trie
  "A quick and dirty trie structure.")

(defn trie? [s]
  (and (map? s)
       (= (:type s) ::trie)))

(defn trie-assoc [t k v]
  (println t k v)
  (if (or (nil? t)
          (and (map? t)
               (empty? t)))
    (recur {:type ::trie} k v)
    (let [k'      k
          [k & r] k]
      (cond (and (not (empty? r))
                 (trie? t))
            (update t k trie-assoc r v)

            (and (empty? r)
                 (trie? t))
            (assoc t k v)

            (not (trie? t))
            (recur {:type    ::trie
                    :default t}
                   k' v)))))
