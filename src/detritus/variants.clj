(ns detritus.variants
  (:require [detritus.update :refer [take-when]]))

(defmacro defvariant
  "Defines a variant constructor with a namespace qualified tag, and keyword
  named members. Preconditions on members may be specified by the pre-map.

  Ex. (defvariant test \"A demo variant\" [a b]
        {:pre [(number? a) (vector? b)]})"
  {:arglists '([name doc-string? attr-map? members pre-map?])}
  [vname & args]
  (let [;; Parse direct args
        [?docstring args]    (take-when string? "" args)
        [?attr-map args]     (take-when map? {} args)
        [members args]       (take-when vector? nil args)
        [?pre-map args] (take-when map? {} args)
        
        ;; FIXME inline guards are a bad habit of mine
        _                    (assert (vector? members) "Members is not a vector!")
        _                    (assert (every? symbol? members) "Members may contain only symbols!")

        ;; Build used datastructures
        kw-members           (map (comp keyword name) members)
        kw-tag               (keyword (name (ns-name *ns*))
                                      (name vname))
        ?attr-map            (assoc ?attr-map
                                    :variant true
                                    :variant/members kw-members
                                    :variant/tag kw-tag)]
  `(defn ~vname ~?docstring ~?attr-map ~members
     ~?pre-map
     [~kw-tag (hash-map ~@(interleave kw-members members))])))
