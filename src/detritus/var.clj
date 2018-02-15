(ns detritus.var)

;; FIXME: remove when CLJ-1488 drops
;; http://dev.clojure.org/jira/browse/CLJ-1488

(defn var->ns [v]
  {:pre [(var? v)]}
  (some-> v (.ns) ns-name))

(defn var->sym [v]
  {:pre [(var? v)]}
  (some-> v (.sym)))

(defn macro? [v]
  {:pre [(var? v)]}
  (:macro (meta v)))
