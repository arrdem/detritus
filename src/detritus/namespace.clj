(ns detritus.namespace)

;; FIXME: remove when CLJ-1488 drops
;; http://dev.clojure.org/jira/browse/CLJ-1488

(defn ns->sym [ns]
  {:pre [(instance? clojure.lang.Namespace ns)]}
  (.name ns))
