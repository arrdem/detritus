(ns detritus.var
  (:require [detritus.namespace :refer [ns->sym]]))

(defn var->ns [v]
  {:pre [(var? v)]}
  (-> v (.ns) ns->sym))

(defn var->sym [v]
  {:pre [(var? v)]}
  (-> v (.sym)))
