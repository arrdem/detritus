(ns detritus.logging
  "This namespace provides a trivial logging engine which uses
  detritus.text for minimal formatting.

  Note that the API here is likely very volatile and subject to
  change due to being bare bones to the point of trivial."
  (:require [detritus.text :as text]))


;; Logging levels
;;--------------------------------------------------------------------
;; FATAL    : 5
;; ERROR    : 4
;; WARN     : 3
;; MESSAGE  : 2
;; INFO     : 1
;; DEBUG    : 0


;; Dynamic configuration stuff [HACK]
;;--------------------------------------------------------------------

(def ^:dynamic *log-level*  -1)
(def ^:dynamic *log-width*  80)
(def ^:dynamic *log-stream* *err*)


;; Logging API
;;--------------------------------------------------------------------

(defn do-print
  [prefix args]
  (->> args
     (apply str)
     (text/wrap-lines 70)
     (map (fn [line] (str prefix line "\n")))
     (apply str)
     println))


(defn debug [& args]
  (when (>= 0 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (do-print "[DEBUG   ]" args))))


(defn info [& args]
  (when (>= 1 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (do-print "[INFO    ]" args))))


(defn message [& args]
  (when (>= 2 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (do-print "[MESSAGE ]" args))))


(defn warn [& args]
  (when (>= 3 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (do-print "[WARN    ]" args))))


(defn error [& args]
  (when (>= 4 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (do-print "[ERROR   ]" args))))


(defn fatal [args]
  (when (>= 5 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (do-print "[FATAL   ]" args))))
