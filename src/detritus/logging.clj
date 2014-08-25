(ns detritus.logging
  "This namespace provides a trivial ")

;; FATAL    : 5
;; ERROR    : 4
;; WARN     : 3
;; MESSAGE  : 2
;; INFO     : 1
;; DEBUG    : 0

(def ^:dynamic *log-level*  -1)
(def ^:dynamic *log-width*  80)
(def ^:dynamic *log-stream* *err*)

(defn debug [& args]
  (when (>= 0 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (println "[DEBUG   ]" (apply str args)))))

(defn info [& args]
  (when (>= 1 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (println "[INFO    ]" (apply str args)))))

(defn message [args]
  (when (>= 2 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (println "[MESSAGE ]" (apply str args)))))

(defn warn [args]
  (when (>= 3 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (println "[WARN    ]" (apply str args)))))

(defn error [args]
  (when (>= 4 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (println "[ERROR   ]" (apply str args)))))

(defn fatal [args]
  (when (>= 5 *log-level*)
    (with-bindings {#'clojure.core/*out* *log-stream*}
      (println "[FATAL   ]" (apply str args)))))
