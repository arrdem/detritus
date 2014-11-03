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
  [log-level message prefix args]
  (when (>= log-level *log-level*)
    (->> args
       (map str)
       (map text/lines)
       (apply concat)
       (map (partial (fn format-line [spacing line]
                 (if (>= (+ (count prefix) (count line)) *log-width*)
                   (->> line
                      (text/wrap-lines (- *log-width* 7 (count prefix)))
                      (map (partial format-line "     : "))
                      (apply str))
                   (str prefix spacing line "\n")))
               "     "))
       (cons (str prefix " [" message "]\n"))
       (apply str)
       print)))


(defmacro defdebug [symbol level prefix]
  `(defmacro ~symbol [~'& args#]
     (let [meta# (meta ~'&form)]
       (list 'with-bindings '{#'clojure.core/*out* *log-stream*}
             (list 'do-print
                   ~level
                   (str "ns " (name (.name *ns*)) " | file " *file* ":" (:line meta#))
                   ~prefix
                   (vec args#))))))


(defdebug debug   0 "[DEBUG   ]")
(defdebug info    1 "[INFO    ]")
(defdebug message 2 "[MESSAGE ]")
(defdebug warn    3 "[WARN    ]")
(defdebug error   4 "[ERROR   ]")
(defdebug fatal   5 "[FATAL   ]")
