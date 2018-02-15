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

;; Dynamic configuration stuff
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

(def print-thread-factory
  (fn [atom writer]
    {:pre [(instance? clojure.lang.Atom atom)
           (instance? java.io.PrintWriter writer)]}
    (Thread.
     (fn []
       (loop []
         (let [buff @atom
               _    (swap! atom #(drop-last (count buff) %))]
           (if buff
             (doseq [e (reverse buff)]
               (.println writer e))
             (Thread/yield))
           (recur)))))))

(defn ->Printer [writer]
  (let [a (atom nil)]
    [:printer
     {:thread (print-thread-factory a writer)
      :buffer a}]))

(defn printer? [x]
  (and (vector? x)
       (= :printer (first x))
       (map? (second x))
       (let [m (second x)]
         (and (instance? java.lang.Thread (:thread m))
              (instance? clojure.lang.Atom (:buffer m))))))

(defn start! [printer]
  {:pre [(printer? printer)]}
  (.start (:thread (second printer))))

(defn stop! [printer]
  {:pre [(printer? printer)]}
  (.stop (:thread (second printer))))

(defn p! [printer & xs]
  {:pre [(printer? printer)]}
  (let [t (Thread/currentThread)
        s (format  "%s %s" t (apply print-str xs))]
    (swap! (:buffer (second printer)) conj s)))
