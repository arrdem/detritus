(ns detritus.printer)

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
