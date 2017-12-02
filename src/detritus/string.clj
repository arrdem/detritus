(ns detritus.string)

(defn remove-from-end [s end]
  (when s
    (if (.endsWith s end)
      (.substring s 0 (- (count s)
                         (count end)))
      s)))

(defn re-find-last [re s]
  (let [m (re-matcher re s)]
    (loop [p nil]
      (if-let [x (re-find m)]
        (recur x)
        p))))

(defn re-fn
  "Construct a regular expression from string.  Calling a regular
  expression with no arguments returns a Pattern.  Calling a regular
  expression with a string argument returns nil if no matches,
  otherwise the equivalent of (re-seq re string)."
  [string]
  (let [pp (re-pattern string)]
    (fn re
      ([] pp)
      ([s] (let [groups (re-seq pp s)]
             (if (first groups)
               groups
               nil))))))
