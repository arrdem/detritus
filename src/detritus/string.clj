(ns detritus.string)

(defn remove-from-end
  "Removes a strictly trailing substring."
  [s end]
  (when s
    (if (.endsWith s end)
      (.substring s 0 (- (count s)
                         (count end)))
      s)))

(defn re-find-last
  "Returns the last match of `re` in `s` if any."
  [re s]
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

(defn indicies-of [^String text ^String s]
  (loop [acc  []
         last (.indexOf text s)]
    (if-not (= -1 last)
      (recur (conj acc last) (.indexOf text s (inc last)))
      acc)))

(defn prefixes-by-including
  "Returns all prefixes of the `text` which are delimited by an
  occurrence of the provided string `s`.

  Includes the delimiter in all prefixes."
  [^String text ^String s]
  (for [idx (reverse (indicies-of text s))]
    (.substring text 0 (+ idx (count s)))))
