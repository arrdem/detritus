(ns detritus.macros)

(defmacro -<> [val & forms]
  `(as-> ~val ~'<> ~@forms))

(defmacro re-cond
  "cond specialized for dealing with regexes and binding the result of
  a regex match. Takes a leading form expected to be a String and a
  series of pattern/binding/form guards. If the input String matches a
  pattern, that match is destructured with the given binding(s) and
  the form is evaluated.

  Handy for dealing with operations on one of several regex matches."

  {:style/indent 1}
  [text & forms]
  (let [[pattern binding form & forms] forms]
    (if (and pattern binding form)
      `(if-let [~binding (re-find ~pattern ~text)]
         ~form
         (re-cond ~text ~@forms))
      (if (= :else pattern)
        binding
        text))))

(defn -? [named]
  (str (name named) "?"))

(defmacro defenum
  "Defines a enum to be a set of keywords either already qualified or
  qualified with the name of the enum.

  Takes as arguments a name for the enum, and a sequence of
  expressions either being sets of potentially qualified keywords or
  individual keywords. Generates an unqualified alias def and
  predicate for each keyword, and a predicate for the enum as a
  whole.

  Ex.
  (defenum foo
    :bar
    :baz)

  -> foo? bar, bar?, baz, baz?"
  
  {:format/indent 1}
  [ename & kws]
  (let [kws (->> kws
                 (mapcat (fn [o]
                           (let [o (eval o)]
                             (if-not (set? o)
                               [o] o))))
                 set)]
    ;; Check inputs
    ;;----------------------------------------
    (assert (symbol? ename)
            "defenum requires a symbol for the enum name!")
    
    (doseq [kw kws]
      (assert (keyword? kw)
              "defenum only accepts keyword arguments!"))

    ;; Generate code
    ;;----------------------------------------
    (let [kws (map (fn [kw]
                     (keyword (or (namespace kw)
                                  (name ename))
                              (name kw)))
                   kws)]
      `(do ~@(for [kw kws]
               `(do (def ~(symbol (name kw)) ~kw)
                    (defn ~(symbol (-? kw)) [x#]
                      (= x# ~kw))))
           (def ~(symbol (-? ename)) ~(set kws))))))
