(ns detritus.macros)

(defmacro -<> [val & forms]
  `(as-> ~val ~'<> ~@forms))
