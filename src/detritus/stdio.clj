(ns detritus.stdio)

(defn eprintf
  "`#'printf` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply printf args)))

(defn eprint
  "`#'print` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply print args)))

(defn eprintln
  "`#'println` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn epr
  "`#'pr` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply pr args)))

(defn eprn
  "`#'prn` but to `*err*`."
  [& args]
  (binding [*out* *err*]
    (apply prn args)))
