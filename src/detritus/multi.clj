(ns detritus.multi
  "Tools for working with multimethods."
  {:authors ["Reid \"arrdem\" McKenzie <me@arrdem.com>"]
   :license "https://www.eclipse.org/legal/epl-v10.html"}
  (:refer-clojure :exclude [type]))

(defn type
  "Dispatch function for switching on, in order
   - The declared `:type` of an object
   - The `:type` metadata
   - The Java type

  The logical extension of Clojure's `#'type` operator to the `:type` pattern."
  [x]
  (:type x (clojure.core/type x)))

(defn make-prefix-dispatch
  "Make a function which dispatches on the `#'type` tuple of the first N arguments, silently absorbing
  the rest."
  (fn [n]
    (fn [& xs]
      (mapv type (take n xs)))))

(defn derive!
  "Although hierarchies are mostly used by indirection as vars (`defmulti` supports the `:hierarchy`
  argument for using hierarchies other than the default global one), and although `(derive child
  parent)` implicitly side-effects the global hierarchy, `(derive h child parent)` is a pure
  function of a hierarchy as a map structure which doesn't behave nicely when given a var.

  Wrap it up with an alter-var-root so it makes sense."
  [h child parent]
  {:pre [(var? h)]}
  (alter-var-root h derive child parent))
