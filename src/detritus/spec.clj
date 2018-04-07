(ns detritus.spec
  "Tools for hammering out specs rapidly."
  {:authors ["Reid \"arrdem\" McKenzie <me@arrdem.com>"],
   :license "https://www.eclipse.org/legal/epl-v10.html"}
  (:refer-clojure :exclude [name])
  (:require [clojure.spec.alpha :as s]
            [detritus :refer [name]]))

(defn- spec-in-tag [tag spec]
  (keyword (str (name *ns*) "." (name tag))
           (name spec)))

(s/def ::field-spec
  (s/cat :field symbol?
         :turnstyle #{:- :?}
         :spec (complement #{:- :?})))

(s/def ::fields
  (s/and vector?
         (s/* ::field-spec)))

(s/fdef deftag
        :args (s/cat :tag-name symbol?
                     :fields ::fields))

(defmacro deftag
  "Somewhat comparable to `#'guten-tag.core/deftag`, except that instead
  of generating a custom tagged value type, it just generates a
  `clojure.spec(.alpha)` spec for the `:type` tagged map form of the union."
  [tag-name fields]
  (let [fields+specs (s/conform ::fields fields)]
    `(do ~@(for [{:keys [field spec]} fields+specs]
             `(s/def ~(spec-in-tag tag-name field) ~spec))
         (s/def ~(spec-in-tag tag-name :type) #{~(keyword (name *ns*) (name tag-name))})
         (s/def ~(keyword (name *ns*) (name tag-name))
           (s/keys :req-un [~(spec-in-tag tag-name :type)
                            ~@(keep #(when (= :- (:turnstyle %))
                                       (spec-in-tag tag-name (:field %)))
                                    fields+specs)]
                   :opt-un ~(vec (keep #(when (= :? (:turnstyle %))
                                          (spec-in-tag tag-name (:field %)))
                                       fields+specs)))))))
