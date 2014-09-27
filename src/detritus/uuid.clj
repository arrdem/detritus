(ns detritus.uuid
  (:import java.util.UUID))

(defn ->uuid
  ([n m]
     (UUID. (long n) (long m))))

(defn ->random-uuid []
  (UUID/randomUUID))
