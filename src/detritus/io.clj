(ns detritus.io
  "Stuff that should be in clojure.java.io"
  (:import java.io.InputStream))

(defn ^InputStream resource-stream
  "Returns a stream for a named resource. Use the context class loader
   if no loader is specified."
  ([n]
   (resource-stream n (.getContextClassLoader (Thread/currentThread))))

  ([n ^ClassLoader loader]
   (.getResourceAsStream loader n)))
