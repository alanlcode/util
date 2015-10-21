(ns alanlcode.util.time
  "Time-related utilities."
  (:require [clj-time
             [coerce :as coerce]
             [core :as time]]
            [clojail.core :refer (thunk-timeout)]))

(defn gen-timestamp []
  (coerce/to-long (time/now)))

(defmacro with-timeout [ms & body]
  `(thunk-timeout (fn [] ~@body) ~ms))

(defmacro timed
  "Return how long the underlying forms took to execute in nanoseconds."
  ([& forms]
   `(let [s# (System/nanoTime)]
      ~@forms
      (- (System/nanoTime) s#))))
