(ns alanlcode.util.random
  (:refer-clojure :exclude [long])
  (:import java.security.SecureRandom))

(defn ^SecureRandom pseudo-rng
  "Return a new pseudo-random number generator."
  []
  (SecureRandom/getInstance "SHA1PRNG"))

(defn ^SecureRandom strong-rng
  "Returns a new strong random number generator."
  []
  (SecureRandom/getInstanceStrong))

(defn ^Long long
  [^SecureRandom rng]
  (.nextLong rng))

(defn ^String hex-string
  [^SecureRandom rng]
  (format "%x" (long rng)))
