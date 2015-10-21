(ns alanlcode.util.print
    "Utilities for output formatting and printing."
  (:require [clojure.pprint :as pprint]))

(defn pprint-str [x]
  (with-out-str
    (pprint/pprint x)))

