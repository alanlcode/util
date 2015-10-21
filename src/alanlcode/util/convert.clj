(ns alanlcode.util.convert
    "Utilities for converting between data formats.")

(defn int-or-nil
  "Convert a String in base 10 format to an Integer if possible, otherwise return nil"
  [x]
  (when (and x (string? x))
    (try
      (Integer/parseInt x)
      (catch NumberFormatException _
        nil))))
