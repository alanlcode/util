(ns alanlcode.util.checksum
  "Checksum-related utilities."
  (:import java.security.MessageDigest))

(defn sha256-string [file]
  (when-let [contents (slurp file)]
    (let [hash-instance (doto (MessageDigest/getInstance "SHA-256")
                          (.update (.getBytes contents)))]
      (apply str (map #(format "%02x" %) (.digest hash-instance))))))
