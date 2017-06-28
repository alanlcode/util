(ns alanlcode.util.package
  "Package management related utilities"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn lein-project-map
  "Return Leiningen project map if it exists, nil otherwise"
  []
  (when-let [[_ project-name project-version & remaining]
             (some-> (or (io/file "project.clj") (io/resource "project.clj")) slurp edn/read-string)]
    (apply hash-map
           :project project-name
           :version project-version
           remaining)))

(defn version
  "Return version string from Leiningen project map"
  []
  (:version (lein-project-map)))
