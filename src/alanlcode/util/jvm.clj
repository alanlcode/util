(ns alanlcode.util.jvm
  "Java VM utilities."
  (:require [clojure.tools.logging :as log]))

;; http://stuartsierra.com/2015/05/27/clojure-uncaught-exceptions
(defn set-default-exception-handler! []
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [_ thread ex]
       (log/error ex "Uncaught exception on" (.getName thread))))))

