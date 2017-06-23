(defproject alanlcode/util "0.1.0-SNAPSHOT"
  :description "Utility belt"
  :url "http://github.com/alanlcode/util"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.145"]
                 [org.clojure/tools.logging "0.3.1"]
                 [clojail "1.0.6"]
                 [clj-time "0.11.0"]]
  :profiles {:dev {:dependencies [[expectations "2.0.9"]]
                   :plugins [[lein-expectations "0.0.8"]
                             [lein-autoexpect "1.7.0"]
                             [codox "0.8.10"]]
                   :codox {:src-dir-uri "https://github.com/alanlcode/util/blob/master/"
                           :src-linenum-anchor-prefix "L"}}})
