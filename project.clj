(defproject aoc19 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/algo.generic "0.1.3"]
                 [org.clojure/core.async "0.6.532"]
                 [medley "1.1.0"]
                 [com.rpl/specter "1.1.3"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :jvm-opts ["-XstartOnFirstThread"])
