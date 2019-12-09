(ns aoc19.5
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string]])
  (:require [aoc19.intcode :refer [run-program]]))

(def data-file (load-file-as-string "day5.txt"))                         ; the url of the resource file

(def core-program (vec (map #(Integer/parseInt %) (string/split data-file #","))))

(def part1 (println (get (run-program core-program 0 [1] nil) :output)))

(def part2 (println (get (run-program core-program 0 [5] nil) :output)))

(defn -main
  [& args]
  (do part1 part2))