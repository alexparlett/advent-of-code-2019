(ns aoc19.9
  (:gen-class)
  (:require [aoc19.intcode :refer [run-program load-program]]))

(def core-program (load-program "day9.txt"))

(def part1 (run-program core-program 0 [1] nil 0))

(def part2 (run-program core-program 0 [2] nil 0))

(defn -main
  [& args]
  (do part1 part2))