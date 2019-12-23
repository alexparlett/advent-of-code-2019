(ns aoc19.9
  (:gen-class)
  (:require [aoc19.intcode :refer [run-program load-program]])
  (:require [clojure.core.async :refer [>!! <!!]]))

(def core-program (load-program "day9.txt"))

(def part1 (println (let [[input output] (run-program core-program)]
                      (do (>!! input 1) (<!! output)))))

(def part2 (println (let [[input output] (run-program core-program)]
                      (do (>!! input 2) (<!! output)))))

(defn -main
  [& args]
  (do part1 part2))