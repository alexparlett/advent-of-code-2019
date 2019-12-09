(ns aoc19.7
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [clojure.math.combinatorics :as combo])
  (:require [aoc19.core :refer [load-file-as-string]])
  (:require [aoc19.intcode :refer [run-program]]))

(def data-file (load-file-as-string "day7.txt"))                         ; the url of the resource file

(def core-program (vec (map #(Integer/parseInt %) (string/split data-file #","))))

(def phase-permutations (combo/permutations [0 1 2 3 4]))

(defn amp
  [program phase input]
  (get (run-program program 0 [phase input] nil) :output))

(defn run-amplifiers
  [program]
  (for [phases phase-permutations] (amp program (nth phases 4) (amp program (nth phases 3) (amp program (nth phases 2) (amp program (nth phases 1) (amp program (nth phases 0) 0)))))))

(def part1 (println (apply max (run-amplifiers core-program))))

(def part2 (println "Omg"))

(defn -main
  [& args]
  (do part1 part2))