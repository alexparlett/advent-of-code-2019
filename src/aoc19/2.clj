(ns aoc19.2
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string replace-value]])
  (:require [aoc19.intcode :refer [run-program]]))

(def data-file (load-file-as-string "day2.txt"))                         ; the url of the resource file

(def core-program (vec (map #(Integer/parseInt %) (string/split data-file #","))))

(defn restore-program
  [program noun verb]
  (replace-value 2 verb (replace-value 1 noun program)))

(defn find-input-for-output
  [program output]
  (for [i (range 99) j (range 99) :when (= output (first (get (run-program (restore-program program i j) 0 [0] nil 0) :program)))] (+ (* 100 i) j)))

(def part1 (first (get (run-program (restore-program core-program 12 2) 0 [0] nil 0) :program)))

(def part2 (first (find-input-for-output core-program 19690720)))

(defn -main
  [& args]
  (println [part1 part2]))