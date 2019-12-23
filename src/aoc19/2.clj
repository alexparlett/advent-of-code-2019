(ns aoc19.2
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string replace-value]])
  (:require [aoc19.intcode :refer [run-program]])
  (:require [clojure.core.async :refer [>!! <!!]]))

(def data-file (load-file-as-string "day2.txt"))

(def core-program (vec (map #(Integer/parseInt %) (string/split data-file #","))))

(defn restore-program
  [program noun verb]
  (replace-value 2 verb (replace-value 1 noun program)))

(defn find-input-for-output
  [program target]
  (for [i (range 99) j (range 99)
        :when (let [[input output] (run-program (restore-program program i j))] (= target (first (get (<!! output) :program))))]
    (+ (* 100 i) j)))

(def part1 (let [[input output state] (run-program (restore-program core-program 12 2))]
             (first (get (<!! output) :program))))

(def part2 (last (find-input-for-output core-program 19690720)))

(defn -main
  [& args]
  (println [part1 part2]))