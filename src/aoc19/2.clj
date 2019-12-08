(ns aoc19.2
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string replace-value]]))

(def data-file (load-file-as-string "day2.txt"))                         ; the url of the resource file

(def core-program (vec (map #(Integer/parseInt %) (string/split data-file #","))))

(defn get-value
  [index arr]
  (nth arr (nth arr index)))

(defn code-add
  [program index]
  (replace-value (nth program (+ 3 index)) (+ (get-value (+ 1 index) program) (get-value (+ 2 index) program)) program))

(defn code-mul
  [program index]
  (replace-value (nth program (+ 3 index)) (* (get-value (+ 1 index) program) (get-value (+ 2 index) program)) program))

(defn apply-code
  [program index]
  (case (nth program index)
    1 (code-add program index)
    2 (code-mul program index)
    99 :done))

(defn run-program
  [program pointer]
  (let [result (apply-code program pointer)]
    (if (= result :done)
      program
      (recur result (+ 4 pointer)))))

(defn restore-program
  [program noun verb]
  (replace-value 2 verb (replace-value 1 noun program)))

(defn find-input-for-output
  [program output]
  (for [i (range 99) j (range 99) :when (= output (first (run-program (restore-program program i j) 0)))] (+ (* 100 i) j)))

(def part1 (first (run-program (restore-program core-program 12 2) 0)))

(def part2 (first (find-input-for-output core-program 19690720)))

(defn -main
  [& args]
  (println [part1 part2]))