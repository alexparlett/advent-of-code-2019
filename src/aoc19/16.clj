(ns aoc19.16
  (:gen-class)
  (:require [aoc19.core :refer [load-file-as-string digits]]))

(def data-file (load-file-as-string "day16.txt"))

(defn create-pattern
  [iteration]
  (rest (cycle (mapcat #(repeat (inc iteration) %) [0 1 0 -1]))))

(def patterns
  (map create-pattern (range)))

(defn apply-pattern
  [signal pattern]
  (Math/abs (rem (reduce + (apply map * (list signal pattern))) 10)))

(defn calculate-phase
  [signal]
  (take (count signal) (map (partial apply-pattern signal) patterns)))

(def signal-offset (Integer/parseInt (subs data-file 0 7)))

(defn partial-sum
  [signal]
  (reductions (fn [last current] (mod (+ current last) 10)) signal))

(defn transform
  [signal]
  (vec (reverse (nth (iterate partial-sum (reverse signal)) 100))))

(def part1 (let [signal (digits data-file)]
             (println (take 8 (nth (iterate calculate-phase signal) 100)))))

(def part2 (let [signal (flatten (repeat 10000 (digits data-file)))]
             (println (subvec (transform signal) signal-offset (+ signal-offset 8)))))

(defn -main
  [& args]
  (do part1 part2))