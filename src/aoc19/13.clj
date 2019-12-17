(ns aoc19.13
  (:gen-class)
  (:require [aoc19.core :refer [replace-value]]
            [aoc19.intcode :refer [run-program load-program]]
            [clojure.core.async :refer [<!! go-loop put!]]
            [clojure.string :as string]))

(def core-program (load-program "day13.txt"))

(defn move
  [pin ball paddle]
  (cond
    (or (zero? paddle) (= paddle ball)) (put! pin 0)
    (> paddle ball) (put! pin -1)
    (< paddle ball) (put! pin 1)))

(def part1  (let [[pin pout] (run-program core-program 0 0) total (atom 0)]
              (<!! (go-loop []
                     (let [x (<!! pout) y (<!! pout) type (<!! pout)]
                       (if (and (number? x) (number? y) (number? type))
                         (do
                           (if (= type 2) (swap! total inc))
                           (recur))))))
              (println @total)))

(def part2  (let [[pin pout] (run-program (replace-value 0 2 core-program) 0 0) score (atom 0) ball (atom 0) paddle (atom 0)]
              (<!! (go-loop []
                     (let [x (<!! pout) y (<!! pout) type (<!! pout)]
                       (if (and (number? x) (number? y) (number? type))
                         (do
                           (cond
                             (= x -1) (reset! score type)
                             (= type 3) (reset! paddle x)
                             (= type 4) (do (reset! ball x) (move pin @ball @paddle)))
                           (recur))))))
              (println @score)))

(defn -main
  []
  (do part1 part2))