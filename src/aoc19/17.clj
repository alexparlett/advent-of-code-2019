(ns aoc19.17
  (:gen-class)
  (:require [aoc19.core :refer [build-2d-array print-2d-array get-value-2d]]
            [aoc19.intcode :refer [run-program load-program]]
            [clojure.core.async :refer [<!! >!!]]
            [clojure.string :as string]))

(def core-program (load-program "day17.txt"))

(defn is-intersection?
  [[x y] map]
  (every? #(= \# (get-value-2d % map nil))
          [[x y]
           [x (- y 1)]
           [x (+ y 1)]
           [(- x 1) y]
           [(+ x 1) y]]))

(defn build-map
  [[pin pout]]
  (loop [map [] current []]
    (let [next-val (<!! pout)]
      (case next-val
        35 (recur map (conj current \#))
        46 (recur map (conj current \.))
        60 (recur map (conj current \<))
        94 (recur map (conj current \^))
        62 (recur map (conj current \>))
        86 (recur map (conj current \V))
        88 (recur map (conj current \X))
        10 (recur (conj map current) [])
        map))))

(def scaffolding (build-map (run-program core-program)))

(def part1  (println (reduce + (for [y (range (count scaffolding))
                           x (range (count (nth scaffolding y)))
                           :when (is-intersection? [x y] scaffolding)]
                       (* x y)))))

(def part2  (println "Omg"))

(defn -main
  []
  (do part1 part2))