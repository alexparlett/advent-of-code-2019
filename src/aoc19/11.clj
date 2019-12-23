(ns aoc19.11
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]])
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [replace-value build-2d-array]])
  (:require [aoc19.intcode :refer [run-program load-program]])
  (:require [clojure.core.async :refer [<!! put!]]))

(def core-program (load-program "day11.txt"))

(defn paint-hull-tile
  [hull x y painted input]
  (let [nr (replace-value x input (nth hull y))
        nh (replace-value y nr hull)
        np (conj painted [x y])]
    [nh np]))

(defn turn-robot
  [current input]
  (case current
    [0 1] (if (zero? input) [1 0] [-1 0])
    [1 0] (if (zero? input) [0 -1] [0 1])
    [0 -1] (if (zero? input) [-1 0] [1 0])
    [-1 0] (if (zero? input) [0 1] [0 -1])))

(defn move-robot
  [x y direction input]
  (let [nd (turn-robot direction input)
        nx (+ x (last nd))
        ny (- y (first nd))]
    [nx ny nd]))

(defn paint-hull
  [input output hull x y direction painted]
  (let [open (put! input (nth (nth hull y) x))
        pin (<!! output)
        tin (<!! output)]
    (if (and (true? open) (number? pin) (number? tin))
      (let [[nh np] (paint-hull-tile hull x y painted pin)
            [nx ny nd] (move-robot x y direction tin)]
        (recur input output nh nx ny nd np))
      [hull painted])))

(defn build-identifier
  [hull]
  (map #(string/replace (string/join %) \0 \space) hull))

(def part1 (pprint
            (count
             (distinct
              (second
               (let [[input output] (run-program core-program)]
                 (paint-hull input output (build-2d-array 128 128 0) 63 63 [1 0] [])))))))

(def part2 (doseq [row (build-identifier
                        (first
                         (let [[input output] (run-program core-program)]
                           (paint-hull input output (build-2d-array 128 128 0 1) 63 63 [1 0] []))))]
             (println row)))

(defn -main
  []
  (do part1 part2))