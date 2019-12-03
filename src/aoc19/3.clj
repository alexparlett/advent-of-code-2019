(ns aoc19.3
  (:import [java.awt.geom Line2D])
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  (:require [clojure.data :as data])
  (:require [aoc19.core :refer [load-file-as-string]]))

(deftype Segment [x1 y1 x2 y2 steps]
  Object
  (toString [_] (str "start (" (pr-str x1) "," (pr-str y1) ") end (" (pr-str x2) "," (pr-str y2) ") steps " (pr-str steps)))
)

(def data-file (load-file-as-string "3/wires.txt"))

(def wires
  (vec
   (map
    #(string/split % #",")
    (string/split-lines data-file))))

(def wire-one (vec (nth wires 0)))

(def wire-two (vec (nth wires 1)))

(defn plot-instruction
  [previous instruction]
  (let [direction (subs instruction 0 1) movement (Integer/parseInt (subs instruction 1)) before (or (last previous) (Segment. 0 0 0 0 0))]
    (case direction
      "U" (conj previous (Segment. (.x2 before) (.y2 before) (+ (.x2 before) movement) (.y2 before) (+ (.steps before) movement)))
      "D" (conj previous (Segment. (.x2 before) (.y2 before) (- (.x2 before) movement) (.y2 before) (+ (.steps before) movement)))
      "R" (conj previous (Segment. (.x2 before) (.y2 before) (.x2 before) (+ (.y2 before) movement) (+ (.steps before) movement)))
      "L" (conj previous (Segment. (.x2 before) (.y2 before) (.x2 before) (- (.y2 before) movement) (+ (.steps before) movement)))
    )
  )
)

(defn plot-wire
  [wire]
  (reduce #(plot-instruction %1 %2) [] wire)
)

(defn get-a 
  [seg] 
  (- (.y2 seg) (.y1 seg))
)

(defn get-b 
  [seg] 
  (- (.x1 seg) (.x2 seg))
)

(defn get-c 
  [seg] 
  (+ (* (get-a seg) (.x1 seg)) (* (get-b seg) (.y1 seg)))
)

(defn get-determinate
  [left right]
  (- (* (get-a left) (get-b right)) (* (get-a right) (get-b left)))
)

(defn find-intersection-point
  [left right]
  (let 
    [
      x (/ (- (* (get-b right) (get-c left)) (* (get-b left) (get-c right))) (get-determinate left right))
      y (/ (- (* (get-a left) (get-c right)) (* (get-a right) (get-c left))) (get-determinate left right))
    ]
    {
      :x x 
      :y y
      :steps (+ (- (.steps left) (Math/abs (- (.x2 left) x)) (Math/abs (- (.y2 left) y))) (- (.steps right) (Math/abs (- (.x2 right) x)) (Math/abs (- (.y2 right) y))))
    }
  )
)

(defn find-intersection
  [left right]
  (for [l left r right :when (and (and (not= (.x1 l) (.x1 r)) (not= (.y1 l) (.y1 r))) (Line2D/linesIntersect (.x1 l) (.y1 l) (.x2 l) (.y2 l) (.x1 r) (.y1 r) (.x2 r) (.y2 r)))] (find-intersection-point l r))
)

(defn map-distances
  [intersection]
  (map #(+ (Math/abs (get % :x)) (Math/abs (get % :y))) intersection)
)

(defn closest
  [distances]
  (apply min distances)
)

(defn map-steps
  [intersection]
  (map #(get % :steps) intersection)
)


(def part1 (closest (map-distances (find-intersection (plot-wire wire-one) (plot-wire wire-two)))))

; (def part1 (closest (map-distances (find-intersection (plot-wire ["R75","D30","R83","U83","L12","D49","R71","U7","L72"]) (plot-wire ["U62","R66","U55","R34","D71","R55","D58","R83"])))))

(def part2 (closest (map-steps (find-intersection (plot-wire wire-one) (plot-wire wire-two)))))

; (def part2 (closest (map-steps (find-intersection (plot-wire ["R75","D30","R83","U83","L12","D49","R71","U7","L72"]) (plot-wire ["U62","R66","U55","R34","D71","R55","D58","R83"])))))

(defn -main
  [& args]
  (println [part1 part2])
)