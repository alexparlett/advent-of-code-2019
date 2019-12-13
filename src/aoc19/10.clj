(ns aoc19.10
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string]])
  (:require [clojure.algo.generic.math-functions :as gmf]))

(def data-file (load-file-as-string "day10.txt"))

(defn map-asteroids
  [asteroids y]
  (let [split-roids (string/split asteroids #"")]
    (for [i (range 0 (count split-roids))] (if (= "#" (nth split-roids i)) {:present true :x i :y y} {:present false :x i :y y}))))

(defn build-asteroid-map 
  [raw-map]
  (let [asteroids (string/split-lines raw-map)]
    (flatten (for [y (range 0 (count asteroids))] (map-asteroids (nth asteroids y) y)))))

(defn get-angle
  [start end]
  (Math/toDegrees (gmf/atan2 (- (end :y) (start :y)) (- (end :x) (start :x)))))

(defn calculate-visible-asteroids
  [asteroid-map current]
  (assoc current :visible (count (distinct (map #(get-angle current %) (filter #(true? (get % :present)) asteroid-map))))))

(defn find-best-position
  [asteroid-map]
  (apply max-key :visible (for [position asteroid-map] (if (true? (position :present)) (calculate-visible-asteroids asteroid-map position) (assoc position :visible 0)))))

(def part1 (let [asteroid-map (build-asteroid-map data-file)] (println (find-best-position asteroid-map))))

(def part2 (println "Omg"))

(defn -main
  [& args]
  (do part1 part2))