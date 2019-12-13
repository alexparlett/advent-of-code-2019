(ns aoc19.10
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string]])
  (:require [clojure.algo.generic.math-functions :as gmf])
  (:require [medley.core :as medley]))

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

(defn get-distance
  [start end]
  (Math/hypot (- (end :y) (start :y)) (- (end :x) (start :x))))

(defn get-asteroids-positions
  [asteroid-map current]
  (map #(assoc (assoc % :angle (get-angle current %)) :distance (get-distance current %)) (filter #(true? (get % :present)) asteroid-map)))

(defn calculate-visible-asteroids
  [asteroid-map current]
  (assoc current :visible (count (medley/distinct-by #(get % :angle) (get-asteroids-positions asteroid-map current)))))

(defn find-best-position
  [asteroid-map]
  (apply max-key :visible (for [position asteroid-map] (if (true? (position :present)) (calculate-visible-asteroids asteroid-map position) (assoc position :visible 0)))))

(defn destroy-asteroids
  [asteroids monitoring-station]
  (for [i (range 200)] (nth asteroids i)))

(defn place-monitoring-station
  [asteroid-map]
  (let [monitoring-station (find-best-position asteroid-map)]
    (destroy-asteroids (sort-by :angle (get-asteroids-positions asteroid-map monitoring-station)) monitoring-station)))

(def part1 (println (find-best-position (build-asteroid-map data-file))))

(def part2 (println (nth (place-monitoring-station (build-asteroid-map data-file)) 199)))

(defn -main
  [& args]
  (do part1 part2))