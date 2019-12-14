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
    (for [x (range 0 (count split-roids))]
      (if (= "#" (nth split-roids x))
        {:present true :x x :y y}
        {:present false :x x :y y}))))

(defn build-asteroid-map
  [raw-map]
  (let [asteroids (string/split-lines raw-map)]
    (flatten (for [y (range 0 (count asteroids))] (map-asteroids (nth asteroids y) y)))))

(defn get-angle
  [start end]
  (- 180 (Math/toDegrees (gmf/atan2 (- (end :x) (start :x)) (- (end :y) (start :y))))))

(defn get-distance
  [start end]
  (Math/hypot (- (end :y) (start :y)) (- (end :x) (start :x))))

(defn get-asteroids-positions
  [asteroid-map current]
  (filter #(not= (get % :distance) 0.0) (map #(assoc (assoc % :angle (get-angle current %)) :distance (get-distance current %)) (filter #(true? (get % :present)) asteroid-map))))

(defn calculate-visible-asteroids
  [asteroid-map current]
  (assoc current :visible (count (medley/distinct-by #(get % :angle) (get-asteroids-positions asteroid-map current)))))

(defn find-best-position
  [asteroid-map]
  (apply max-key :visible (for [position asteroid-map]
                            (if (true? (position :present))
                              (calculate-visible-asteroids asteroid-map position)
                              (assoc position :visible 0)))))

; if there were less than 200 visible asteroids this wouldnt work
; couldnt think how to loop round the angles once for the first asteroid then again for the second asteroid
(defn destroy-asteroids
  [asteroids monitoring-station]
  (let [target (last (for [i (range 200)] (first (second (nth asteroids i)))))]
    (+ (target :y) (* 100 (target :x)))))

(defn place-monitoring-station
  [asteroid-map]
  (let [monitoring-station (find-best-position asteroid-map)]
    (destroy-asteroids
     (sort-by first
              (group-by :angle
                        (sort-by :distance
                                 (get-asteroids-positions asteroid-map monitoring-station)))) monitoring-station)))

(def part1 (println (find-best-position (build-asteroid-map data-file))))

(def part2 (println (place-monitoring-station (build-asteroid-map data-file))))

(defn -main
  [& args]
  (do part1 part2))