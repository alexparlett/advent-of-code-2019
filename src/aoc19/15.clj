(ns aoc19.15
  (:gen-class)
  (:require [aoc19.core :refer [replace-value build-2d-array print-2d-array]]
            [aoc19.intcode :refer [run-program load-program]]
            [clojure.core.async :refer [<!! go-loop >!!]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))

(def core-program (load-program "day15.txt"))

(defn get-tile
  [ship-section x y]
  (nth (nth ship-section y) x))

(defn calculate-next-move
  [ship-section [x y]]
  (let [[ux uy :as next-up] [x (inc y)]
        [rx ry :as next-right] [(inc x) y]
        [lx ly :as next-left] [(dec x) y]
        [dx dy :as next-down] [x (dec y)]
        up-tile (get-tile ship-section ux uy)
        right-tile (get-tile ship-section rx ry)
        left-tile (get-tile ship-section lx ly)
        down-tile (get-tile ship-section dx dy)
        available-tiles (filter #(not= \# %) [up-tile right-tile left-tile down-tile])
        not-explored-tiles (filter #(= \space %) available-tiles)
        explored-tiles (filter #(= \. %) available-tiles)]
  (cond
    (not (empty? not-explored-tiles)) (cond
                                        (= up-tile (first not-explored-tiles)) [1 next-up]
                                        (= right-tile (first not-explored-tiles)) [4 next-right]
                                        (= down-tile (first not-explored-tiles)) [2 next-down]
                                        (= left-tile (first not-explored-tiles)) [3 next-left])
    (not (empty? explored-tiles)) (cond
                                    (= up-tile (first not-explored-tiles)) [1 next-up]
                                    (= right-tile (first not-explored-tiles)) [4 next-right]
                                    (= down-tile (first not-explored-tiles)) [2 next-down]
                                    (= left-tile (first not-explored-tiles)) [3 next-left])
    :else "Im lost..."
    )))

(defn move-drone
  [ship-section [ox oy] [nx ny]]
  (let [old-drone-removed (replace-value oy (replace-value ox \. (nth ship-section oy)) ship-section)]
    (replace-value ny (replace-value nx \D (nth old-drone-removed ny)) old-drone-removed)))

(defn map-ship-section
  []
  (let [[pin pout] (run-program core-program 0 0) width 100 height 100 sx (/ width 2) sy (/ height 2)]
    (<!! (go-loop [ship-section (build-2d-array width height \space \D) drone-pos [sx sy]]
           (print-2d-array ship-section)
           (println "Drone Pos :" drone-pos)
           (let [[next-move [x y :as new-drone-pos]] (calculate-next-move ship-section drone-pos)]
             (println "Input :" next-move)
             (>!! pin next-move)
             (let [result (<!! pout)]
               (println "Output :" result)
               (case result
                 0 (recur (replace-value y (replace-value x \# (nth ship-section y)) ship-section) drone-pos)
                 1 (recur (move-drone ship-section drone-pos new-drone-pos) new-drone-pos)
                 2 [(replace-value y (replace-value x \O (nth ship-section y)) ship-section) new-drone-pos [sx sy]])))))))

(def part1  (let [[ship-section oxygen-pos drone-pos] (map-ship-section)]
              (println oxygen-pos)
              (println drone-pos)
              (print-2d-array ship-section)))

(def part2  (println "Omg"))

(defn -main
  []
  (do part1 part2))