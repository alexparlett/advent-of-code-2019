(ns aoc19.15
  (:gen-class)
  (:require [aoc19.core :refer [replace-value build-2d-array print-2d-array lazy-contains?]]
            [aoc19.intcode :refer [run-program load-program]]
            [clojure.core.async :refer [<!! go-loop >!!]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))

(def core-program (load-program "day15.txt"))

(defn get-tile
  [ship-section x y action]
  {:pos [x y] :type (nth (nth ship-section y) x) :action action})

(defn calculate-next-move
  [ship-section [x y] processed]
  (let [[ux uy :as next-up] [x (inc y)]
        [rx ry :as next-right] [(inc x) y]
        [lx ly :as next-left] [(dec x) y]
        [dx dy :as next-down] [x (dec y)]
        up-tile (get-tile ship-section ux uy [1 next-up])
        right-tile (get-tile ship-section rx ry [4 next-right])
        left-tile (get-tile ship-section lx ly [3 next-left])
        down-tile (get-tile ship-section dx dy [2 next-down])
        to-check (filter #(not (lazy-contains? processed (:pos %))) [up-tile right-tile left-tile down-tile])
        available-tiles (filter #(and (not= \X (:type %)) (not= \# (:type %)) (not= \O (:type %))) to-check)
        not-explored-tiles (filter #(= \space (:type %)) available-tiles)
        explored-tiles (filter #(= \. (:type %)) available-tiles)]
    (cond
      (empty? to-check) nil
      (not (empty? not-explored-tiles)) (:action (first not-explored-tiles))
      (not (empty? explored-tiles)) (:action (first (for [tile explored-tiles
                                                          :let [found (calculate-next-move ship-section (:pos tile) (concat processed (map #(:pos %) to-check)))]
                                                          :when (some? found)] tile)))
      :else nil)))

(defn move-drone
  [ship-section [ox oy :as old-pos] [nx ny] oxy-pos start-pos]
  (let [old-drone-removed (replace-value oy (replace-value ox (cond (= old-pos oxy-pos) \O (= old-pos start-pos) \X :else \.) (nth ship-section oy)) ship-section)]
    (replace-value ny (replace-value nx \D (nth old-drone-removed ny)) old-drone-removed)))

(defn map-ship-section
  []
  (let [[pin pout] (run-program core-program 0 0) width 100 height 100 sx (/ width 2) sy (/ height 2)]
    (<!! (go-loop [ship-section (build-2d-array width height \space \D) drone-pos [sx sy] found-oxygen nil start-pos [sx sy]]
           (let [[next-move [x y :as new-drone-pos]] (calculate-next-move ship-section drone-pos [])]
             (if (some? next-move)
               (do 
                 (>!! pin next-move)
                 (let [result (<!! pout)]
                   (case result
                     0 (recur (replace-value y (replace-value x \# (nth ship-section y)) ship-section) drone-pos found-oxygen start-pos)
                     1 (recur (move-drone ship-section drone-pos new-drone-pos found-oxygen start-pos) new-drone-pos found-oxygen start-pos)
                     2 (recur (move-drone ship-section drone-pos new-drone-pos found-oxygen start-pos) new-drone-pos new-drone-pos start-pos))))
               [ship-section drone-pos found-oxygen start-pos]))))))

(def part1  (let [[ship-section drone-pos oxygen-pos start-pos] (map-ship-section)]
              (println oxygen-pos)
              (println drone-pos)
              (print-2d-array ship-section)))

(def part2  (println "Omg"))

(defn -main
  []
  (do part1 part2))