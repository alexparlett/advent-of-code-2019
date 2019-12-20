(ns aoc19.15
  (:gen-class)
  (:require [aoc19.core :refer [replace-value build-2d-array print-2d-array lazy-contains? replace-value-2d]]
            [aoc19.intcode :refer [run-program load-program]]
            [clojure.core.async :refer [<!! go-loop >!!]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))

(def core-program (load-program "day15.txt"))

(defn get-tile
  [ship-section x y action]
  {:pos [x y] :type (nth (nth ship-section y) x) :action action})

(defn get-adjacent-tiles
  [ship-section x y width height]
  (let [adjecencies [[1 x (inc y)] [4 (inc x) y] [3 (dec x) y] [2 x (dec y)]]]
    (for [[ta tx ty] adjecencies :when (and (< tx width) (>= tx 0) (< ty height) (>= ty 0))] (get-tile ship-section tx ty ta))))

(defn calculate-next-move
  [ship-section [x y] processed width height]
  (let [to-check (filter #(not (lazy-contains? processed (:pos %))) (get-adjacent-tiles ship-section x y width height))
        available-tiles (filter #(and (not= \X (:type %)) (not= \# (:type %)) (not= \O (:type %))) to-check)
        not-explored-tiles (filter #(= \space (:type %)) available-tiles)
        explored-tiles (filter #(= \. (:type %)) available-tiles)]
    (cond
      (empty? to-check) nil
      (not (empty? not-explored-tiles)) (first not-explored-tiles)
      (not (empty? explored-tiles)) (first (for [tile explored-tiles
                                                 :let [found (calculate-next-move ship-section (:pos tile) (concat processed (map #(:pos %) to-check)) width height)]
                                                 :when (some? found)] tile))
      :else nil)))

(defn move-drone
  [ship-section [ox oy :as old-pos] [nx ny] oxy-pos start-pos]
  (let [old-drone-removed (replace-value-2d ox oy (cond (= old-pos oxy-pos) \O (= old-pos start-pos) \X :else \.) ship-section)]
    (replace-value-2d nx ny \D old-drone-removed)))

(defn map-ship-section
  []
  (let [[pin pout] (run-program core-program 0 0) width 42 height 42 sx (int (Math/floor (/ width 2))) sy (int (Math/floor (/ height 2)))]
    (<!! (go-loop [ship-section (build-2d-array width height \space \D) [dx dy :as drone-pos] [sx sy] found-oxygen nil start-pos [sx sy]]
           (let [{next-move :action [ndx ndy :as new-drone-pos] :pos} (calculate-next-move ship-section drone-pos [] width height)]
             (if (some? next-move)
               (do
                 (>!! pin next-move)
                 (let [result (<!! pout)]
                   (case result
                     0 (recur (replace-value-2d ndx ndy \# ship-section) drone-pos found-oxygen start-pos)
                     1 (recur (move-drone ship-section drone-pos new-drone-pos found-oxygen start-pos) new-drone-pos found-oxygen start-pos)
                     2 (recur (move-drone ship-section drone-pos new-drone-pos found-oxygen start-pos) new-drone-pos new-drone-pos start-pos))))
               [(replace-value-2d dx dy \. ship-section) drone-pos found-oxygen start-pos width height]))))))

(defn is-dead-end?
  [ship-section x y width height]
  (let [tile (get-tile ship-section x y nil) adjacent-tiles (get-adjacent-tiles ship-section x y width height)]
    (and
     (not= (:type tile) \O)
     (not= (:type tile) \X)
     (or
      (= (:type tile) \#)
      (= (:type tile) \space)
      (= (:type tile) \!)
      (= 1 (count (filter #(or (= \. (:type %)) (= \O (:type %)) (= \X (:type %))) adjacent-tiles)))))))

(defn find-shortest-path
  [ship-section start end width height]
  (let [pathed-ship-section (atom ship-section)]
    (doseq [i (range 50)]
      (doseq [y (range height) x (range width) :let [closed (atom []) open (atom [])]]
        (if (is-dead-end? @pathed-ship-section x y width height)
          (do (reset! closed (conj @closed [x y]))
              (if (= (:type (get-tile @pathed-ship-section x y nil)) \.)
                (reset! pathed-ship-section (replace-value-2d x y \! @pathed-ship-section))))
          (reset! open (conj @open [x y]))))
      (doseq [y (reverse (range height)) x (reverse (range width)) :let [closed (atom []) open (atom [])]]
        (if (is-dead-end? @pathed-ship-section x y width height)
          (do (reset! closed (conj @closed [x y]))
              (if (= (:type (get-tile @pathed-ship-section x y nil)) \.)
                (reset! pathed-ship-section (replace-value-2d x y \! @pathed-ship-section))))
          (reset! open (conj @open [x y])))))
    (println (frequencies (flatten @pathed-ship-section)))
    (print-2d-array @pathed-ship-section)))

(def part1  (let [[ship-section drone-pos oxygen-pos start-pos width height] (map-ship-section)]
              (println oxygen-pos)
              (println drone-pos)
              (print-2d-array ship-section)
              (find-shortest-path ship-section oxygen-pos start-pos width height)))

(def part2  (println "Omg"))

(defn -main
  []
  (do part1 part2))