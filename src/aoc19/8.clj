(ns aoc19.8
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string]]))

(def data-file (load-file-as-string "day8.txt"))

(def image-size (* 25 6))

(defn digits
  [input]
  (map #(Character/digit % 10) input))

(def layers (partition image-size (digits data-file)))

(defn merge-layer
  [a b]
  (for [i (range 0 image-size)] (min (nth a i 2) (nth b i 2))))

(defn get-color
  [i]
  (let [pixel (first (filter #(not= 2 %) (map #(nth % i) layers)))] (if (= 0 pixel) \space \x))
)

(def decode (map #(string/join %) (partition 25 (map #(get-color %) (range 0 image-size)))))

(def part1 (println (let [minimum (apply min-key #(% 0) (map #(frequencies %) layers))] (* (minimum 1) (minimum 2)))))

(def part2 (doseq [row decode] (println row)))

(defn -main
  [& args]
  (do part1 part2))