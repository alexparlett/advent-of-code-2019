(ns aoc19.6
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string]]))

(def data-file (load-file-as-string "day6.txt"))

(defn get-orbits
  [data]
  (map #(string/split % #"\)") (string/split-lines data)))

(defn group-orbits
  [orbits]
  (reduce (fn [dict [parent child]] (assoc dict child parent)) {} orbits))

(defn count-orbits-for-planet
  [orbits planet]
  (take-while #(not= "COM" %) (iterate orbits planet)))

(defn count-orbits
  [orbits]
  (reduce + (map #(count %) (map #(count-orbits-for-planet orbits %) (keys orbits)))))

(defn get-orbital-path
  [orbits start end]
  (concat (count-orbits-for-planet orbits start) (count-orbits-for-planet orbits end)))

(defn count-orbital-transitions
  [orbits start end]
  (- (count (filter #(= 1 (second %)) (frequencies (get-orbital-path orbits start end)))) 2))

(def part1 (println (count-orbits (group-orbits (get-orbits data-file)))))

(def part2 (let [orbits (group-orbits (get-orbits data-file))] (println (count-orbital-transitions orbits "YOU" "SAN"))))

(defn -main
  [& args]
  (do part1 part2))