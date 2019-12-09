(ns aoc19.8
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string]]))

(def data-file (load-file-as-string "day8.txt"))

(def image-size (* 25 6))

(def layers (partition image-size (digits data-file)))

(defn digits
  [input]
  (map #(Character/digit % 10) input))

(def part1 (println
            (first
             (sort-by first
                      (for [layer (map #(frequencies %) layers)]
                        {(get layer 0 Integer/MAX_VALUE) (* (get layer 1 Integer/MAX_VALUE) (get layer 2 Integer/MAX_VALUE))})))))

(def part2 (println "Omg"))

(defn -main
  [& args]
  (do part1 part2))