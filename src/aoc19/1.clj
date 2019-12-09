(ns aoc19.1
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string]]))

(def data-file (load-file-as-string "day1.txt"))                         ; the url of the resource file

(defn get-modules
  []
  (map
   #(Integer/parseInt %)                                   ; map each line to an integer
   (string/split-lines data-file))                         ; load file and split the lines
  )

(defn fuel
  [mass]
  (- (quot mass 3) 2))                                    ; divide by 3, round down, minus 2 and max or 0


(defn total-fuel
  [mass]
  (reduce + (take-while pos? (rest (iterate fuel mass)))))

(def part1
  (reduce + (map fuel (get-modules))))

(def part2
  (reduce + (map total-fuel (get-modules))))

(defn -main
  [& args]
  (println [part1, part2]))
