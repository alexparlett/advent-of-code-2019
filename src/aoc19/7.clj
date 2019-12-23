(ns aoc19.7
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [clojure.math.combinatorics :as combo])
  (:require [aoc19.core :refer [load-file-as-string]])
  (:require [aoc19.intcode :refer [run-program]])
  (:require [clojure.core.async :refer [>!! >! <!! <! go-loop put!]]))

(def data-file (load-file-as-string "day7.txt"))

(def core-program (vec (map #(Integer/parseInt %) (string/split data-file #","))))

(defn amp
  [program phase name]
  (let [[input output] (run-program program)]
    (do (>!! input phase) [input output])))

(defn feedback
  [initial previous a-in a-out b-in b-out c-in c-out d-in d-out e-in e-out]
  (let [open (put! a-in initial)]
    (if (true? open)
      (do
        (>!! b-in (<!! a-out))
        (>!! c-in (<!! b-out))
        (>!! d-in (<!! c-out))
        (>!! e-in (<!! d-out))
        (recur (<!! e-out) initial a-in a-out b-in b-out c-in c-out d-in d-out e-in e-out))
      initial)))

(defn run
  [program phase-permutations]
  (for [phases phase-permutations]
    (let [[a-in a-out] (amp program (nth phases 0) (str "A " phases))
          [b-in b-out] (amp program (nth phases 1) (str "B " phases))
          [c-in c-out] (amp program (nth phases 2) (str "C " phases))
          [d-in d-out] (amp program (nth phases 3) (str "D " phases))
          [e-in e-out] (amp program (nth phases 4) (str "E " phases))]
      (feedback 0 nil a-in a-out b-in b-out c-in c-out d-in d-out e-in e-out))))

(def part1 (println (apply max (run core-program (combo/permutations (range 5))))))

(def part2 (println (apply max (run core-program (combo/permutations (range 5 10))))))

; (def test (println (apply max (run [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [[9,8,7,6,5]]))))

(defn -main
  [& args]
  (do part1 part2))