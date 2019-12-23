(ns aoc19.15
  (:gen-class)
  (:require [aoc19.core :refer [replace-value build-2d-array print-2d-array lazy-contains? replace-value-2d]]
            [aoc19.intcode :refer [run-program load-program]]
            [clojure.core.async :refer [<!! >!!]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))

(def core-program (load-program "day15.txt"))

(defn start-pos
  [program pos]
  {:pos pos :program {:pointer 0 :program program :relative-offset 0} :steps 0 :output 1})

(defn directions
  [[x y]]
  [[1 [x (- y 1)]] ;north
   [2 [x (+ y 1)]] ;south
   [3 [(- x 1) y]] ;west
   [4 [(+ x 1) y]]]) ;east

(defn find-neighbors
  [{pos :pos {pointer :pointer program :program relative-offset :relative-offset} :program steps :steps}]
  (println "Location" pos pointer steps)
  (for [direction (directions pos)
        :let [[input new-pos] direction
              [pin pout state] (run-program program pointer relative-offset :immediate)
              output (do (>!! pin input) (<!! pout))
              printer (println "Neighbor" new-pos input output)]
        :when (not= output 0)]
    {:pos new-pos :program @state :steps (inc steps) :output output}))

(defn breadth-first-search
  [initial-state get-neighbors done?]
  (loop [visited #{}
         max-depth 0
         todo [[0 initial-state]]]
    (if (empty? todo)
      [max-depth visited]
      (let [[depth state] (first todo)
            loc (:pos state)
            new-visited (conj visited loc)]
        (if (done? state) [depth state]
            (if (visited loc)
              (recur (conj visited loc) max-depth (rest todo))
              (recur (conj visited loc) (max max-depth depth) (concat (rest todo)
                                                               (map (fn [x] [(inc depth) x]) (get-neighbors state))))))))))

(defn explore
  [oxygen-pos]
  (breadth-first-search oxygen-pos find-neighbors (constantly false)))

(defn grid-neighbors
  [full-grid {pos :pos steps :steps :as state}]
  (for [direction (directions pos)
        :let [[_ new-loc] direction]
        :when (full-grid pos)]
    (assoc state :pos new-loc :steps (inc steps))))

(defn find-oxygen
  [program drone-pos]
  (breadth-first-search (start-pos program drone-pos) find-neighbors #(= 2 (:output %))))

(defn fill-oxygen
  [oxygen-pos full-grid]
  (breadth-first-search oxygen-pos (partial grid-neighbors full-grid) (constantly false)))

(def part1  (println (find-oxygen core-program [0 0])))
 
(def part2 (let [[_ visited] (find-oxygen core-program [0 0])
                 [_ full-grid] (explore visited)]
             (println (fill-oxygen visited full-grid))))

(defn -main
  []
  (do part1 part2))