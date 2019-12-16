(ns aoc19.13
  (:gen-class)
  (:require [aoc19.core :refer [replace-value print-2d-array]])
  (:require [aoc19.intcode :refer [run-program load-program]])
  (:require [clojure.core.async :refer [<!! >!! go-loop chan dropping-buffer take!]]))

(def core-program (load-program "day13.txt"))

(defn to-tile
  [type]
  (case type
    0 \space
    1 \#
    2 \|
    3 \_
    4 \O))

(defn add-tile-to-map
  [score map x y type]
  (if (and (= -1 x) (= y 0))
    [type map]
    [score(replace-value y (replace-value x (to-tile type) (nth map y)) map)]))

(defn create-game
  [map in out]
  (go-loop [score 0 map map]
    (let [x (<!! in) y (<!! in) type (<!! in)]
      (if (and (number? x) (number? y) (number? type))
        (let [[scr nm] (add-tile-to-map score map x y type)] (do (>!! out [scr map]) (recur scr nm)))
        [score map]))))

(defn build-map
  [xrange yrange]
  (vec (for [y (range yrange)]
         (vec (for [x (range xrange)] \space)))))

(def part1 (println
            (frequencies
             (flatten
              (second
               (let [[in out] (run-program core-program 0 0) state (chan (dropping-buffer 1))]
                 (<!! (create-game (build-map 23 42) out state))))))))

(def part2 (let [[in out] (run-program (replace-value 0 2 core-program) 0 0) state (chan (dropping-buffer 1))]
             (do
               (create-game (build-map 23 42) out state)
               (<!! (go-loop [input (read-line)]
                      (do
                        (take! state (fn [[score map]] (print-2d-array map)))
                        (case input
                          "x" input
                          "a" (do (>!! in -1) (recur (read-line)))
                          "s" (do (>!! in 0) (recur (read-line)))
                          "d" (do (>!! in 1) (recur (read-line))))))))))

(defn -main
  []
  (do part1 part2))