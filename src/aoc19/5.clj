(ns aoc19.5
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string]])
  (:require [aoc19.intcode :refer [run-program]])
  (:require [clojure.core.async :refer [>!! <!! <! go-loop]]))

(def data-file (load-file-as-string "day5.txt"))

(def core-program (vec (map #(Integer/parseInt %) (string/split data-file #","))))

(def part1 (let [[input output] (run-program core-program)]
             (do
               (>!! input 1)
               (go-loop []
                 (let [result (<! output)]
                   (if (number? result)
                     (do (println result) (recur))
                     result))))))

(def part2 (let [[input output] (run-program core-program)]
             (do (>!! input 5) (println (<!! output)))))

(defn -main
  [& args]
  (do part1 part2))