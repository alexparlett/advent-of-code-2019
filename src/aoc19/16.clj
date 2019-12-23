(ns aoc19.16
  (:gen-class)
  (:require [aoc19.core :refer [load-file-as-string digits]]))

(def data-file (load-file-as-string "day16.txt"))

(def pattern [0 1 0 -1])

(defn fft-mul
  [iteration length]
  (let [multiplied-pattern (mapcat (partial repeat iteration) pattern)
        repeated-pattern (rest (take (inc length) (cycle multiplied-pattern)))]
    repeated-pattern))

(defn map-input-value
  [input fft length]
  (let [new-val (reduce + (for [i (range length)
                                :let [val (nth input i) mul (nth fft i)]]
                            (* val mul)))
        string-val (str new-val)
        start (max (- (.length string-val) 1) 0)
        last (subs string-val start)
        result (Integer/parseInt last)]
    result))

(defn fft-phase
  [input length phase]
  (let [iteration (for [index (range length) :let [fft (fft-mul (inc index) length)]]
                    (map-input-value input fft length))]
    iteration))

(defn build-fft
  [input length max-phases]
  (loop [input input phase 0]
    (if (= phase max-phases)
      input
      (recur (fft-phase input length phase) (inc phase)))))

(def part1 (let [input (digits data-file) length (count input)]
             (println (take 8 (build-fft input length 100)))))

(def part2 (let [input (digits data-file) 
                 message (flatten (repeat 10000 input)) 
                 offset (Integer/parseInt (apply str (take 7 input))) 
                 length (count message)
                 decoded (build-fft message length 100)]
             (println (take 8 (drop offset decoded)))))

(defn -main
  [& args]
  (do part1 part2))