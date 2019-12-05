(ns aoc19.4
  (:gen-class))

(def end-password 576723)

(def start-password 109165)

(defn digits
  [number]
  (map #(Character/digit % 10) (str number)))

(defn validate-password1
  [pwd]
  (let [partitions (for [partition (partition 2 1 (digits pwd))] partition)]
    (and (every? #(<= (first %) (last %)) partitions) (some #(= (first %) (last %)) partitions))))

(defn validate-password2
  [pwd]
  (let [digits (digits pwd) partitions (for [partition (partition 2 1 digits)] partition)]
    (and (every? #(<= (first %) (last %)) partitions) (some #(and (= (first %) (last %)) (= 2 (get (frequencies digits) (first %)))) partitions))))

(defn get-password
  [start end validator]
  (for [pwd (range start end) :when (validator pwd)] pwd))

(def part1 (count (get-password start-password end-password validate-password1)))

(def part2 (count (get-password start-password end-password validate-password2)))

(defn -main
  [& args]
  (println [part1 part2]))