(ns aoc19.12
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]))

(def moon1 {:x 17  :y -9  :z 4 :vx 0 :vy 0 :vz 0})
(def moon2 {:x 2 :y 2 :z -13 :vx 0 :vy 0 :vz 0})
(def moon3 {:x -1 :y 5 :z -1 :vx 0 :vy 0 :vz 0})
(def moon4 {:x 4 :y 7 :z -7 :vx 0 :vy 0 :vz 0})

; <x=-1, y=0, z=2>
; <x=2, y=-10, z=-7>
; <x=4, y=-8, z=8>
; <x=3, y=5, z=-1>
; (def moon1 {:x -1  :y 0  :z 2 :vx 0 :vy 0 :vz 0})
; (def moon2 {:x 2 :y -10 :z -7 :vx 0 :vy 0 :vz 0})
; (def moon3 {:x 4 :y -8 :z 8 :vx 0 :vy 0 :vz 0})
; (def moon4 {:x 3 :y 5 :z -1 :vx 0 :vy 0 :vz 0})

(defn calculate-axis-velocity
  [position velocity position-of-others]
  (+ velocity (reduce + (for [other-position position-of-others] (compare other-position position)))))

(defn calculate-velocity
  [current others]
  (let [nvx (calculate-axis-velocity (current :x) (current :vx) (map #(% :x) others))
        nvy (calculate-axis-velocity (current :y) (current :vy) (map #(% :y) others))
        nvz (calculate-axis-velocity (current :z) (current :vz) (map #(% :z) others))]
    (assoc current :vx nvx :vy nvy :vz nvz)))

(defn move-moon
  [moon]
  (assoc moon :x (+ (moon :x) (moon :vx)) :y (+ (moon :y) (moon :vy)) :z (+ (moon :z) (moon :vz))))

(defn step-simulation
  [[a b c d]]
  (let [na (move-moon (calculate-velocity a [b c d]))
        nb (move-moon (calculate-velocity b [a c d]))
        nc (move-moon (calculate-velocity c [b a d]))
        nd (move-moon (calculate-velocity d [b c a]))]
   [na nb nc nd]))

(defn until-step-limit
  [max-steps moons step]
  (= max-steps step))

(defn until-returned
  [axis moons step]
    (and (not= 0 step) (every? #(= 0 (% axis)) moons)))

(defn simulate
  [moons step until]
  (if (until moons step)
    [moons step]
    (recur (step-simulation moons) (inc step) until)))

(defn calculate-moon-energy
  [moon]
  (* 
   (+ (Math/abs (moon :x)) (Math/abs (moon :y)) (Math/abs (moon :z))) 
   (+ (Math/abs (moon :vx)) (Math/abs (moon :vy)) (Math/abs (moon :vz)))))

(defn calculate-total-energy
  [moons]
  (reduce + (map #(calculate-moon-energy %) moons)))

(def part1 (pprint (calculate-total-energy (first (simulate [moon1 moon2 moon3 moon4] 0 (partial until-step-limit 1000))))))

(def part2 (pprint (let [
                         xf (second (simulate [moon1 moon2 moon3 moon4] 0 (partial until-returned :vx)))
                         yf (second (simulate [moon1 moon2 moon3 moon4] 0 (partial until-returned :vy)))
                         zf (second (simulate [moon1 moon2 moon3 moon4] 0 (partial until-returned :vz)))
                         ] (* xf yf zf))))

(defn -main
  []
  (do part1 part2))