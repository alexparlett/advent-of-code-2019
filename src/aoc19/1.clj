(ns aoc19.1
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  )

(def data-file (io/resource
                 "1/modules.txt"))                          ; the url of the resource file

(defn get-modules
  []
  (map
    (fn [module] (Integer/parseInt module))                 ; map each line to an integer
    (string/split-lines (slurp data-file)))                 ; load file and split the lines
  )

(defn calculate-fuel-required-for-weight
  [weight]
  (Math/max (- (Math/floor (/ weight 3)) 2) 0.0)            ; divide by 3, round down, minus 2 and max or 0
  )

(defn calculate-fuel-required-for-module
  [weight fuel]
  (if (zero? weight)                                        ; is our current weight check greater than 0
    fuel                                                    ; if so return the currently fuel
    (recur                                                  ; else do this recursively
      (calculate-fuel-required-for-weight weight)           ; calculate the weight of the required fuel
      (+ fuel (calculate-fuel-required-for-weight weight))  ; add the current fuel and the new fuel
      )
    )
  )

(defn calculate-modules-fuel
  []
  (->> (get-modules)                                        ; get all modules
       (map #(calculate-fuel-required-for-module % 0))      ; map them to calculated fuel requirements
       (reduce +)                                           ; sum them all together
       (println)                                            ; print for answer
       )
  )

(defn -main
  [& args]
  (calculate-modules-fuel)
  )
