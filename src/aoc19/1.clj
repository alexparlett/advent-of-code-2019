(ns aoc19.1
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  )

(def data-file (io/resource
                 "1/modules.txt"))

(defn get-modules
  []
  (map
    (fn [module] (Integer/parseInt module))
    (string/split-lines (slurp data-file)))
  )

(defn calculate-fuel-required-for-weight
  [weight]
  (Math/max (- (Math/floor (/ weight 3)) 2) 0.0)
  )

(defn calculate-fuel-required-for-module
  [weight fuel]
  (if (zero? weight)
    fuel
    (recur
      (calculate-fuel-required-for-weight weight)
      (+ fuel (calculate-fuel-required-for-weight weight))
      )
    )
  )

(defn calculate-modules-fuel
  []
  (->> (get-modules)
       (map #(calculate-fuel-required-for-module % 0))
       (reduce +)
       (println)
       )
  )

(defn -main
  [& args]
  (calculate-modules-fuel)
  )
