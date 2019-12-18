(ns aoc19.14
  (:gen-class)
  (:require [aoc19.core :refer [load-file-as-string replace-value update-map]]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [com.rpl.specter :as specter]))

(defn parse-product
  [str]
  (let [[amount product] (string/split (string/trim str) #" ")]
    {(keyword product) (Integer/parseInt amount)}))

(defn parse-inputs
  [str]
  (let [products (string/split (string/trim str) #",")]
    (apply merge (map #(parse-product %) products))))

(defn parse-processing-chain
  [line]
  (let [[input output] (string/split line #"\=\>")]
    (assoc (parse-product output) :requires (parse-inputs input))))

(defn load-processing-chains
  []
  (let [str (load-file-as-string "day14.txt")]
    (map #(parse-processing-chain %) (string/split-lines str))))

(defn get-product
  [processing-chains product]
  (first (filter #(contains? % product) processing-chains)))

(defn has-only-ore
  [product processing-chains]
  (let [found (get-product processing-chains product) result (every? #(= % :ORE) (keys (:requires found)))]
    result))

(defn required-product-for-product
  [product amount processing-chains required remaining]
  (let [found (first (filter #(contains? % product) processing-chains))]
      (let [produces (get found product)
            mult (Math/ceil (/ amount produces))
            total-produced (* produces mult)
            spare (- total-produced amount)
            needs (get (:requires found) required)]
        (reset! remaining (assoc @remaining product spare))
        (println "Remaining " @remaining)
        (* mult needs))))

(defn reduce-requirements
  [requirements processing-chains remaining]
  (let [reduced (for [[key val :as requirement] requirements :let [product (first (filter #(contains? % key) processing-chains))]]
                                      (if (has-only-ore key processing-chains)
                                        {key val}
                                        (update-map (:requires product) 
                                                    (fn [k v] (required-product-for-product key val processing-chains k remaining)))))]
    (apply merge-with + reduced)))

(defn get-requirements
  [processing-chains products-requiring-ore]
  (loop [requirements (:requires (get-product processing-chains :FUEL)) remaining (atom {})]
    (println "Requirements " requirements)
    (if (every? #(has-only-ore % processing-chains) (keys requirements))
      [requirements remaining]
      (recur (reduce-requirements requirements processing-chains remaining) remaining))))

(defn get-products-requiring-ore
  [processing-chains]
  (filter #(contains? (:requires %) :ORE) processing-chains))

(defn get-ore-for-requirements
  [requirements products-requiring-ore]
  (println requirements)
  (reduce + (map #(required-product-for-product (key %) (val %) products-requiring-ore :ORE (atom {})) requirements)))

(defn calculate-total-ore-required
  [processing-chains]
  (let [products-requiring-ore (get-products-requiring-ore processing-chains) [requirements remaining] (get-requirements processing-chains products-requiring-ore)]
    (println requirements @remaining)
    (get-ore-for-requirements (update-map requirements (fn [k v] (- v (get @remaining k 0)))) processing-chains)))

(def part1 (pprint (calculate-total-ore-required (load-processing-chains))))
(def part2 (println "Omg"))

(defn -main
  []
  (do part1 part2))