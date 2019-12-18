(ns aoc19.14
  (:gen-class)
  (:require [aoc19.core :refer [load-file-as-string update-map binary-search]]
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

(defn perform-reactions
  [processing-chains product-key required spare-products]
  (if (= product-key :ORE)
    {product-key required}
    (let [spare-product (get @spare-products product-key 0) required (max (- required spare-product) 0) spare-product-left (max (- spare-product required) 0)]
      (reset! spare-products (assoc @spare-products product-key spare-product-left))
      (if (zero? required)
        {}
        (let [product (get-product processing-chains product-key)
              produces (get product product-key)
              requirements (:requires product)
              mult (Math/ceil (/ required produces))
              total-produced (* produces mult)
              spare (- total-produced required)]
          (reset! spare-products (update-in @spare-products [product-key] + spare))
          (update-map requirements (fn [k v] (* mult v))))))))

(defn reduce-requirements
  [requirements processing-chains spare-product]
  (let [reduced (for [[product-key required :as requirement] requirements] (perform-reactions processing-chains product-key required spare-product))]
    (apply merge-with + reduced)))

(defn calculate-ore-for-requirements
  [processing-chains requirements spare-product]
  (loop [requirements requirements]
    (if (every? #(= :ORE %) (keys requirements))
      requirements
      (recur (reduce-requirements requirements processing-chains spare-product)))))

(defn calculate-total-ore-required
  [processing-chains fuel-requirements]
  (:ORE (calculate-ore-for-requirements processing-chains {:FUEL fuel-requirements} (atom {}))))

(def part1 (pprint (calculate-total-ore-required (load-processing-chains) 1)))

(def part2 (pprint (binary-search  (partial calculate-total-ore-required (load-processing-chains)) 1000000000000 0 1000000000000)))

(defn -main
  []
  (do part1 part2))