(ns aoc19.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string]))

(defn load-file-as-string
  [file]
  (slurp (io/resource file)))

(defn replace-value
  [index value arr]
  (assoc arr index value))

(defn update-map [m f]
  (reduce-kv (fn [m k v]
               (assoc m k (f k v))) {} m))

(defn binary-search [f goal low high]
  (:probe
   (apply max-key
          #(:res %)
          (loop [low low
                 high high
                 probes '()]
            (if (< high low)
              probes
              (let [mid (quot (+ low high) 2)
                    res (f mid)
                    latest-res' (conj probes {:probe mid :res res})]
                (cond
                  (< res goal) (recur (inc mid) high latest-res')
                  (> res goal) (recur low (dec mid) probes)
                  :else {:res mid})))))))


(defn build-2d-array
  [xrange yrange initial-value & [mid-point-value]]
  (vec (for [y (range yrange)]
         (vec (for [x (range xrange)]
                (if (and (= (Math/floor (/ xrange 2)) x) 
                         (= (Math/floor (/ yrange 2)) y) )
                         mid-point-value 
                         initial-value))))))

(defn print-2d-array
  [arr]
  (doseq [row arr] (println (string/join row))))