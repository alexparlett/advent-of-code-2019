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
