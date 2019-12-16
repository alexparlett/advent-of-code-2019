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

(defn print-2d-array
  [arr]
  (doseq [row arr] (println (string/join row))))
