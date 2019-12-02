(ns aoc19.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  )

(defn load-file-as-string 
    [file]
    (slurp (io/resource file))
)