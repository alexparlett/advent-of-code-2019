(ns aoc19.intcode
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string replace-value]])
  (:require [clojure.core.async :as a :refer [>! <! >!! <!! go chan go-loop close!]]))

(defn param-to-index
  [param]
  (case param
    :c 1
    :b 2
    :a 3))

(defn get-index
  [param operation program relative-offset]
  (let [param-index (param-to-index param) mode (get operation param)]
    (case mode
      0 (nth program (+ (operation :index) param-index))
      1 (+ (operation :index) param-index)
      2 (+ (nth program (+ (operation :index) param-index)) relative-offset))))

(defn get-value
  [param operation program relative-offset]
  (let [param-index (param-to-index param) mode (get operation param)]
    (case mode
      0 (nth program (get-index param operation program relative-offset))
      1 (nth program (get-index param operation program relative-offset))
      2 (nth program (get-index param operation program relative-offset)))))

(defn code-add
  [program operation relative-offset]
  (replace-value (get-index :a operation program relative-offset) (+ (get-value :c operation program relative-offset) (get-value :b operation program relative-offset)) program))

(defn code-mul
  [program operation relative-offset]
  (replace-value (get-index :a operation program relative-offset) (* (get-value :c operation program relative-offset) (get-value :b operation program relative-offset)) program))

(defn code-input
  [program operation input relative-offset]
  (replace-value (get-index :c operation program relative-offset) input program))

(defn code-output
  [program operation relative-offset]
  (get-value :c operation program relative-offset))

(defn code-relative
  [program operation relative-offset]
  (+ relative-offset (get-value :c operation program relative-offset)))

(defn code-jump-false
  [operation program relative-offset]
  (if (= 0 (get-value :c operation program relative-offset)) (get-value :b operation program relative-offset) (+ 3 (operation :index))))

(defn code-jump-true
  [operation program relative-offset]
  (if (not= 0 (get-value :c operation program relative-offset)) (get-value :b operation program relative-offset) (+ 3 (operation :index))))

(defn code-less
  [operation program relative-offset]
  (let [value (if (< (get-value :c operation program relative-offset) (get-value :b operation program relative-offset)) 1 0)]
    (replace-value (get-index :a operation program relative-offset) value program)))

(defn code-equal
  [operation program relative-offset]
  (let [value (if (= (get-value :c operation program relative-offset) (get-value :b operation program relative-offset)) 1 0)]
    (replace-value (get-index :a operation program relative-offset) value program)))

(defn digits
  [number]
  (map #(Character/digit % 10) (format "%05d" number)))

(defn get-op-code
  [digits]
  (Integer/parseInt (string/join (take-last 2 digits))))

(defn get-param-mode
  [digits index]
  (Integer/parseInt (str (nth digits index \0))))

(defn get-operation
  [program index]
  (let [digits (digits (nth program index))]
    {:de (get-op-code digits) :c (get-param-mode digits 2) :b (get-param-mode digits 1) :a (get-param-mode digits 0) :index index}))

(defn apply-code
  [program index input output relative-offset & [name]]
  (let [operation (get-operation program index)]
    (case (operation :de)
      1 {:step (+ 4 index) :program (code-add program operation relative-offset) :relative-offset relative-offset}
      2 {:step (+ 4 index) :program (code-mul program operation relative-offset) :relative-offset relative-offset}
      3 {:step (+ 2 index) :program (code-input program operation (<!! input) relative-offset) :relative-offset relative-offset}
      4 (do (>!! output (code-output program operation relative-offset)) {:step (+ 2 index) :program program :relative-offset relative-offset})
      5 {:step (code-jump-true operation program relative-offset) :program program :relative-offset relative-offset}
      6 {:step (code-jump-false operation program relative-offset) :program program :relative-offset relative-offset}
      7 {:step (+ 4 index) :program (code-less operation program relative-offset)  :relative-offset relative-offset}
      8 {:step (+ 4 index) :program (code-equal operation program relative-offset) :relative-offset relative-offset}
      9 {:step (+ 2 index) :program program :relative-offset (code-relative program operation relative-offset)}
      99 {:step nil :program program :relative-offset relative-offset})))

(defn run-program
  [program pointer relative-offset & [name]]
  (let [input (chan) output (chan)]
    (go-loop [program program pointer pointer relative-offset relative-offset]
      (let [result (apply-code program pointer input output relative-offset name)]
        (if (= (result :step) nil)
          (do (close! input) (>! output result) (close! output))
          (recur (result :program) (result :step) (result :relative-offset)))))
    [input output]))

(defn load-program
  [file]
  (let [program (map #(Long/parseLong %) (string/split (load-file-as-string file) #","))]
    (vec (concat program (repeat (- 2048 (count program)) 0)))))