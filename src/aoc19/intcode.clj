(ns aoc19.intcode
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string replace-value]]))

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
  (let [value (get-value :c operation program relative-offset)] (do (println value) value)))

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
  [program index inputs output relative-offset]
  (let [operation (get-operation program index)]
    ; (do (println [inputs output relative-offset operation])
    (case (operation :de)
      1 {:step (+ 4 index) :program (code-add program operation relative-offset) :output output :inputs inputs :relative-offset relative-offset}
      2 {:step (+ 4 index) :program (code-mul program operation relative-offset) :output output :inputs inputs :relative-offset relative-offset}
      3 {:step (+ 2 index) :program (code-input program operation (first inputs) relative-offset) :output output :inputs (rest inputs) :relative-offset relative-offset}
      4 {:step (+ 2 index) :program program :output (code-output program operation relative-offset) :inputs inputs :relative-offset relative-offset}
      5 {:step (code-jump-true operation program relative-offset) :program program :output output :inputs inputs :relative-offset relative-offset}
      6 {:step (code-jump-false operation program relative-offset) :program program :output output :inputs inputs :relative-offset relative-offset}
      7 {:step (+ 4 index) :program (code-less operation program relative-offset)  :output output :inputs inputs :relative-offset relative-offset}
      8 {:step (+ 4 index) :program (code-equal operation program relative-offset) :output output :inputs inputs :relative-offset relative-offset}
      9 {:step (+ 2 index) :program program :output output :inputs inputs :relative-offset (code-relative program operation relative-offset)}
      99 {:step nil :program program :output output :inputs inputs :relative-offset relative-offset})))

(defn run-program
  [program pointer inputs output relative-offset]
  (let [result (apply-code program pointer inputs output relative-offset)]
    (if (= (result :step) nil)
      result
      (recur (result :program) (result :step) (result :inputs) (result :output) (result :relative-offset)))))

(defn load-program
  [file]
  (let [program (map #(Long/parseLong %) (string/split (load-file-as-string file) #","))] 
    (vec (concat program (repeat (- 2048 (count program)) 0)))))