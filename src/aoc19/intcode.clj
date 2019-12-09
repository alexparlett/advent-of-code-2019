(ns aoc19.intcode
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [replace-value]]))

(defn get-value
  [param operation program]
  (case param
    :c (if (= 0 (get operation param)) (nth program (nth program (+ (get operation :index) 1))) (nth program (+ (get operation :index) 1)))
    :b (if (= 0 (get operation param)) (nth program (nth program (+ (get operation :index) 2))) (nth program (+ (get operation :index) 2)))
    :a (if (= 0 (get operation param)) (nth program (nth program (+ (get operation :index) 3))) (nth program (+ (get operation :index) 3)))))

(defn code-add
  [program operation]
  (replace-value (nth program (+ 3 (get operation :index))) (+ (get-value :c operation program) (get-value :b operation program)) program))

(defn code-mul
  [program operation]
  (replace-value (nth program (+ 3 (get operation :index))) (* (get-value :c operation program) (get-value :b operation program)) program))

(defn code-input
  [program operation input]
  (replace-value (nth program (+ 1 (get operation :index))) input program))

(defn code-output
  [program operation]
  (get-value :c operation program))

(defn code-jump-false
  [operation program]
  (if (= 0 (get-value :c operation program)) (get-value :b operation program) (+ 3 (get operation :index))))

(defn code-jump-true
  [operation program]
  (if (not= 0 (get-value :c operation program)) (get-value :b operation program) (+ 3 (get operation :index))))

(defn code-less
  [operation program]
  (let [value (if (< (get-value :c operation program) (get-value :b operation program)) 1 0)]
    (replace-value (nth program (+ 3 (get operation :index))) value program)))

(defn code-equal
  [operation program]
  (let [value (if (= (get-value :c operation program) (get-value :b operation program)) 1 0)]
    (replace-value (nth program (+ 3 (get operation :index))) value program)))

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
  [program index inputs output]
  (let [operation (get-operation program index)]
    (case (get operation :de)
      1 {:step (+ 4 index) :program (code-add program operation) :output output :inputs inputs}
      2 {:step (+ 4 index) :program (code-mul program operation) :output output :inputs inputs}
      3 {:step (+ 2 index) :program (code-input program operation (first inputs)) :output output :inputs (rest inputs)}
      4 {:step (+ 2 index) :program program :output (code-output program operation) :inputs inputs}
      5 {:step (code-jump-true operation program) :program program :output output :inputs inputs}
      6 {:step (code-jump-false operation program) :program program :output output :inputs inputs}
      7 {:step (+ 4 index) :program (code-less operation program) :output output :inputs inputs}
      8 {:step (+ 4 index) :program (code-equal operation program) :output output :inputs inputs}
      99 {:step nil :program program :output output :inputs inputs})))

(defn run-program
  [program pointer inputs output]
  (let [result (apply-code program pointer inputs output)]
    (if (= (get result :step) nil)
      result
      (recur (get result :program) (get result :step) (get result :inputs) (get result :output)))))