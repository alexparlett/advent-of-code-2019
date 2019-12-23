(ns aoc19.intcode
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [aoc19.core :refer [load-file-as-string replace-value]])
  (:require [clojure.core.async :as a :refer [>! <! >!! <!! go chan go-loop close!]]))

(defn param-to-pointer
  [param]
  (case param
    :c 1
    :b 2
    :a 3))

(defn get-pointer
  [param operation program relative-offset]
  (let [param-pointer (param-to-pointer param) mode (get operation param)]
    (case mode
      0 (nth program (+ (operation :pointer) param-pointer))
      1 (+ (operation :pointer) param-pointer)
      2 (+ (nth program (+ (operation :pointer) param-pointer)) relative-offset))))

(defn get-value
  [param operation program relative-offset]
  (let [param-pointer (param-to-pointer param) mode (get operation param)]
    (case mode
      0 (nth program (get-pointer param operation program relative-offset))
      1 (nth program (get-pointer param operation program relative-offset))
      2 (nth program (get-pointer param operation program relative-offset)))))

(defn code-add
  [program operation relative-offset]
  (replace-value (get-pointer :a operation program relative-offset) (+ (get-value :c operation program relative-offset) (get-value :b operation program relative-offset)) program))

(defn code-mul
  [program operation relative-offset]
  (replace-value (get-pointer :a operation program relative-offset) (* (get-value :c operation program relative-offset) (get-value :b operation program relative-offset)) program))

(defn code-input
  [program operation input relative-offset]
  (replace-value (get-pointer :c operation program relative-offset) input program))

(defn code-output
  [program operation relative-offset]
  (get-value :c operation program relative-offset))

(defn code-relative
  [program operation relative-offset]
  (+ relative-offset (get-value :c operation program relative-offset)))

(defn code-jump-false
  [operation program relative-offset]
  (if (= 0 (get-value :c operation program relative-offset)) (get-value :b operation program relative-offset) (+ 3 (operation :pointer))))

(defn code-jump-true
  [operation program relative-offset]
  (if (not= 0 (get-value :c operation program relative-offset)) (get-value :b operation program relative-offset) (+ 3 (operation :pointer))))

(defn code-less
  [operation program relative-offset]
  (let [value (if (< (get-value :c operation program relative-offset) (get-value :b operation program relative-offset)) 1 0)]
    (replace-value (get-pointer :a operation program relative-offset) value program)))

(defn code-equal
  [operation program relative-offset]
  (let [value (if (= (get-value :c operation program relative-offset) (get-value :b operation program relative-offset)) 1 0)]
    (replace-value (get-pointer :a operation program relative-offset) value program)))

(defn digits
  [number]
  (map #(Character/digit % 10) (format "%05d" number)))

(defn get-op-code
  [digits]
  (Integer/parseInt (string/join (take-last 2 digits))))

(defn get-param-mode
  [digits pointer]
  (Integer/parseInt (str (nth digits pointer \0))))

(defn get-operation
  [program pointer]
  (let [digits (digits (nth program pointer))]
    {:de (get-op-code digits) :c (get-param-mode digits 2) :b (get-param-mode digits 1) :a (get-param-mode digits 0) :pointer pointer}))

(defn apply-code
  [{program :program pointer :pointer relative-offset :relative-offset} pin pout mode]
  (let [operation (get-operation program pointer)]
    (case (operation :de)
      1 {:pointer (+ 4 pointer) :program (code-add program operation relative-offset) :relative-offset relative-offset :halt false}
      2 {:pointer (+ 4 pointer) :program (code-mul program operation relative-offset) :relative-offset relative-offset :halt false}
      3 (let [input (<!! pin)]
          (if (= :halt input)
            {:pointer (+ 2 pointer) :program program :relative-offset relative-offset :halt true}
            {:pointer (+ 2 pointer) :program (code-input program operation input relative-offset) :relative-offset relative-offset :halt false}))
      4 (let [output (code-output program operation relative-offset)]
          (>!! pout output)
          (if (= mode :immediate)
            {:pointer (+ 2 pointer) :program program :relative-offset relative-offset  :halt true}
            {:pointer (+ 2 pointer) :program program :relative-offset relative-offset :halt false}))
      5 {:pointer (code-jump-true operation program relative-offset) :program program :relative-offset relative-offset :halt false}
      6 {:pointer (code-jump-false operation program relative-offset) :program program :relative-offset relative-offset :halt false}
      7 {:pointer (+ 4 pointer) :program (code-less operation program relative-offset)  :relative-offset relative-offset :halt false}
      8 {:pointer (+ 4 pointer) :program (code-equal operation program relative-offset) :relative-offset relative-offset} :halt false
      9 {:pointer (+ 2 pointer) :program program :relative-offset (code-relative program operation relative-offset) :halt false}
      99 {:pointer :done :program program :relative-offset relative-offset :halt true})))

(defn run-program
  ([program]
   (run-program program 0 0 :async))
  ([program pointer relative-offset mode]
   (let [pin (chan) pout (chan) state (atom {:pointer pointer :program program :relative-offset relative-offset})]
     (go-loop []
       (let [result (apply-code @state pin pout mode)]
         (reset! state result)
         (if (result :halt)
           (do (close! pin) (>! pout result) (close! pout))
           (recur))))
     [pin pout state])))

(defn load-program
  [file]
  (let [program (map #(Long/parseLong %) (string/split (load-file-as-string file) #","))]
    (vec (concat program (repeat (- 4096 (count program)) 0)))))