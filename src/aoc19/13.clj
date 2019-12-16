(ns aoc19.13
  (:gen-class)
  (:require [aoc19.core :refer [replace-value print-2d-array]]
            [aoc19.intcode :refer [run-program load-program]]
            [clojure.core.async :refer [<!! go-loop chan dropping-buffer put! close! take!]]
            [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.util :as zutil]
            [clojure.string :as string])
  (:import (zaffre.font CompositeFont)))

(def core-program (load-program "day13.txt"))

(defn to-tile
  [type]
  (case type
    0 \space
    1 \#
    2 \|
    3 \_
    4 \O))

(defn add-tile-to-map
  [score map x y type]
  (if (and (= -1 x) (= y 0))
    [(int type) map]
    [score (replace-value y (replace-value x (to-tile type) (nth map y)) map)]))

(defn create-game
  [map in out]
  (go-loop [score 0 map map]
    (let [x (<!! in) y (<!! in) type (<!! in)]
      (if (and (number? x) (number? y) (number? type))
        (let [[scr nm] (add-tile-to-map score map x y type)] (put! out [scr nm]) (recur scr nm))
        [score map]))))

(defn build-map
  [xrange yrange]
  (vec (for [y (range yrange)]
         (vec (for [x (range xrange)] \space)))))


(def font (CompositeFont. [ztiles/pastiche-16x16
                           ztiles/fantasy]))

(defn convert-to-movement
  [character]
  (case character
    \a -1
    \s 0
    \d 1
    0))

(defn draw-map
  [terminal map row offset]
  (if (= (count map) row)
    nil
    (do (zutil/put-string terminal :text 2 (+ offset row) (string/join (nth map row))) (recur terminal map (inc row) offset))))

(defn play-game
  [input state]
  (zgl/create-terminal
   [{:id :app
     :layers [:text]
     :columns 48
     :rows 48
     :pos [0 0]
     :font (constantly font)}]
   {:title "Day 13"
    :screen-width (* 48 16)
    :screen-height (* 48 16)}
   (fn [terminal]
       (zat/do-frame terminal 33
                     (let [[score map] (<!! state)]  
                       (zutil/put-string terminal :text 0 0 "Score")
                       (zutil/put-string terminal :text 7 0 (str score))
                       (draw-map terminal map 0 3)))
       (zevents/add-event-listener terminal :keypress
                                   (fn [new-key]
                                     (case new-key
                                       \q (do (zat/destroy! terminal) (close! state) (close! input))
                                       (put! input (convert-to-movement new-key))))))))

(def part1 (println
            (frequencies
             (flatten
              (second
               (let [[in out] (run-program core-program 0 0) state (chan (dropping-buffer 1))]
                 (<!! (create-game (build-map 23 42) out state))))))))

(def part2 (let [[in out] (run-program (replace-value 0 2 core-program) 0 0) state (chan (dropping-buffer 1))]
             (do
               (create-game (build-map 23 42) out state)
               (play-game in state))))

(defn -main
  []
  (do part1 part2))