(ns aoc19.core
  (:gen-class))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(defn cost [curr start end]
  (let [g (manhattan-distance start curr)
        h (manhattan-distance curr end)
        f (+ g h)]
    [f g h]))

(defn edges [map width height closed [x y]]
  (for [tx (range (- x 1) (+ x 2))
        ty (range (- y 1) (+ y 2))
        :when (and (>= tx 0)
                   (>= ty 0)
                   (<= tx width)
                   (<= ty height)
                   (not= [x y] [tx ty])
                   (not= (nth (nth map ty) tx) 1)
                   (not (contains? closed [tx ty])))]
    [tx ty]))

(defn path [end parent closed]
  (reverse
   (loop [path [end parent]
          node (closed parent)]
     (if (nil? node)
       path
       (recur (conj path node) (closed node))))))