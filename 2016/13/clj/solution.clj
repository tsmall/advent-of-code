(ns aoc.day13
  (:require [clojure.string :as str]))

(defn input
  "Reads the program's input from file f."
  [f]
  (-> (slurp f)
      (str/trim)
      Integer/parseInt))

(defn bit-count
  "Returns the number of 1 bits in integer x."
  [x]
  (let [xs (take-while #(> % 0)
                       (iterate #(bit-shift-right % 1) x))]
    (reduce (fn [c x] (+ c (bit-and x 1)))
            0
            xs)))

(defn coord-type
  "Returns whether the given xy coordinate is a :wall or :open. Both
   x and y must be positive integers."
  [input [x y]]
  (let [n (+ (* x x) (* 3 x) (* 2 x y) y (* y y))
        c (bit-count (+ n input))]
    (if (even? c)
      :open
      :wall)))

(defn print-grid
  "Prints the grid from 0,0 to x,y."
  [input [x y]]
  (dotimes [y (inc y)]
    (dotimes [x (inc x)]
      (print (case (coord-type input [x y]) :open "." :wall "#")))
    (print "\n")))

(defn all-open-spaces
  "Returns a set of all open spaces in the grid from 0,0 to x,y."
  [input [x y]]
  (into #{} (for [y (range 0 (inc y))
                  x (range 0 (inc x))
                  :when (= :open (coord-type input [x y]))]
              [x y])))

(defn neighbors
  "Returns all neighbor coordinates of x,y in grid, not including diagonals."
  [grid [x y :as coord]]
  (for [xd (range -1 2)
        yd (range -1 2)
        :let [[x' y' :as coord'] [(+ x xd) (+ y yd)]]
        :when (and (>= x' 0)
                   (>= y' 0)
                   (or (= x x') (= y y'))
                   (not= coord' coord)
                   (contains? grid coord'))]
    coord'))

(defn update-path
  [u q dist prev]
  (reduce (fn [[dist prev] v]
            (let [alt (inc (get dist u Integer/MAX_VALUE))]
              (if (< alt (get dist v Integer/MAX_VALUE))
                [(assoc dist v alt) (assoc prev v u)]
                [dist prev])))
          [dist prev]
          (neighbors q u)))

(defn find-path
  "Returns the shortest path in grid from src, using Dijkstra's algorithm.
   Stops when dest is reached, if provided."
  [grid src & dest]
  (loop [u    src
         q    (disj grid src)
         dist {src 0}
         prev {}]
    (if (or (empty? q) (and (not (nil? dest)) (= u dest)))
      [dist prev]
      (let [[dist' prev'] (update-path u q dist prev)
            u' (apply min-key #(get dist' % Integer/MAX_VALUE) q)
            q' (disj q u')]
        (recur u'
               q'
               dist'
               prev')))))

(defn shortest-path
  "Returns the fewest number of steps required to navigate grid
   from 0,0 to dest, using Dijkstra's algorithm."
  [grid dest]
  (let [src [1 1]
        [dist prev] (find-path grid src dest)]
    (loop [n 0
           u dest]
      (if (= u [1 1])
        n
        (recur (inc n)
               (get prev u))))))

(defn possible-locations
  "Returns the number of locations in grid starting from src
   and reachable in at most n steps, using Dijkstra's algorithm."
  [grid src n]
  (let [[dist prev_] (find-path grid src)]
    (->> (vals dist)
         (filter #(<= % n))
         (count))))

(defn part1
  ([]
   (part1 "../input.txt"))
  ([f]
   (let [grid (all-open-spaces (input f) [40 40])]
     (shortest-path grid [31 39]))))

(defn part2
  ([]
   (part2 "../input.txt"))
  ([f]
   (let [grid (all-open-spaces (input f) [100 100])]
     (possible-locations grid [1 1] 50))))

(comment
  (part1)
  (part2)
  )
