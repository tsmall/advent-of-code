(ns aoc.day01
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def startpos
  [:north [0 0]])

(defn turn
  [dir lr]
  (case [dir lr]
    [:north :left]  :west
    [:north :right] :east
    [:east  :left]  :north
    [:east  :right] :south
    [:south :left]  :east
    [:south :right] :west
    [:west  :left]  :south
    [:west  :right] :north))

(deftest test-turning
  (is (= :west (turn :north :left)))
  (is (= :south (turn :east :right))))

(defn walk
  [coord dir steps]
  (let [[x y] coord]
    (case dir
      :north [x (+ y steps)]
      :east  [(+ x steps) y]
      :south [x (- y steps)]
      :west  [(- x steps) y])))

(defn move
  [pos instr]
  (let [[dir coord] pos
        [lr steps]  instr
        new-dir     (turn dir lr)
        new-coord   (walk coord new-dir steps)]
    [new-dir new-coord]))

(deftest test-move
  (is (= [:east [5 0]] (move startpos [:right 5]))))

(defn blocks-away
  [pos]
  (apply + (map #(Math/abs %1) pos)))

(defn parse
  [s]
  (let [dir (case (.substring s 0 1)
              "L" :left
              "R" :right)
        steps (Integer/parseInt (.substring s 1))]
    [dir steps]))

(deftest test-parse
  (is (= [:right 2] (parse "R2")))
  (is (= [:right 5] (parse "R5")))
  (is (= [:left 3] (parse "L3")))
  (is (= [:left 12] (parse "L12"))))

(defn parse-input
  [s]
  (->> (str/split s #", ")
       (map parse)))

(deftest test-parse-input
  (is (= [[:right 2] [:left 3]]
         (parse-input "R2, L3")))
  (is (= [[:right 2] [:right 2] [:right 2]]
         (parse-input "R2, R2, R2")))
  (is (= [[:left 10] [:right 42]]
         (parse-input "L10, R42"))))

(defn part1
  [input]
  (let [end (reduce #(move %1 %2)
                    startpos
                    (parse-input input))
        [dir pos] end]
    (blocks-away pos)))

(deftest test-part1-examples
  (is (= 5 (part1 "R2, L3")))
  (is (= 2 (part1 "R2, R2, R2")))
  (is (= 12 (part1 "R5, L5, R5, R3"))))

(comment
  (part1 (str/trim (slurp "../input.txt"))))
