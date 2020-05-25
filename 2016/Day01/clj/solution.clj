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

(defn walk-all
  "Returns every block visited along a walk"
  [coord dir steps]
  (reductions (fn [coord _] (walk coord dir 1))
              (walk coord dir 1)
              (range (dec steps))))

(deftest test-walk-all
  (is (= [[1 0] [2 0] [3 0]]
         (walk-all [0 0] :east 3)))
  (is (= [[1 1] [1 0]]
         (walk-all [1 2] :south 2))))

(defn move
  [pos instr]
  (let [[dir coord] pos
        [lr steps]  instr
        new-dir     (turn dir lr)
        new-coord   (walk coord new-dir steps)]
    [new-dir new-coord]))

(deftest test-move
  (is (= [:east [5 0]] (move startpos [:right 5]))))

(defn move-all
  "Returns tuple of new-pos and all visited blocks"
  [pos instr]
  (let [[dir coord] pos
        [lr steps]  instr
        new-dir     (turn dir lr)
        new-coords  (walk-all coord new-dir steps)]
    [[new-dir (last new-coords)] new-coords]))

(deftest test-move-all
  (is (= [[:east [2 0]] [[0 0] [1 0] [2 0]]]
         (move-all startpos [:right 2]))))

(defn blocks-away
  [pos]
  (apply + (map #(Math/abs %1) pos)))

(defn first-visited
  "Returns the first coord in coords already in visited, or nil"
  [visited coords]
  (some visited coords))

(deftest test-first-visited
  (is (= [0 1]
         (first-visited #{[0 0] [0 1] [0 2]}
                        [[1 2] [0 1] [-1 1]])))
  (is (= nil
         (first-visited #{[0 0] [0 1] [0 2]}
                        [[1 1] [1 2] [1 3]]))))

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
        [dir coord] end]
    (blocks-away coord)))

(deftest test-part1-examples
  (is (= 5 (part1 "R2, L3")))
  (is (= 2 (part1 "R2, R2, R2")))
  (is (= 12 (part1 "R5, L5, R5, R3"))))

(defn part2
  [input]
  (loop [visited #{(second startpos)}
         instrs  (parse-input input)
         pos     startpos]
    (if-let [instr (first instrs)]
      (let [[new-pos coords] (move-all pos instr)]
        (if-let [v (first-visited visited coords)]
          (blocks-away v)
          (recur (into visited coords)
                 (next instrs)
                 new-pos))))))

(deftest test-part2-examples
  (is (= 4 (part2 "R8, R4, R4, R8"))))

(comment
  (part1 (str/trim (slurp "../input.txt")))
  (part2 (str/trim (slurp "../input.txt"))))
