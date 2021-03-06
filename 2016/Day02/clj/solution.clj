(ns aoc.day02
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-all-tests]]))

(def part1-keypad
  {1 [0 0] 2 [0 1] 3 [0 2]
   4 [1 0] 5 [1 1] 6 [1 2]
   7 [2 0] 8 [2 1] 9 [2 2]})

(def part2-keypad
  {                  1 [0 2]
            2 [1 1]  3 [1 2]  4 [1 3]
   5 [2 0]  6 [2 1]  7 [2 2]  8 [2 3] 9 [2 4]
           \A [3 1] \B [3 2] \C [3 3]
                    \D [4 2]})

(def ^:dynamic *keypad*)

(defn rownum
  "Returns button's row number"
  [button]
  (-> (get *keypad* button)
      (first)))

(deftest test-rownum
  (binding [*keypad* part1-keypad]
    (is (every? #(= 0 %) (map rownum [1 2 3])))
    (is (every? #(= 1 %) (map rownum [4 5 6])))
    (is (every? #(= 2 %) (map rownum [7 8 9]))))
  (binding [*keypad* part2-keypad]
    (is (= 0 (rownum 1)))
    (is (every? #(= 1 %) (map rownum [2 3 4])))
    (is (every? #(= 2 %) (map rownum [5 6 7 8 9])))
    (is (every? #(= 3 %) (map rownum [\A \B \C])))
    (is (= 4 (rownum \D)))))

(defn colnum
  "Returns button's column number"
  [button]
  (-> (get *keypad* button)
      (second)))

(deftest test-colnum
  (binding [*keypad* part1-keypad]
    (is (every? #(= 0 %) (map colnum [1 4 7])))
    (is (every? #(= 1 %) (map colnum [2 5 8])))
    (is (every? #(= 2 %) (map colnum [3 6 9]))))
  (binding [*keypad* part2-keypad]
    (is (= 0 (colnum 5)))
    (is (every? #(= 1 %) (map colnum [2 6 \A])))
    (is (every? #(= 2 %) (map colnum [1 3 7 \B \D])))
    (is (every? #(= 3 %) (map colnum [4 8 \C])))
    (is (= 4 (colnum 9)))))

(defn adjust
  [pos dir]
  (let [[row col] pos]
    (case dir
      :up    [(dec row) col]
      :down  [(inc row) col]
      :left  [row (dec col)]
      :right [row (inc col)])))

(deftest test-adjust
  (is (= [0 1] (adjust [0 0] :right)))
  (is (= [9 9] (adjust [8 9] :down)))
  (is (= [3 0] (adjust [4 0] :up)))
  (is (= [1 1] (adjust [1 2] :left))))

(defn move
  "Returns the button you're on if you move dir from start"
  [start dir]
  (let [lookup   (set/map-invert *keypad*)
        startpos [(rownum start) (colnum start)]
        newpos   (adjust startpos dir)]
    (get lookup newpos (get lookup startpos))))

(deftest test-move
  (binding [*keypad* part1-keypad]
    (is (= 2 (move 5 :up)))
    (is (= 6 (move 9 :up)))
    (is (= 1 (move 1 :up)))
    (is (= 6 (move 3 :down)))
    (is (= 4 (move 1 :down)))
    (is (= 9 (move 6 :down)))
    (is (= 4 (move 5 :left)))
    (is (= 4 (move 4 :left)))
    (is (= 8 (move 9 :left)))
    (is (= 1 (move 1 :left)))
    (is (= 9 (move 9 :right)))
    (is (= 3 (move 2 :right))))
  (binding [*keypad* part2-keypad]
    (is (= 2 (move 6 :up)))
    (is (= \B (move \D :up)))
    (is (= 5 (move 5 :left)))
    (is (= 5 (move 5 :down)))
    (is (= 9 (move 9 :up)))))

(defn move-many
  "Returns the button you're on if you move dirs from start"
  [start dirs]
  (reduce #(move %1 %2) start dirs))

(deftest test-move-many
  (binding [*keypad* part1-keypad]
    (is (= 1 (move-many 5 [:up :left :left])))
    (is (= 9 (move-many 1 [:right :right :down :down :down])))
    (is (= 8 (move-many 9 [:left :up :right :down :left])))
    (is (= 5 (move-many 8 [:up :up :up :up :down]))))
  (binding [*keypad* part2-keypad]
    (is (= 5 (move-many 5 [:up :left :left])))
    (is (= \D (move-many 5 [:right :right :down :down :down])))
    (is (= \B (move-many \D [:left :up :right :down :left])))
    (is (= 3 (move-many \B [:up :up :up :up :down])))))

(defn follow-instructions
  "Returns buttons pressed when following instructions"
  [instructions]
  (rest (reductions #(move-many %1 %2) 5 instructions)))

(deftest test-follow-instructions
  (binding [*keypad* part1-keypad]
    (is (= [1 9 8 5]
           (follow-instructions
            [[:up :left :left]
             [:right :right :down :down]
             [:left :up :right :down :left]
             [:up :up :up :up :down]]))))
  (binding [*keypad* part2-keypad]
    (is (= [5 \D \B 3]
           (follow-instructions
            [[:up :left :left]
             [:right :right :down :down]
             [:left :up :right :down :left]
             [:up :up :up :up :down]])))))

(defn parse-char
  [c]
  (case c
    \U :up
    \D :down
    \L :left
    \R :right))

(defn parse-line
  [line]
  (->> (.toCharArray line)
       (map parse-char)))

(defn parse-input
  []
  (with-open [rdr (io/reader "../input.txt")]
    (doall (map parse-line (line-seq rdr)))))

(defn run
  []
  (doall
   (-> (parse-input)
       (follow-instructions))))

(defn part1
  []
  (binding [*keypad* part1-keypad]
    (run)))

(defn part2
  []
  (binding [*keypad* part2-keypad]
    (run)))

(comment
  (run-all-tests)
  (part1)
  (part2))
