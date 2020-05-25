(ns aoc.day02
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(defn rownum
  "Returns button's row number"
  [button]
  (int (/ button 3.5)))

(deftest test-rownum
  (is (every? #(= 0 %) (map rownum [1 2 3])))
  (is (every? #(= 1 %) (map rownum [4 5 6])))
  (is (every? #(= 2 %) (map rownum [7 8 9]))))

(defn move
  "Returns the button you're on if you move dir from start"
  [start dir]
  (case dir
    :up    (if (< (- start 3) 1) start (- start 3))
    :down  (if (> (+ start 3) 9) start (+ start 3))
    :left  (if (or (< (dec start) 1)
                   (not= (rownum start) (rownum (dec start))))
             start
             (dec start))
    :right (if (or (> (inc start) 9)
                   (not= (rownum start) (rownum (inc start))))
             start
             (inc start))))

(deftest test-move
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


(defn move-many
  "Returns the button you're on if you move dirs from start"
  [start dirs]
  (reduce #(move %1 %2) start dirs))

(deftest test-move-many
  (is (= 1 (move-many 5 [:up :left :left])))
  (is (= 9 (move-many 1 [:right :right :down :down :down])))
  (is (= 8 (move-many 9 [:left :up :right :down :left])))
  (is (= 5 (move-many 8 [:up :up :up :up :down]))))

(defn follow-instructions
  "Returns buttons pressed when following instructions"
  [instructions]
  (rest (reductions #(move-many %1 %2) 5 instructions)))

(deftest test-follow-instructions
  (is (= [1 9 8 5]
         (follow-instructions
          [[:up :left :left]
           [:right :right :down :down]
           [:left :up :right :down :left]
           [:up :up :up :up :down]]))))

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

(defn part1
  []
  (-> (parse-input)
      (follow-instructions)))

(comment
  (part1))
