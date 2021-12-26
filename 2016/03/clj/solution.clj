(ns aoc.day03
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn valid?
  "Returns true if triangle with sides [a b c] is valid"
  [triangle]
  (every? (fn [[a b c]] (> (+ a b) c))
          (combo/permutations triangle)))

(defn rows->cols
  "Converts seq of rows into seq of columns"
  [rows]
  (apply map vector rows))

(defn column-triangles
  [rows]
  (let [cols  (rows->cols rows)
        split (mapcat (partial partition 3) cols)]
    split))

(defn parse-line
  [line]
  (as-> line _
    (str/trim _)
    (str/split _ #"\s+")
    (map #(Integer/parseInt %) _)))

(defn parse-input
  [input]
  (map parse-line (str/split-lines input)))

(defn part1
  []
  (->> (slurp "../input.txt")
       (parse-input)
       (map valid?)
       (filter identity)
       (count)))

(defn part2
  []
  (->> (slurp "../input.txt")
       (parse-input)
       (column-triangles)
       (map valid?)
       (filter identity)
       (count)))

(comment
  (part1)
  (part2))
