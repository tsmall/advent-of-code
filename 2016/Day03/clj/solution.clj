(ns aoc.day03
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-all-tests]]))

(defn valid?
  "Returns true if triangle with sides [a b c] is valid"
  [triangle]
  (every? (fn [[a b c]] (> (+ a b) c))
          (combo/permutations triangle)))

(defn parse-line
  [line]
  (as-> line _
    (str/trim _)
    (str/split _ #"\s+")
    (map #(Integer/parseInt %) _)))

(defn part1
  []
  (with-open [rdr (io/reader "../input.txt")]
    (->> (line-seq rdr)
         (map parse-line)
         (map valid?)
         (filter identity)
         (count))))

(comment
  (part1))
