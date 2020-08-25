(ns aoc.day06
  (:require [clojure.string :as str]))


(defn transpose
  [m]
  (apply mapv vector m))


(defn most-frequent
  [frequencies]
  (->> (seq frequencies)
       (sort-by second >)
       (ffirst)))


(defn corrected
  "Returns a string, the error-corrected message recovered
  from the sequence of messages sent using repetition code."
  [messages]
  (->> messages
       (map #(str/split %1 #""))
       (transpose)
       (map frequencies)
       (map most-frequent)
       (str/join)))


(defn part1
  ([]
   (part1 "../input.txt"))
  ([f]
   (-> (slurp f)
       (str/split-lines)
       (corrected))))


(comment
  (part1)  ; "mshjnduc"
  )
