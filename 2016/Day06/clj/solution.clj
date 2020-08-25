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


(defn least-frequent
  [frequencies]
  (->> (seq frequencies)
       (sort-by second <)
       (ffirst)))


(defn corrected
  "Returns a string, the error-corrected message recovered
  from the sequence of messages sent using repetition code."
  [messages code-fn]
  (->> messages
       (map #(str/split %1 #""))
       (transpose)
       (map frequencies)
       (map code-fn)
       (str/join)))


(defn part1
  ([]
   (part1 "../input.txt"))
  ([f]
   (-> (slurp f)
       (str/split-lines)
       (corrected most-frequent))))


(defn part2
  ([]
   (part2 "../input.txt"))
  ([f]
   (-> (slurp f)
       (str/split-lines)
       (corrected least-frequent))))


(comment
  (part1)  ; "mshjnduc"
  (part2)  ; "apfeeebz"
  )
