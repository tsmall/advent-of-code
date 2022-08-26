(ns aoc.y2016.day16
  (:require [clojure.string :as str]))

(defn dragon-curve-step
  [data]
  (let [a data
        b (->> data
               reverse
               (map {\0 \1, \1, \0}))]
    (concat a [\0] b)))

(defn dragon-curve
  [start-data desired-size]
  (loop [data start-data]
    (if (>= (count data) desired-size)
      (take desired-size data)
      (recur (dragon-curve-step data)))))

(defn checksum-pair
  [[x y]]
  (if (= x y)
    1
    0))

(defn checksum-step
  [data]
  (->> data
       (partition 2)
       (map checksum-pair)))

(defn checksum
  [data]
  (loop [data (checksum-step data)]
    (if (odd? (count data))
      data
      (recur (checksum-step data)))))

(defn generate
  [input disk-size]
  (-> input
      (dragon-curve disk-size)
      (checksum)))

(def input
  "01000100010010111")

(defn part1
  []
  (-> input
      (generate 272)
      (str/join)))

(defn part2
  []
  (-> input
      (generate 35651584)
      (str/join)))

(comment
  (time (part1))
  (time (part2))
  )
