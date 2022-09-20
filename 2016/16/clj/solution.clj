(ns aoc.y2016.day16
  (:require [clojure.string :as str])
  (:import [clojure.lang IPersistentVector]))

(def flipped
  {\0 \1, \1 \0})

(defn dragon-curve-step
  [data]
  (loop [i   (dec (count data))
         res (conj! data \0)]
    (if (< i 0)
      res
      (recur (dec i)
             (conj! res (flipped (nth data i)))))))

(defn dragon-curve
  [start-data desired-size]
  (loop [data (transient start-data)]
    (if (>= (count data) desired-size)
      (take desired-size (persistent! data))
      (recur (dragon-curve-step data)))))

(defn checksum-pair
  [^IPersistentVector pair]
  (let [x (.nth pair 0)
        y (.nth pair 1)]
   (if (.equals x y)
     1
     0)))

(defn checksum-step
  [data]
  (into []
        (comp
         (partition-all 2)
         (map checksum-pair))
        data))

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
  (vec "01000100010010111"))

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
