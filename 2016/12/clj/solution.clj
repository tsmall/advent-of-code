(ns aoc.day11
  (:require [clojure.string :as str]))

(def cpu
  {:pc 0
   :a 0
   :b 0
   :c 0
   :d 0})

(defmulti exec (fn [cpu inst] (first inst)))

(defmethod exec :cpy
  [cpu [_ n r]]
  (let [val (if (number? n) n (get cpu n))]
    (-> cpu
        (update :pc inc)
        (assoc r val))))

(defmethod exec :inc
  [cpu [_ r]]
  (-> cpu
      (update :pc inc)
      (update r inc)))

(defmethod exec :dec
  [cpu [_ r]]
  (-> cpu
      (update :pc inc)
      (update r dec)))

(defmethod exec :jnz
  [cpu [_ r n]]
  (let [v (if (number? r) r (get cpu r))]
    (if (zero? v)
      (update cpu :pc inc)
      (update cpu :pc #(+ % n)))))

(defn run
  [cpu program]
  (loop [cpu cpu]
    (if-let [inst (get program (:pc cpu))]
      (recur (exec cpu inst))
      cpu)))

(defn parse-num-or-keyword
  [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException _ (keyword s))))

(defn parse-line
  [s]
  (->> (str/split s #" ")
       (map parse-num-or-keyword)
       (into [])))

(defn parse
  [s]
  (into [] (map parse-line (str/split-lines s))))

(defn part1
  [f]
  (->> (slurp f)
       (parse)
       (run cpu)
       :a))

(defn part2
  [f]
  (let [cpu (assoc cpu :c 1)]
    (->> (slurp f)
         (parse)
         (run cpu)
         :a)))

(comment
  (part1 "../example.txt")
  (part1 "../input.txt")
  (part2 "../input.txt")
  )
