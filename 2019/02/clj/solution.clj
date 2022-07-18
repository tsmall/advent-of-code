(ns solution
  (:require [cpu :as cpu]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]))

(def input
  (slurp "../input.txt"))

(defn parse
  [input]
  (->> (str/split input #",")
       (map #(Integer/parseInt (str/trim %)))
       vec))

(defn part1
  [input]
  (-> input
      parse
      (cpu/set-input 12 2)
      cpu/run
      (get 0)))

(defn part2
  [input]
  (let [mem (parse input)]
    (first
     (for [noun (range 100)
           verb (range 100)
           :let [program (cpu/set-input mem noun verb)
                 result  (cpu/run program)]
           :when (= (get result 0) 19690720)]
       (+ (* 100 noun) verb)))))

(deftest answers
  (is (= 3409710 (part1 input)))
  (is (= 7912    (part2 input))))

(comment
  (run-tests 'aoc.day02)
  (time (part1 input))
  (time (part2 input))
  )
