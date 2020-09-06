(ns aoc.day08
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-all-tests]]))


(defn parse-rect
  [words]
  (let [[w h] (str/split (first words) #"x")]
    {:command :rect
     :width (Integer/parseInt w)
     :height (Integer/parseInt h)}))


(defn parse-rotate
  [words]
  (let [[kind eql _ amount] words
        [_ index] (str/split eql #"=")]
    {:command :rotate
     :kind (keyword kind)
     :index (Integer/parseInt index)
     :amount (Integer/parseInt amount)}))


(defn parse
  [line]
  (let [words (str/split line #" ")]
    (case (first words)
      "rect" (parse-rect (rest words))
      "rotate" (parse-rotate (rest words)))))


(deftest test-parse
  (is (= {:command :rect :width 3 :height 2}
         (parse "rect 3x2")))
  (is (= {:command :rect :width 2 :height 3}
         (parse "rect 2x3")))
  (is (= {:command :rotate :kind :column :index 1 :amount 2}
         (parse "rotate column x=1 by 2")))
  (is (= {:command :rotate :kind :column :index 2 :amount 1}
         (parse "rotate column x=2 by 1")))
  (is (= {:command :rotate :kind :row :index 20 :amount 100}
         (parse "rotate row y=20 by 100"))))


(def ^:dynamic *column-count* 50)
(def ^:dynamic *row-count* 6)


(def initial-screen
  #{})


(defmulti apply-operation :command)


(defmethod apply-operation :rect
  [op screen]
  (let [pixels (for [x (range (:width op))
                     y (range (:height op))]
                 [x y])]
    (apply conj screen pixels)))


(defn find-column
  [screen index]
  (filter #(= (get %1 0) index) screen))


(defn find-row
  [screen index]
  (filter #(= (get %1 1) index) screen))


(defn rotate-column
  [screen index amount]
  (let [original (find-column screen index)
        rotated (map (fn [[x y]] (vector x (mod (+ y amount) *row-count*)))
                     original)]
    (apply conj (apply disj screen original) rotated)))


(defn rotate-row
  [screen index amount]
  (let [original (find-row screen index)
        rotated (map (fn [[x y]] (vector (mod (+ x amount) *column-count*) y))
                     original)]
    (apply conj (apply disj screen original) rotated)))


(deftest test-rotation
  (let [init #{[0 0] [0 1] [1 0] [1 1]}]
    (is (= #{[0 0] [0 1] [1 1] [1 2]}
           (rotate-column init 1 1)))
    (is (= #{[1 0] [2 0] [0 1] [1 1]}
           (rotate-row init 0 1)))
    (binding [*row-count* 3
              *column-count* 3]
      (is (= init
             (rotate-row init 1 3)))
      (is (= #{[0 0] [2 0] [0 1] [1 1]}
             (rotate-row init 0 2))))))


(defmethod apply-operation :rotate
  [op screen]
  (case (:kind op)
    :column (rotate-column screen (:index op) (:amount op))
    :row (rotate-row screen (:index op) (:amount op))))


(defn char-at
  [screen point]
  (if (contains? screen point)
    \#
    \.))


(defn draw
  [screen]
  (doseq [y (range *row-count*)]
    (doseq [x (range *column-count*)]
      (print (char-at screen [x y])))
    (println)))


(defn part1
  []
  (->> (slurp "../input.txt")
       (str/split-lines)
       (map parse)
       (reduce #(apply-operation %2 %1) initial-screen)
       (count)))


(defn part2
  []
  (->> (slurp "../input.txt")
       (str/split-lines)
       (map parse)
       (reduce #(apply-operation %2 %1) initial-screen)
       (draw)))


(comment
  (run-all-tests)
  (part1)
  (part2)
  )
