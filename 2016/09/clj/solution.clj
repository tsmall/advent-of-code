(ns aoc.day09
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-all-tests]]))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defmulti next-token
  first)

(defmethod next-token \(
  [s]
  (let [marker (re-find #"\(\d+x\d+\)" s)
        [chars mult] (map parse-int (re-seq #"\d+" marker))
        token {:type :marker
               :chars chars
               :multiplier mult
               :value (subs s (count marker) (+ (count marker) chars))}]
    [token (subs s (+ chars (count marker)))]))

(defmethod next-token nil
  [s]
  [nil s])

(declare decompressed-length)

(defmethod next-token :default
  [s]
  (let [t (re-find #"[^\(]+" s)
        token {:type :chars
               :chars (count t)}]
    [token (subs s (count t))]))

(defmulti token-count
  (fn [token version] (:type token)))

(defmethod token-count :marker
  [token version]
  (case version
    :v1 (* (:multiplier token) (:chars token))
    :v2 (* (:multiplier token) (decompressed-length (:value token) version))))

(defmethod token-count :chars
  [token version]
  (:chars token))

(defn decompressed-length
  [s version]
  (loop [n 0, s s]
    (let [[token s] (next-token s)]
      (if (nil? token)
        n
        (recur
         (+ n (token-count token version))
         s)))))

(deftest part1-examples
  (is (= 6 (decompressed-length "ADVENT" :v1)))
  (is (= 7 (decompressed-length "A(1x5)BC" :v1)))
  (is (= 9 (decompressed-length "(3x3)XYZ" :v1)))
  (is (= 6 (decompressed-length "(6x1)(1x3)A" :v1)))
  (is (= 11 (decompressed-length "A(2x2)BCD(2x2)EFG" :v1)))
  (is (= 18 (decompressed-length "X(8x2)(3x3)ABCY" :v1))))

(defn part-1
  []
  (-> (slurp "../input.txt")
      (str/trim)
      (decompressed-length :v1)))

(deftest part1
  (is (= 74532 (part-1))))

(deftest part2-examples
  (is (= 9 (decompressed-length "(3x3)XYZ" :v2)))
  (is (= 20 (decompressed-length "X(8x2)(3x3)ABCY" :v2)))
  (is (= 241920 (decompressed-length "(27x12)(20x12)(13x14)(7x10)(1x12)A" :v2)))
  (is (= 445 (decompressed-length "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" :v2))))

(defn part-2
  []
  (-> (slurp "../input.txt")
      (str/trim)
      (decompressed-length :v2)))

(deftest part2
  (is (= 11558231665 (part-2))))

(comment
  (run-all-tests)
  (part-1)
  (part-2)
  )
