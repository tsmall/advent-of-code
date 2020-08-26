(ns aoc.day07
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-all-tests]]))


(defn abba?
  [xs]
  (let [[x y] (split-at 2 xs)]
    (and (< 1 (count (set xs)))
         (= x (reverse y)))))


(defn contains-abba?
  [s]
  (some abba? (partition 4 1 s)))


(defn categorize
  [match]
  (if (nil? (first match))
    :hyper
    :safe))


(defn parse
  [s]
  (let [re #"([a-z]+)|\[([a-z]+)\]"
        m (->> (re-seq re s)
               (map (partial drop 1))
               (group-by categorize))]
    {:safe (mapcat (partial take 1) (:safe m))
     :hyper (mapcat (partial drop 1) (:hyper m))}))


(deftest test-parse
  (is (= {:safe ["abba" "qrst"], :hyper ["mnop"]}
         (parse "abba[mnop]qrst")))
  (is (= {:safe ["abba" "fghi" "nopq"], :hyper ["cdde" "jklm"]}
         (parse "abba[cdde]fghi[jklm]nopq"))))


(defn supports-tls?
  [s]
  (let [{:keys [safe hyper]} (parse s)]
    (and (not (some contains-abba? hyper))
         (some contains-abba? safe))))


(deftest test-supports-tls?
  (is (supports-tls? "abba[mnop]qrst"))
  (is (not (supports-tls? "abcd[bddb]xyyx")))
  (is (not (supports-tls? "aaaa[qwer]tyui")))
  (is (supports-tls? "ioxxoj[asdfgh]zxcvbn"))
  (is (supports-tls? "abcd[jklm]oxxo"))
  (is (supports-tls? "abba[cdde]fghi[jklm]nopq")))


(defn part1
  ([]
   (part1 (slurp "../input.txt")))
  ([s]
   (->> (str/split-lines s)
        (filter supports-tls?)
        (count))))


(deftest test-part1
  (let [lines ["abba[mnop]qrst"
               "abcd[bddb]xyyx"
               "aaaa[qwer]tyui"
               "ioxxoj[asdfgh]zxcvbn"]
        s (str/join "\n" lines)]
    (is (= 2 (part1 s)))))


(comment
  (run-all-tests)
  (part1)
  )
