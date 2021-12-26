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


(defn aba?
  [xs]
  (let [[a b c] xs]
    (and (= a c)
         (not= a b))))


(defn bab?
  [aba xs]
  (let [[a b c] aba
        [x y z] xs]
    (and (aba? xs)
         (= a y)
         (= b x))))


(deftest test-bab?
  (is (bab? "aba" "bab"))
  (is (bab? "xyx" "yxy"))
  (is (not (bab? "xyx" "xyx")))
  (is (not (bab? "aaa" "aaa"))))


(defn abas
  [s]
  (filter aba? (partition 3 1 s)))


(defn contains-bab?
  [aba s]
  (some (partial bab? aba) (partition 3 1 s)))


(deftest test-contains-bab?
  (is (contains-bab? "aba" "cdbabef"))
  (is (contains-bab? "bzb" "zazbz")))


(defn categorize
  [match]
  (if (nil? (first match))
    :hypernet
    :supernet))


(defn parse
  [s]
  (let [re #"([a-z]+)|\[([a-z]+)\]"
        m (->> (re-seq re s)
               (map (partial drop 1))
               (group-by categorize))]
    {:supernet (mapcat (partial take 1) (:supernet m))
     :hypernet (mapcat (partial drop 1) (:hypernet m))}))


(deftest test-parse
  (is (= {:supernet ["abba" "qrst"], :hypernet ["mnop"]}
         (parse "abba[mnop]qrst")))
  (is (= {:supernet ["abba" "fghi" "nopq"], :hypernet ["cdde" "jklm"]}
         (parse "abba[cdde]fghi[jklm]nopq"))))


(defn supports-tls?
  [s]
  (let [{:keys [supernet hypernet]} (parse s)]
    (and (not (some contains-abba? hypernet))
         (some contains-abba? supernet))))


(deftest test-supports-tls?
  (is (supports-tls? "abba[mnop]qrst"))
  (is (not (supports-tls? "abcd[bddb]xyyx")))
  (is (not (supports-tls? "aaaa[qwer]tyui")))
  (is (supports-tls? "ioxxoj[asdfgh]zxcvbn"))
  (is (supports-tls? "abcd[jklm]oxxo"))
  (is (supports-tls? "abba[cdde]fghi[jklm]nopq")))


(defn supports-ssl?
  [s]
  (let [{:keys [supernet hypernet]} (parse s)
        abas (mapcat abas supernet)]
    (some identity
     (for [aba abas, s hypernet]
       (contains-bab? aba s)))))


(deftest test-supports-ssl?
  (is (supports-ssl? "aba[bab]xyz"))
  (is (not (supports-ssl? "xyx[xyx]xyx")))
  (is (supports-ssl? "aaa[kek]eke"))
  (is (supports-ssl? "zazbz[bzb]cdb")))


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


(defn part2
  ([]
   (part2 (slurp "../input.txt")))
  ([s]
   (->> (str/split-lines s)
        (filter supports-ssl?)
        (count))))


(deftest test-part2
  (let [lines ["aba[bab]xyz"
               "xyx[xyx]xyx"
               "aaa[kek]eke"
               "zazbz[bzb]cdb"
               "aba[xyz]ded[ede]fgh"]
        s (str/join "\n" lines)]
    (is (= 4 (part2 s)))))


(comment
  (run-all-tests)
  (part1)
  (part2)
  )
