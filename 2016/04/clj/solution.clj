(ns aoc.day04
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-all-tests]]))

(defn by-frequencies
  [[char1 freq1] [char2 freq2]]
  (cond
    (> freq1       freq2)       -1
    (< freq1       freq2)        1
    (> (int char1) (int char2))  1
    (< (int char1) (int char2)) -1
    :else                        0))

(defn checksum
  [name]
  (->> (str/replace name "-" "")
       (frequencies)
       (sort by-frequencies)
       (take 5)
       (map first)
       (str/join)))

(deftest checksum-tests
  (is (= "abxyz" (checksum "aaaaa-bbb-z-y-x")))
  (is (= "abcde" (checksum "a-b-c-d-e-f-g-h")))
  (is (= "oarel" (checksum "not-a-real-room")))
  (is (not= "decoy" (checksum "totally-real-room"))))

(defn rotate
  [rot ch]
  (let [c (+ rot (int ch))]
    (char
     (if (> c (int \z))
       (+ (int \a) (- c (int \z) 1))
       c))))

(defn rotate-word
  [rot s]
  (str/join (map (partial rotate rot) s)))

(defn decrypt
  [room]
  (let [rot   (mod (:sector room) 26)
        words (str/split (:name room) #"-")]
    (assoc room
           :decrypted (->> words
                           (map (partial rotate-word rot))
                           (str/join " ")))))

(deftest decrypt-test
  (is (= "very encrypted name"
         (:decrypted
          (decrypt {:name "qzmt-zixmtkozy-ivhz"
                    :sector 343})))))

(defn valid?
  [room]
  (= (:checksum room) (checksum (:name room))))

(let [regex #"([^\d]+)-(\d+)\[([^\]]+)\]"]
  (defn parse-room
    [s]
    (let [matches (re-matches regex s)]
      {:name     (nth matches 1)
       :sector   (Integer/parseInt (nth matches 2))
       :checksum (nth matches 3)})))

(defn north-pole-room?
  [room]
  (= "northpole object storage"
     (:decrypted room)))

(deftest part1-examples
  (is (valid? (parse-room "aaaaa-bbb-z-y-x-123[abxyz]")))
  (is (valid? (parse-room "a-b-c-d-e-f-g-h-987[abcde]")))
  (is (valid? (parse-room "not-a-real-room-404[oarel]")))
  (is (not (valid? (parse-room "totally-real-room-200[decoy]")))))

(defn part1
  []
  (->> (slurp "../input.txt")
       (str/split-lines)
       (map parse-room)
       (filter valid?)
       (map :sector)
       (apply +)))

(defn part2
  []
  (->> (slurp "../input.txt")
       (str/split-lines)
       (map parse-room)
       (map decrypt)
       (some #(if (north-pole-room? %) %))))

(comment
  (run-all-tests)
  (part1)
  (part2))
