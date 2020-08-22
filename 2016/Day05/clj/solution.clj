(ns aoc.day04
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-all-tests]]))

(defn md5
  [s]
  (apply str
         (map (partial format "%02x")
              (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                         .reset
                         (.update (.getBytes s)))))))

(defn door-ids
  ([key]
   (door-ids key 1))
  ([key id]
   (lazy-seq (cons (str key id) (door-ids key (inc id))))))

(defn valid?
  [hash]
  (str/starts-with? hash "00000"))

(defn valid-hashes
  [key]
  (->> (door-ids key)
       (pmap md5)
       (filter valid?)))

(defn parse-code
  [hash]
  (.charAt hash 5))

(defn door-code
  [key]
  (->> (valid-hashes key)
       (take 8)
       (map parse-code)
       (str/join)))

(defn get-input
  []
  (str/trim (slurp "../input.txt")))

(defn part1
  []
  (-> (get-input)
      (door-code)))

(comment
  (part1) ; "f77a0e6e"
  (part2))
