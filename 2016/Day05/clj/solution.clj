(ns aoc.day05
  (:require [clojure.string :as str]))


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


(defn valid-prefix?
  [hash]
  (str/starts-with? hash "00000"))


(defn valid-hashes
  [key valid?]
  (->> (door-ids key)
       (pmap md5)
       (filter valid?)))


(defn parse-code
  [hash]
  (.charAt hash 5))


(defn get-input
  []
  (str/trim (slurp "../input.txt")))


(defn part1
  []
  (let [key (get-input)]
    (->> (valid-hashes key valid-prefix?)
         (take 8)
         (map parse-code)
         (str/join))))


(defn valid-position?
  [hash]
  (let [c (.charAt hash 5)]
    (and
     (Character/isDigit c)
     (<= 0 (Character/digit c 10) 7))))


(defn parse-positioned-code
  [hash]
  (let [position (Character/digit (.charAt hash 5) 10)
        code (.charAt hash 6)]
    [position code]))


(defn add-result
  [code hash]
  (let [[p c] (parse-positioned-code hash)]
    (if (nil? (get code p))
      (assoc code p c)
      code)))


(defn part2
  ([]
   (part2 (get-input)))
  ([key]
   (let [valid? #(and (valid-prefix? %1)
                      (valid-position? %1))
         start (into [] (repeat 8 nil))]
     (->> (valid-hashes key valid?)
          (reductions add-result start)
          (filter #(not-any? nil? %1))
          (take 1)
          (first)
          (str/join)))))


(comment
  (part1)  ; "f77a0e6e"
  (part2)  ; "999828ec"
  )
