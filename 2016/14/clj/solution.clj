(ns solution)

(defn bytes->hex
  [bytes]
  (let [HEXES (char-array "0123456789abcdef")
        hex   (StringBuilder. (* 2 (count bytes)))]
    (doseq [b bytes]
      (.append hex (get HEXES (bit-shift-right (bit-and b 0xF0) 4)))
      (.append hex (get HEXES (bit-and b 0x0F))))
    (.toString hex)))

(def digest
  (java.security.MessageDigest/getInstance "MD5"))

(defn md5
  [s]
  (bytes->hex (.digest (doto digest
                         .reset
                         (.update (.getBytes s))))))

(defn stretched-md5
  [s]
  (loop [i    0
         hash (md5 s)]
    (if (= i 2016)
      hash
      (recur (inc i) (md5 hash)))))

(defn find-triple
  [s]
  (->> (seq s)
       (partition 3 1)
       (filter #(apply = %))
       (first)
       (first)))

(defn find-quintuplets
  [s]
  (->> (seq s)
       (partition 5 1)
       (filter #(apply = %))
       (map first)
       (into #{})))

(defn analyze-hash
  [hash index]
  {:index       index
   :triple      (find-triple hash)
   :quintuplets (find-quintuplets hash)})

(defn possible-keys
  ([salt f]
   (possible-keys salt f 0))
  ([salt f index]
   (lazy-seq
    (let [hash (f (str salt index))
          info (analyze-hash hash index)]
      (if (or (some? (:triple info)) (not (empty? (:quintuplets info))))
        (cons info (possible-keys salt f (inc index)))
        (possible-keys salt f (inc index)))))))

(defn is-key?
  [hash future-hashes]
  (when-let [c (:triple hash)]
    (let [max-index (+ 1000 (:index hash))]
      (some #(contains? (:quintuplets %) c)
            (take-while #(<= (:index %) max-index) future-hashes)))))

(defn next-key
  [hashes]
  (loop [offset 0
         hashes hashes]
    (if (is-key? (first hashes) (rest hashes))
      hashes
      (recur (inc offset) (rest hashes)))))

(def keys-needed
  64)

(defn solution
  [input f]
  (loop [hashes (possible-keys input f)
         found  0]
    (let [new-hashes (next-key hashes)]
      (println "Found key" found "at index" (:index (first new-hashes)))
      (if (= found (dec keys-needed))
        (:index (first new-hashes))
        (recur (rest new-hashes) (inc found))))))

(def input
  "qzyelonm")

(defn part1
  [input]
  (solution input md5))

(defn part2
  [input]
  (solution input stretched-md5))

(comment
  (time (part1 input))
  (time (part2 input))
  )
