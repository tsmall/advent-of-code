(ns aoc.day11
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as s]
            [clojure.string :as str]))

(defn move-objs
  [objs dest state]
  (let [{:keys [:elevator :floors]} state]
    (assert (s/subset? objs (floors elevator)))
    (-> floors
        (update elevator #(set (s/difference % objs)))
        (update dest #(set (s/union % objs))))))

(defn possible-moves
  [{:keys [:elevator :floors]}]
  (let [objs (seq (floors elevator))]
    (apply conj
     (combo/combinations objs 1)
     (combo/combinations objs 2))))

(defn possible-floors
  [{:keys [:elevator]}]
  (letfn [(in-bounds? [f] (<= 0 f 3))]
    (filter in-bounds? [(inc elevator) (dec elevator)])))

(defn possible-states
  [{:keys [:elevator :floors :steps] :as state}]
  (let [curr   (floors elevator)
        floors (possible-floors state)
        moves  (possible-moves state)]
    (for [f floors
          m moves]
      (assoc state
             :steps    (inc steps)
             :elevator f
             :floors   (move-objs m f state)))))

(defn get-elements
  [type floor]
  (->> floor
       (filter (fn [f] (= (:type f) type)))
       (map :element)
       (set)))

(defn valid-state?
  [{:keys [:elevator :floors]}]
  (let [curr  (floors elevator)
        chips (get-elements :microchip curr)
        gens  (get-elements :generator curr)
        lones (s/difference chips gens)]
    (or (= 0 (count gens))
        (= 0 (count lones)))))

(defn next-states
  [state]
  (->> (possible-states state)
       (filter valid-state?)))

(defn done?
  [{:keys [:elevator :floors]}]
  (every? empty? (butlast floors)))

(defn state-hash
  [{:keys [:elevator :floors]}]
  (letfn [(count [floor]
            (->> floor
                 (map :type)
                 (frequencies)))]
    {:elevator elevator
     :counts   (map count floors)}))

(defn fewest-steps
  [state]
  (loop [queue (clojure.lang.PersistentQueue/EMPTY)
         state state
         seen  #{}]
    (if (done? state)
      (:steps state)
      (let [nexts  (next-states state)
            unseen (filter #(not (seen (state-hash %))) nexts)
            q      (reduce conj queue unseen)]
        (recur (pop q)
               (peek q)
               (s/union seen (into #{} (map state-hash unseen))))))))

(defn parse-line
  [s]
  (let [re #"(\w+)(?:-compatible)? (microchip|generator)"
        ms (re-seq re s)]
    (into #{} (map (fn [[_match element type]]
                     {:type (keyword type)
                      :element (keyword element)})
                   ms))))

(defn parse
  [s]
  {:steps 0
   :elevator 0
   :floors (into [] (map parse-line (str/split-lines s)))})

(defn part1
  [f]
  (-> (slurp f)
      (parse)
      (fewest-steps)))

(defn add-extra-objs
  [state]
  (let [objs [{:type :generator :element :elerium}
              {:type :microchip :element :elerium}
              {:type :generator :element :dilithium}
              {:type :microchip :element :dilithium}]]
    (update-in state [:floors 0] #(apply conj % objs))))

(defn part2
  [f]
  (-> (slurp f)
      (parse)
      (add-extra-objs)
      (fewest-steps)))

(comment
  (part1 "../example.txt")
  (part1 "../input.txt")
  (part2 "../example.txt")
  (part2 "../input.txt")
  )
