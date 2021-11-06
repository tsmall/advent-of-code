(ns aoc.day10
  (:require [clojure.string :as str]))


(defn parse-int
  [s]
  (Integer/parseInt s))


(defn assign-value
  [value bots]
  (let [id (:id value)
        bot (get bots id {:id id :chips []})]
    (assoc bots id (update bot :chips #(conj % (:chip value))))))


(defn assign-rules
  [rules bots]
  (let [id (:id rules)
        bot (get bots id {:id id})]
    (assoc bots id (merge bot rules))))


(def value-line-re
  #"value (\d+) goes to bot (\d+)")

(defn parse-value-line
  [line]
  (when-let [matches (re-seq value-line-re line)]
    (let [[chip id] (map parse-int (nfirst matches))]
      {:chip chip
       :id id})))


(def give-line-re
  #"bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)")

(defn parse-give-line
  [line]
  (when-let [matches (re-seq give-line-re line)]
    (let [[id lo-type lo-chip hi-type hi-chip] (nfirst matches)]
      {:id (parse-int id)
       :low [(keyword lo-type) (parse-int lo-chip)]
       :high [(keyword hi-type) (parse-int hi-chip)]})))


(defn parse-line
  [bots line]
  (if-let [value (parse-value-line line)]
    (assign-value value bots)
    (if-let [rules (parse-give-line line)]
      (assign-rules rules bots)
      (do
        (println "Line didn't match:" line)
        bots))))


(defn parse
  [input]
  (let [lines (str/split-lines input)]
    (reduce parse-line {} lines)))

(defmulti give-chip
  (fn [chip from-id [to id] state] to))

(defmethod give-chip :bot
  [chip from-id [to id] state]
  (let [bots (:bots state)
        src-bot (get bots from-id)
        dst-bot (get bots id)
        updated-src-bot (update dst-bot :chips (partial filter #(not= % chip)))
        updated-dst-bot (update dst-bot :chips #(conj % chip))]
    (update
     state :bots
     #(assoc %
             from-id updated-src-bot
             id updated-dst-bot))))

(defmethod give-chip :output
  [chip from-id [to id] state]
  (update state :outputs #(assoc % id chip)))


(defn can-proceed?
  [bot]
  (= 2 (count (:chips bot))))

(defn proceed
  [bot state]
  {:pre [(can-proceed? bot)]}
  (let [chips (:chips bot)
        lo (apply min chips)
        hi (apply max chips)]
    (->> state
         (give-chip lo (:id bot) (:low bot))
         (give-chip hi (:id bot) (:high bot)))))


(defn tick
  "Performs one tick, updating all bots and outputs simultaneously."
  [state]
  (reduce-kv
   (fn [state id bot]
     (if (can-proceed? bot)
       (proceed bot state)
       state))
   state
   (:bots state)))


(defn find-bot-comparing
  [bots chips]
  (first (filter #(= (sort chips) (sort (:chips %))) (vals bots))))


(defn load-bots
  []
  (-> (slurp "../input.txt")
      (str/trim)
      (parse)))


(defn part-1
  []
  (let [bots (load-bots)]
    (loop [state {:bots bots :outputs {}}]
      (or
       (find-bot-comparing (:bots state) [61 17])
       (recur (tick state))))))


(defn part-2-calculation
  [outputs]
  (let [values (map #(get outputs %) [0 1 2])]
    (apply * values)))

(defn part-2
  []
  (let [bots (load-bots)]
    (loop [prev-state {}
           state {:bots bots :outputs {}}]
      (if (= prev-state state)
        (part-2-calculation (:outputs state))
        (recur state (tick state))))))


(comment
  (part-1)
  (part-2)
  )
