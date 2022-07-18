(ns cpu
  (:require [clojure.test :refer [deftest is run-tests]]))

(def ^:private op-name
  {1  :add
   2  :multiply
   99 :halt})

(defn- execute-binary-op
  [pc mem f]
  (let [x-addr   (mem (+ pc 1))
        y-addr   (mem (+ pc 2))
        out-addr (mem (+ pc 3))
        result   (f (mem x-addr) (mem y-addr))]
    (assoc mem out-addr result)))

(defmulti ^:private execute
  (fn [op pc mem] op))

(defmethod execute :add
  [_ pc mem]
  (execute-binary-op pc mem +))

(defmethod execute :multiply
  [_ pc mem]
  (execute-binary-op pc mem *))

(defn run
  [program]
  (loop [pc  0
         mem program]
    (let [op (-> (get mem pc) op-name)]
      (cond
        (= op :halt) mem
        :else (recur (+ pc 4)
                     (execute op pc mem))))))

(deftest text-example-programs
  (is (= (run [1 0 0 0 99])          [2 0 0 0 99]))
  (is (= (run [2 3 0 3 99])          [2 3 0 6 99]))
  (is (= (run [2 4 4 5 99 0])        [2 4 4 5 99 9801]))
  (is (= (run [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99])))

(defn set-input
  [mem noun verb]
  (assoc mem
         1 noun
         2 verb))
