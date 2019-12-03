### Advent of Code - Day 02

# This program implements an intcode computer
# that operates on the memory provided via stdin.
# It executes each instruction
# until it reaches the first halt instruction (99),
# then prints the result (contained at address 0).

# See ../problem.txt for the full details.


(var ic "Instruction Counter" 0)
(var mem "Computer Memory" @[])


(defn parse-input
  "Parse the intcode memory from stdin."
  []
  (->> (file/read stdin :all)
       (string/trim)
       (string/split ",")
       (map scan-number)))


(defn read-instruction
  "Read the current instruction from memory."
  []
  {:opcode (get mem ic)
   :inptr1 (get mem (+ ic 1))
   :inptr2 (get mem (+ ic 2))
   :outptr (get mem (+ ic 3))})


(defn halt?
  "Returns true if the program should halt."
  [instruction]
  (= 99 (instruction :opcode)))


(defn result
  "Calculate the result of running instr."
  [instr]
  (let [{:opcode opcode
         :inptr1 inptr1
         :inptr2 inptr2} instr
        x (get mem inptr1)
        y (get mem inptr2)]
    (case opcode
      1 (+ x y)
      2 (* x y)
      (error (string "Invalid opcode: " opcode)))))


(defn execute
  "Execute the instruction, modifying the contents of mem."
  [instr]
  (let [{:outptr outptr} instr]
    (set (mem outptr) (result instr))))


(defn restore-state
  "Restore mem state to just before computer caught fire."
  []
  (set (mem 1) 12)
  (set (mem 2) 2))


(def instructions
  "Generator containing all the program's instructions until it should halt."
  (generate [instr :iterate (read-instruction)
             :until (halt? instr)]
            (+= ic 4)
            instr))


(set mem (parse-input))

(restore-state)

(loop [instr :generate instructions]
  (execute instr))

(printf "Answer: %d\n" (mem 0))
