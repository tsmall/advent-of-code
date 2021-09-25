### Advent of Code - Day 02

# This program implements an intcode computer
# that operates on the memory provided via stdin.
# It executes each instruction
# until it reaches the first halt instruction (99),
# then prints the result (contained at address 0).

# See ../problem.txt for the full details.


(var pc "Program Counter" 0)
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
  {:opcode (get mem pc)
   :inptr1 (get mem (+ pc 1))
   :inptr2 (get mem (+ pc 2))
   :outptr (get mem (+ pc 3))})


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


(defn load-program
  "Load a program and set its inputs."
  [program noun verb]
  (set mem (array/slice program))
  (set pc 0)
  (set (mem 1) noun)
  (set (mem 2) verb))


(defn instructions
  []
  "Generator containing all the program's instructions until it should halt."
  (generate [instr :iterate (read-instruction)
             :until (halt? instr)]
            (+= pc 4)
            instr))


(defn run
  "Run the program in mem until it halts. Returns the output."
  [program noun verb]
  (load-program program noun verb)
  (loop [instr :in (instructions)]
    (execute instr))
  (mem 0))


(defn run-until-output-matches
  "Run the program with different input until the output matches expected."
  [program expected]
  (fiber/new
   (fn []
     (loop [noun :range-to [0 99]
            verb :range-to [0 99]]
       (when (= expected (run program noun verb))
         (yield [noun verb]))))))


(def program (parse-input))

(printf
 "Part 1: %d"
 (run program 12 2))

(printf
 "Part 2: %d"
 (let [[noun verb] (resume (run-until-output-matches program 19690720))]
   (+ (* noun 100) verb)))
