import std

// -----------------------------------------------------------------------------

def next_int(s:string, i:int):
    var chars = 0
    var num = 0
    var negative = false
    if s[i] == '-':
        negative = true
        i += 1
    while s[i] >= '0' and s[i] <= '9':
        num *= 10
        num += s[i] - '0'
        i += 1
        chars += 1
    if negative: num *= -1
    return num, chars

def for_int_in(s:string, fun):
    var i = 0
    while i < s.length:
        let num, offset = next_int(s, i)
        if offset > 0:
            fun(num)
        i += offset + 1

def load_input():
    let input = read_file("../input.txt")
    if input == nil: fatal_exit("Could not load input file")
    return input

def load_memory(input):
    let memory = vector_reserve(typeof return, 1061)
    for_int_in(input) num:
        memory.push(num)
    return memory

// -----------------------------------------------------------------------------

enum cpu_mode:
    basic
    complicated

class cpu:
    memory: [int]
    mode: cpu_mode
    pc: int = 0

    def tick():
        let jump = memory[pc]
        memory[pc] += offset_update_amount()
        pc += jump

    def offset_update_amount():
        if mode == basic:
            return 1
        else:
            if memory[pc] >= 3: return -1
            else: return 1

    def done():
        return pc >= memory.length

def run_cpu(input:string, mode:cpu_mode):
    let memory = load_memory(input)
    let cpu    = cpu { memory, mode }
    var count  = 0
    while not cpu.done:
        cpu.tick()
        count += 1
    return count

// -----------------------------------------------------------------------------

let input  = load_input()

print "Part 1: {run_cpu(input, basic)}"
print "Part 2: {run_cpu(input, complicated)}"
