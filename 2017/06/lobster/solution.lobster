import std

// -----------------------------------------------------------------------------

def load_input():
    let input = read_file("../input.txt")
    if input == nil: fatal_exit("Could not load input file")
    return input

def next_int(s:string, i:int):
    var chars = 0
    var num = 0
    while s[i] >= '0' and s[i] <= '9':
        num *= 10
        num += s[i] - '0'
        i += 1
        chars += 1
    return num, chars

def for_int_in(s:string, fun):
    var i = 0
    while i < s.length:
        let num, offset = next_int(s, i)
        if offset > 0:
            fun(num)
        i += offset + 1

def load_state(input:string):
    let state = vector_reserve(typeof return, 16)
    for_int_in(input) n:
        state.push(n)
    return state

def reallocate(state):
    var index = find_max_index(state)
    var blocks_to_redistribute = state[index]
    state[index] = 0
    while blocks_to_redistribute > 0:
        index = (index + 1) % state.length
        state[index] += 1
        blocks_to_redistribute -= 1

def find_max_index(ns):
    var max = 0
    var index = 0
    for(ns) n, i:
        if n > max:
            max = n
            index = i
    return index

def contains(states:[[int]], state:[int]):
    for(states) s:
        if equal(s, state):
            return true
    return false

// -----------------------------------------------------------------------------

let seen_states = []

let input = load_input()
var state = load_state(input)

while not seen_states.contains(state):
    seen_states.push(state)
    state = deepcopy(state)
    reallocate(state)

print "Part 1: {seen_states.length}"

var loop_size = 1
let repeat_state = deepcopy(state)
reallocate(state)
while not equal(state, repeat_state):
    reallocate(state)
    loop_size += 1

print "Part 2: {loop_size}"
