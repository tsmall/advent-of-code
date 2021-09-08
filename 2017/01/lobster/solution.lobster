import std

let input = read_file("../input.txt", true)
if input == nil:
    fatal_exit("Could not read input file")

let len  = input.length - 1  // Ignore trailing newline
let half = len / 2

var next_sum = 0
var half_sum = 0

var i = 0
while i < len:
    let next_i = (i + 1) % len
    let half_i = (i + half) % len
    if input[i] == input[next_i]:
        next_sum += input[i] - '0'
    if input[i] == input[half_i]:
        half_sum += input[i] - '0'
    i += 1

print "Part 1: " + next_sum
print "Part 2: " + half_sum
