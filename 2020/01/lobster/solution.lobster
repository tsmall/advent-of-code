import std

def get_input():
    let numbers = vector_reserve(typeof return, 200)
    var line = ""
    while (line = get_line("")) and line.length != 0:
        let number = string_to_int(line, 10)
        if number == 0:
            break
        numbers.push(number)
    return numbers

def find_pair(entries):
    for(entries) entry1, i:
        for(entries.slice(i + 1, -1)) entry2:
            if (entry1 + entry2) == 2020:
                return [ entry1, entry2 ]
    fatal_exit("Pair not found")

def find_triple(entries):
    for(entries) entry1, i:
        for(entries.slice(i + 1, -1)) entry2, j:
            for(entries.slice(j + 1, -1)) entry3:
                if (entry1 + entry2 + entry3) == 2020:
                    return [ entry1, entry2, entry3 ]
    fatal_exit("Triplet not found")

def product(numbers):
    return reduce(numbers):
        _a * _b

let entries = get_input()
let part1_entries = find_pair(entries)
let part2_entries = find_triple(entries)

print "Part 1: " + product(part1_entries)
print "Part 2: " + product(part2_entries)
