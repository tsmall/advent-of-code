import std

struct policy:
    n1:int
    n2:int
    letter:int

def parse(line:string):
    let parts = tokenize(line, "- :", " ")
    let n1 = string_to_int(parts[0], 10)
    let n2 = string_to_int(parts[1], 10)
    let letter = parts[2][0]
    let password = parts[3]
    return policy { n1, n2, letter }, password

def passes_sled_check(policy, password):
    let count = fold(password, 0) acc, char:
        if char == policy.letter:
            acc + 1
        else:
            acc
    return policy.n1 <= count and
           policy.n2 >= count

def passes_toboggan_check(p::policy, password):
    let char1 = password[ n1 - 1 ]
    let char2 = password[ n2 - 1 ]
    let matches = [ char1 == letter, char2 == letter ]
    return any(matches) and not all(matches)

def each_line(f):
    var line = ""
    while (line = get_line("")) and line.length > 0:
        f(line)

var valid_sled_passwords = 0
var valid_toboggan_passwords = 0
each_line():
    let policy, password = parse(_)
    if passes_sled_check(policy, password):
        valid_sled_passwords++
    if passes_toboggan_check(policy, password):
        valid_toboggan_passwords++

print "Part 1: " + valid_sled_passwords
print "Part 2: " + valid_toboggan_passwords
