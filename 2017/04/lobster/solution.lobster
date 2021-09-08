import std
import dictionary

let input = read_file("../input.txt", true)
if input == nil: fatal_exit("Could not read input file")

let lines = tokenize(input, "\n", "")

def contains_duplicate(xs):
    var i = 0
    while i < xs.length:
        var j = i + 1
        while j < xs.length:
            if xs[i] == xs[j]:
                return true
            j += 1
        i += 1
    return false

let score = dictionary<int, int> { 26 }
score.set('a', 2)
score.set('b', 3)
score.set('c', 5)
score.set('d', 7)
score.set('e', 11)
score.set('f', 13)
score.set('g', 17)
score.set('h', 19)
score.set('i', 23)
score.set('j', 29)
score.set('k', 31)
score.set('l', 37)
score.set('m', 41)
score.set('n', 43)
score.set('o', 47)
score.set('p', 53)
score.set('q', 59)
score.set('r', 61)
score.set('s', 67)
score.set('t', 71)
score.set('u', 73)
score.set('v', 79)
score.set('w', 83)
score.set('x', 89)
score.set('y', 97)
score.set('z', 101)

def anagram_score(word):
    return fold(word, 1) acc, char:
        let n = score.get(char, -1)
        if n == -1: fatal_exit("Unexpected letter: {char}")
        acc * n

def contains_duplicate_anagrams(words):
    let scores = map(words): anagram_score(_)
    return contains_duplicate(scores)

var valid_passphrase_count = 0
var valid_anagram_passphrase_count = 0
for(lines) line:
    let words = tokenize(line, " ", "")
    if not words.contains_duplicate:
        valid_passphrase_count += 1
    if not words.contains_duplicate_anagrams:
        valid_anagram_passphrase_count += 1

print "Part 1: {valid_passphrase_count}"
print "Part 1: {valid_anagram_passphrase_count}"
