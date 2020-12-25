def solution():
    var nice_words = 0

    var word = ""
    def next():
        word = get_line("")
        return word.length

    while next() != 0:
        if is_nice(word):
            nice_words++

    print "Part 1: " + nice_words

def is_nice(word):
    return has_needed_vowels(word)    and
           has_duplicate_letter(word) and
           no_forbidden_strings(word)

def has_needed_vowels(word):
    var count = 0
    for (word) c:
        switch c:
            case 'a', 'e', 'i', 'o', 'u':
                count++
        if count >= 3:
            return true
    return false

def has_duplicate_letter(word):
    var previous = ''
    for (word) c:
        if c == previous:
            return true
        previous = c
    return false

def no_forbidden_strings(word):
    let forbidden = [ "ab", "cd", "pq", "xy" ]
    for (forbidden) f:
        if find_string(word, f) != -1:
            return false
    return true

def has_two_pairs(word):
    return true

solution()
