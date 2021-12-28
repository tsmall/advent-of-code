package main

import "core:fmt"
import "core:os"
import "core:slice"

main :: proc() {
    input := read_input()
    defer delete(input)

    target_room := "northpole object storage "

    sum: int
    target_id: int

    id: int
    letters: [26]u8
    words: [500]u8
    words_i: u8

    data := input
    for len(data) > 0 {
        switch c := data[0]; c {
        case '\n':
            cleartext := decipher(id, words[0:words_i])
            if equal(cleartext, target_room) do target_id = id

            id = 0
            for _, i in letters do letters[i] = 0
            words_i = 0
            data = data[1:]
        case '-':
            words[words_i] = ' '
            words_i += 1
            data = data[1:]
        case '0'..'9':
            id, data = read_int(data)
        case '[':
            checksum := data[1:6]
            real := validate_checksum(letters, checksum)
            if real do sum += id
            data = data[7:]
        case:
            letters[c - 'a'] += 1
            words[words_i] = c
            words_i += 1
            data = data[1:]
        }
    }

    fmt.printf("Part 1: %d\n", sum)
    fmt.printf("Part 2: %d\n", target_id)
}

equal :: proc(a: []byte, b: string) -> bool {
    if len(a) != len(b) do return false

    for _, i in a {
        if a[i] != b[i] do return false
    }

    return true
}

decipher :: proc(rot: int, text: []u8) -> []u8 {
    cleartext := text
    for c, i in text {
        if c == ' ' do continue
        offset := (int(text[i] - 'a') + rot) % 26
        cleartext[i] = 'a' + u8(offset)
    }

    return cleartext
}

validate_checksum :: proc(letters: [26]u8, checksum: []u8) -> bool {
    expected := letters
    slice.reverse_sort(expected[:])

    prev: u8
    for c, i in checksum {
        count := letters[c - 'a']
        if (count != expected[i])               do return false
        if (count == prev && c < checksum[i-1]) do return false
        prev = count
    }

    return true
}

read_input :: proc() -> []u8 {
    input, ok := os.read_entire_file("../input.txt")
    if !ok do fmt.panicf("Unable to read the input file.")
    return input
}

read_int :: proc(data: []u8) -> (n: int, rest: []u8) {
    rest = data
    for len(rest) > 0 && rest[0] >= '0' && rest[0] <= '9' {
        n *= 10
        n += int(rest[0] - '0')
        rest = rest[1:]
    }

    return
}
