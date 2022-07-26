package main

import "core:fmt"
import "core:crypto/md5"
import "core:strings"

// salt :: "abc"
salt :: "qzyelonm"

main :: proc() {
    index := generate_keys(md5_string)
    fmt.printf("Part 1: %d\n", index)

    index = generate_keys(md5_string_stretched)
    fmt.printf("Part 2: %d\n", index)
}

Hash_Proc :: proc(input: string, sb: ^strings.Builder)

generate_keys :: proc(hash_proc : Hash_Proc) -> int {
    context.allocator = context.temp_allocator
    defer free_all(context.temp_allocator)

    triplets    := make(map[int]rune)
    quintuplets := make(map[int][16]bool)

    index        := -1
    max_analyzed := -1
    keys_found   := 0

    sb := strings.make_builder()

    for keys_found < 64 {
        index += 1

        if index > max_analyzed {
            input := fmt.tprintf("%s%d", salt, index)
            hash_proc(input, &sb)
            hash := strings.to_string(sb)

            analyze(hash, index, &triplets, &quintuplets)
            max_analyzed = index
        }

        if index in triplets {
            hex_i := hex_index(triplets[index])

            for i in index+1 ..= index+1000 {
                if i > max_analyzed {
                    input := fmt.tprintf("%s%d", salt, i)
                    hash_proc(input, &sb)
                    hash := strings.to_string(sb)

                    analyze(hash, i, &triplets, &quintuplets)
                    max_analyzed = i
                }

                if (i in quintuplets) && quintuplets[i][hex_i] {
                    keys_found += 1
                    fmt.printf("--> Found key #%d at index #%d\n", keys_found, index)
                    break
                }
            }
        }
    }

    return index
}

analyze :: proc(hash: string, index: int, triplets: ^map[int]rune, quintuplets: ^map[int][16]bool) {
    quints : [16]bool
    found_quint := false

    prev := rune(0)
    run  := 0
    for c in hash {
        if c == prev {
            run += 1

            if run == 3 && !(index in triplets) {
                triplets[index] = c
            }

            if run == 5 {
                hex_i := hex_index(c)
                quints[hex_i] = true
                found_quint = true
            }
        } else {
            run = 1
        }

        prev = c
    }

    if found_quint do quintuplets[index] = quints
}

md5_string :: proc(input: string, sb: ^strings.Builder) {
    strings.reset_builder(sb)
    for b in md5.hash(input) {
        fmt.sbprintf(sb, "%02x", b)
    }
}

md5_string_stretched :: proc(input: string, sb: ^strings.Builder) {
    md5_string(input, sb)
    for i in 1..=2016 {
        md5_string(strings.to_string(sb^), sb)
    }
}

hex_index :: proc(c: rune) -> u8 {
    result: u8
    if c >= '0' && c <= '9' {
        result = u8(c) - '0'
    } else {
        result = u8(c) - 'a' + 10
    }
    return result
}
