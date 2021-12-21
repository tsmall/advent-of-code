package main

import "core:fmt"
import "core:os"

main :: proc() {
    input := read_input()
    defer delete(input)

    ns := read_all_ints(input)
    defer delete(ns)

    part1: int
    part2: int
    for i in 0..<len(ns) {
        for j in i..<len(ns) {
            if ns[i] + ns[j] == 2020 {
                part1 = ns[i] * ns[j]
            }

            for k in j..<len(ns) {
                if ns[i] + ns[j] + ns[k] == 2020 {
                    part2 = ns[i] * ns[j] * ns[k]
                }
            }
        }
    }

    fmt.printf("Part 1: %d\n", part1)
    fmt.printf("Part 2: %d\n", part2)
}

read_input :: proc() -> []u8 {
    input, ok := os.read_entire_file_from_filename("../input.txt")
    if !ok {
        fmt.panicf("Unable to read the input file.")
    }
    return input
}

read_all_ints :: proc(data: []u8) -> [dynamic]int {
    ns: [dynamic]int

    rest := data
    n: int
    for len(rest) > 0 {
        rest = skip_whitespace(rest)
        n, rest = read_int(rest)
        append(&ns, n)
        rest = skip_whitespace(rest)
    }

    return ns
}

skip_whitespace :: proc(data: []u8) -> []u8 {
    rest := data
    for len(rest) > 0 && rest[0] == '\n' {
        rest = rest[1:]
    }
    return rest
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
