package main

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"

main :: proc() {
    arena: mem.Arena
    buffer: [10_000]byte
    mem.init_arena(&arena, buffer[:])
    context.allocator = mem.arena_allocator(&arena)

    input       := read_input("../input.txt")
    frequencies := count_by_column(input)
    most, least := most_least_common_by_column(frequencies)

    fmt.printf("Part 1: %s\n", most)
    fmt.printf("Part 2: %s\n", least)
}

count_by_column :: proc(input: []u8) -> [][26]int {
    frequencies := make([dynamic][26]int, 0, 10)

    col := 0
    for char in input {
        switch char {
        case '\n':
            col = 0
        case:
            i := char - 'a'
            if col == len(frequencies) do append(&frequencies, [26]int{})
            frequencies[col][i] += 1
            col += 1
        }
    }

    return frequencies[:]
}

most_least_common_by_column :: proc(frequencies: [][26]int) -> (most, least: string) {
    most_b  := strings.make_builder(len=0, cap=len(frequencies))
    least_b := strings.make_builder(len=0, cap=len(frequencies))

    for col in frequencies {
        max_i, max_v: int = 0, 0
        min_i, min_v: int = 0, 9999

        for v, i in col {
            if          v > max_v do max_i, max_v = i, v
            if v > 0 && v < min_v do min_i, min_v = i, v
        }

        strings.write_byte(&most_b,  'a' + u8(max_i))
        strings.write_byte(&least_b, 'a' + u8(min_i))
    }

    return strings.to_string(most_b), strings.to_string(least_b)
}

read_input :: proc(filename: string) -> []u8 {
    input, ok := os.read_entire_file(filename)
    if !ok do fmt.panicf("Unable to read the input file.")
    return input
}
