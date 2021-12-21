package main

import "core:fmt"
import "core:mem"
import "core:os"

import "../../lib/cpu"

main :: proc() {
    input := read_input()
    program := cpu.parse_program(input)
    delete(input)
    defer delete(program)

    part_one: int
    part_two: int

    outer: for noun in 0..99 {
        for verb in 0..99 {
            memory := mem.clone_slice(program)
            defer delete(memory)

            result := cpu.run_program(memory, noun, verb)

            if noun == 12 && verb == 2 {
                part_one = result
            }

            if result == 19690720 {
                part_two = 100 * noun + verb
            }

            if part_one > 0 && part_two > 0 do break outer
        }
    }

    fmt.printf("Part 1: %d\n", part_one)
    fmt.printf("Part 2: %d\n", part_two)
}

read_input :: proc() -> []u8 {
    input, ok := os.read_entire_file_from_filename("../input.txt")
    if !ok {
        fmt.panicf("Unable to read the input file.")
    }
    return input
}
