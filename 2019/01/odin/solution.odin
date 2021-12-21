package main

import "core:fmt"
import "core:os"

main :: proc() {
    input := read_input()
    defer delete(input)

    masses := read_all_ints(input)
    defer delete(masses)

    fuel: int
    fuel_fuel: int
    for mass in masses {
        new_fuel := fuel_for(mass)
        fuel += new_fuel

        for {
            new_fuel = fuel_for(new_fuel)
            if new_fuel < 0 do break
            fuel_fuel += new_fuel
        }
    }

    fmt.printf("Part 1: %d\n", fuel)
    fmt.printf("Part 2: %d\n", fuel + fuel_fuel)
}

fuel_for :: #force_inline proc(mass: int) -> int {
    return mass / 3 - 2
}

read_input :: proc() -> []u8 {
    input, ok := os.read_entire_file_from_filename("../input.txt")
    if !ok do fmt.panicf("Unable to read the input file.")
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
