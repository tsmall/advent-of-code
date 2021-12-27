package main

import "core:fmt"
import "core:os"

main :: proc() {
    input := read_input()
    defer delete(input)

    possible1 := 0
    possible2 := 0

    data := input
    for len(data) > 0 {
        skip_whitespace(&data);  a := read_int(&data)
        skip_whitespace(&data);  b := read_int(&data)
        skip_whitespace(&data);  c := read_int(&data)

        skip_whitespace(&data);  d := read_int(&data)
        skip_whitespace(&data);  e := read_int(&data)
        skip_whitespace(&data);  f := read_int(&data)

        skip_whitespace(&data);  g := read_int(&data)
        skip_whitespace(&data);  h := read_int(&data)
        skip_whitespace(&data);  i := read_int(&data)

        if (a + b > c && a + c > b && c + b > a) do possible1 += 1
        if (d + e > f && d + f > e && f + e > d) do possible1 += 1
        if (g + h > i && g + i > h && i + h > g) do possible1 += 1

        if (a + d > g && a + g > d && g + d > a) do possible2 += 1
        if (b + e > h && b + h > e && h + e > b) do possible2 += 1
        if (c + f > i && c + i > f && i + f > c) do possible2 += 1
    }

    fmt.printf("Part 1: %d\n", possible1)
    fmt.printf("Part 2: %d\n", possible2)
}

read_input :: proc() -> []u8 {
    input, ok := os.read_entire_file("../input.txt")
    if !ok do fmt.panicf("Unable to read the input file.")
    return input
}

skip_whitespace :: proc(data: ^[]u8) {
    for len(data) > 0 && (data[0] == ' ' || data[0] == '\n') {
        data^ = data[1:]
    }
}

read_int :: proc(data: ^[]u8) -> int {
    n := 0
    for len(data) > 0 && data[0] >= '0' && data[0] <= '9' {
        n *= 10
        n += int(data[0] - '0')
        data^ = data[1:]
    }
    return n
}
