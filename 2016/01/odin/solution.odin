package main

import "core:fmt"
import "core:os"

N :: 0
E :: 1
S :: 2
W :: 3

main :: proc() {
    d : u8
    p : Point

    input := read_input()
    defer delete(input)

    revisit : Point
    visited := make(map[Point]bool)
    defer delete(visited)

    data := input
    n : int
    for len(data) > 0 {
        if data[0] == '\n' do break

        switch data[0] {
        case 'L': if d == 0 { d = 3 } else { d -= 1 }
        case 'R': if d == 3 { d = 0 } else { d += 1 }
        }

        data = data[1:]

        n, data = read_int(data)

        diff : Point
        switch d {
        case N: diff = Point{0, 1}
        case E: diff = Point{1, 0}
        case S: diff = Point{0, -1}
        case W: diff = Point{-1, 0}
        }

        if data[0] == ',' do data = data[2:]

        dest := p + (diff * n)
        for p != dest {
            p += diff
            if (visited[p] && revisit == Point{0, 0}) do revisit = p
            visited[p] = true
        }
    }

    fmt.printf("Part 1: %d\n", distance(p))
    fmt.printf("Part 2: %d\n", distance(revisit))
}

Point :: [2]int

distance :: proc(p: Point) -> int {
    return abs(p[0]) + abs(p[1])
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
