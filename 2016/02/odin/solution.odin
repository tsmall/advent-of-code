package main

import "core:fmt"
import "core:os"

main :: proc() {
    input := read_input()
    defer delete(input)

    data := input

    keypad1 := [][]u8{
      []u8{1, 2, 3},
      []u8{4, 5, 6},
      []u8{7, 8, 9},
    }

    keypad2 := [][]u8{
      []u8{' ', ' ', '1', ' ', ' '},
      []u8{' ', '2', '3', '4', ' '},
      []u8{'5', '6', '7', '8', '9'},
      []u8{' ', 'A', 'B', 'C', ' '},
      []u8{' ', ' ', 'D', ' ', ' '},
    }

    point1 := Point{x=1, y=1}
    point2 := Point{x=0, y=2}
    code1 : [5]u8; i1 : u8
    code2 : [5]u8; i2 : u8

    for len(data) > 0 {
        switch data[0] {
        case 'U':
            if point1.y > 0 do point1.y -= 1

            y := point2.y - 1
            if (y >= 0 && keypad2[y][point2.x] != ' ') do point2.y = y
        case 'D':
            if point1.y < 2 do point1.y += 1

            y := point2.y + 1
            if (y < 5 && keypad2[y][point2.x] != ' ') do point2.y = y
        case 'L':
            if point1.x > 0 do point1.x -= 1

            x := point2.x - 1
            if (x >= 0 && keypad2[point2.y][x] != ' ') do point2.x = x
        case 'R':
            if point1.x < 2 do point1.x += 1

            x := point2.x + 1
            if (x < 5 && keypad2[point2.y][x] != ' ') do point2.x = x
        case '\n':
            code1[i1] = keypad1[point1.y][point1.x]; i1 += 1
            code2[i2] = keypad2[point2.y][point2.x]; i2 += 1
        }

        data = data[1:]
    }

    fmt.printf("Part 1: %d\n", code1)
    fmt.printf("Part 2: %c\n", code2)
}

Point :: struct { x, y: i8 }

read_input :: proc() -> []u8 {
    input, ok := os.read_entire_file("../input.txt")
    if !ok do fmt.panicf("Unable to read the input file.")
    return input
}
