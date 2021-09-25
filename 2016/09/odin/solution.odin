package main

import "core:fmt"
import "core:os"
import "core:testing"

AlgorithmVersion :: enum{V1, V2}

main :: proc() {
    input := string(#load("../input.txt"))

    using AlgorithmVersion
    fmt.println("Part 1:", decompressed_size(V1, input))
    fmt.println("Part 2:", decompressed_size(V2, input))
}

decompressed_size :: proc(v: AlgorithmVersion, s: string) -> (size: int) {
    using AlgorithmVersion

    i := 0
    for i < len(s) {
        switch s[i] {
        case '(':
            // Read "(<num>x<num>)"
            i += 1
            char_count, j := read_next_int(s[i:])
            i += j + 1
            multiplier, k := read_next_int(s[i:])
            i += k + 1

            switch v {
            case V1:
                size += char_count * multiplier
            case V2:
                size += decompressed_size(V2, s[i:i+char_count]) * multiplier
            }
            i += char_count
        case ' ', '\t', '\n', '\r':
            i += 1
        case:
            i += 1
            size += 1
        }
    }

    return
}

@test
test_part1_examples :: proc(t: ^testing.T) {
    examples := map[string]int{
        "ADVENT" = 6,
        "A(1x5)BC" = 7,
        "(3x3)XYZ" = 9,
        "A(2x2)BCD(2x2)EFG" = 11,
        "(6x1)(1x3)A" = 6,
        "X(8x2)(3x3)ABCY" = 18,
    }
    defer delete(examples)

    for input, expected in examples {
        using AlgorithmVersion
        actual := decompressed_size(V1, input)
        testing.expect(t, actual == expected, input)
    }
}

@test
test_part2_examples :: proc(t: ^testing.T) {
    examples := map[string]int{
        "(3x3)XYZ" = 9,
        "X(8x2)(3x3)ABCY" = 20,
        "(27x12)(20x12)(13x14)(7x10)(1x12)A" = 241920,
        "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" = 445,
    }
    defer delete(examples)

    for input, expected in examples {
        using AlgorithmVersion

        actual := decompressed_size(V2, input)
        testing.expect(t, actual == expected, input)
    }
}

read_next_int :: proc(s:string) -> (num: int, chars_read: int) {
    loop: for i in 0..<len(s) {
        switch c := s[i]; c {
        case '0'..'9':
            num *= 10
            num += int(c - '0')
            chars_read += 1
        case:
            break loop
        }
    }

    return
}
