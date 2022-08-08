package main

import "core:fmt"
import "core:io"
import "core:os"
import "core:testing"


// ----------------------------------------------------------------------------
// Solution

NO_RANGE :: Range{0, 0}

main :: proc() {
    invalid_ranges, max_num := parse_input()
    defer delete(invalid_ranges)

    allowed_ranges := make([]Range, 1000)
    defer delete(allowed_ranges)
    allowed_ranges[0].end = max_num

    for ir in invalid_ranges {
        for _, i in allowed_ranges {
            ar := allowed_ranges[i]
            if ar == NO_RANGE do continue

            if is_superset(ir, ar) {
                allowed_ranges[i] = NO_RANGE
            }
            else if overlaps(ir, ar) {
                ar1, ar2 := split(ar, ir)
                allowed_ranges[i] = ar1
                if ar2 != NO_RANGE do add_range(allowed_ranges[:], ar2)
            }
        }
    }

    min := max_num
    total := 0
    for r in allowed_ranges {
        if r == NO_RANGE do continue
        if r.start < min do min = r.start
        for _ in r.start..=r.end do total += 1
    }

    fmt.printf("Part 1: %d\n", min)
    fmt.printf("Part 2: %d\n", total)
}

Range :: struct { start, end: int }

is_superset :: proc(ir, ar: Range) -> bool {
    return (ir.start <= ar.start) && (ir.end >= ar.end)
}

overlaps :: proc(ir, ar: Range) -> bool {
    return ((ir.start >= ar.start) && (ir.start <= ar.end)) \
        || ((ir.end   >= ar.start) && (ir.end   <= ar.end))
}

split :: proc(ar, ir: Range) -> (ar1, ar2: Range) {
    if ar.start == ir.start {
        ar1.start = ir.end + 1
        ar1.end = ar.end
    }
    else if ir.start < ar.start {
        ar1.start = ir.end + 1
        ar1.end = ar.end
    }
    else {
        ar1.start = ar.start
        ar1.end = ir.start - 1
        if ir.end < ar.end {
            ar2.start = ir.end + 1
            ar2.end = ar.end
        }
    }
    return
}

add_range :: proc(ranges: []Range, r: Range) {
    for _, i in ranges {
        if ranges[i] == NO_RANGE {
            ranges[i] = r
            return
        }
    }
    fmt.panicf("ERROR: Couldn't add range")
}


// ----------------------------------------------------------------------------
// Input Parsing

parse_input :: proc() -> (invalid_ranges: []Range, max_num: int) {
    invalid_ranges = parse_stdin()
    max_num = parse_args()
    return
}

parse_stdin :: proc() -> []Range {
    in_stream := os.stream_from_handle(os.stdin)
    in_reader := io.to_byte_reader(in_stream)

    result: [dynamic]Range

    buffer: [50]u8
    i := 0
    for {
        byte, err := io.read_byte(in_reader)
        if err == .EOF || byte == 0 do break
        if err != .None do fmt.panicf("ERROR: Unable to parse input")

        if byte == '\n' {
            line := string(buffer[:i])
            append(&result, parse_line(line))
            i = 0
        }
        else {
            buffer[i] = byte
            i += 1
        }
    }

    return result[:]
}

parse_line :: proc(s: string) -> Range {
    n, start, end: int

    n, start = read_int(s)
    rest := s[n + 1:]
    n, end = read_int(rest)

    return Range{start = start, end = end}
}

parse_args :: proc() -> int {
    n, result := read_int(os.args[1])
    if n == 0 do fmt.panicf("ERROR: Unable to parse args")
    return result
}

read_int :: proc(s: string) -> (n: int, result: int) {
    s := s
    for len(s) > 0 && s[0] >= '0' && s[0] <= '9' {
        result *= 10
        result += int(s[0]) - '0'

        s = s[1:]
        n += 1
    }
    return
}


// ----------------------------------------------------------------------------
// Tests

@(test)
run_tests :: proc(t: ^testing.T) {
    using testing

    ar := Range{2, 5}

    expect(t, is_superset(Range{2, 5}, ar), "superset/equal")
    expect(t, is_superset(Range{0, 9}, ar), "superset/superset")
    expect(t, is_superset(Range{6, 9}, ar) == false, "superset/no")

    expect(t, overlaps(Range{3, 4}, ar), "overlaps/subset-a")
    expect(t, overlaps(Range{2, 4}, ar), "overlaps/subset-b")
    expect(t, overlaps(Range{3, 5}, ar), "overlaps/subset-c")
    expect(t, overlaps(Range{1, 3}, ar), "overlaps/left")
    expect(t, overlaps(Range{4, 6}, ar), "overlaps/right")
    expect(t, overlaps(Range{0, 1}, ar) == false, "overlaps/no")

    test_split(t, split(ar, Range{3, 4}), Range{2, 2}, Range{5, 5})
    test_split(t, split(ar, Range{2, 4}), Range{5, 5}, NO_RANGE)
    test_split(t, split(ar, Range{3, 5}), Range{2, 2}, NO_RANGE)
    test_split(t, split(ar, Range{1, 3}), Range{4, 5}, NO_RANGE)
    test_split(t, split(ar, Range{4, 6}), Range{2, 3}, NO_RANGE)
}

test_split :: proc(t: ^testing.T, actual1, actual2, expected1, expected2: Range, loc := #caller_location) {
    if actual1 != expected1 {
        testing.errorf(
            t=t,
            format="actual1 != expected1 :: %v != %v\n",
            args={actual1, expected1},
            loc=loc
        )
    }
    if actual2 != expected2 {
        testing.errorf(
            t=t,
            format="actual2 != expected2 :: %v != %v\n",
            args={actual2, expected2},
            loc=loc
        )
    }
}
