package main

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"

main :: proc() {
    runtests();

    input, ok := os.read_entire_file("../input.txt");
    if !ok {
        fmt.println("ERROR: Could not open input file");
        return;
    }
    defer delete(input);

    lines := strings.split(string(input), "\n");
    defer delete(lines);

    total_paper := 0;
    total_ribbon := 0;
    for line in lines {
        if dim, ok := parse(line); ok {
            total_paper += wrapping_paper_needed(dim);
            total_ribbon += ribbon_needed(dim);
        }
        else {
            fmt.printf("Unparseable line: '%s'\n", line);
        }
    }

    fmt.println("\n--- Part One ---");
    fmt.println(total_paper);

    fmt.println("\n--- Part Two ---");
    fmt.println(total_ribbon);
}

Dimensions :: struct {
    l: int,
    w: int,
    h: int,
}

wrapping_paper_needed :: proc(d: Dimensions) -> int {
    using d;
    area_lw := l * w;
    area_wh := w * h;
    area_hl := h * l;
    extra   := min([]int{area_lw, area_wh, area_hl});

    return (2 * area_lw) + (2 * area_wh) + (2 * area_hl) + extra;
}

ribbon_needed :: proc(d: Dimensions) -> int {
    using d;
    perim_lw := (2 * l) + (2 * w);
    perim_wh := (2 * w) + (2 * h);
    perim_hl := (2 * h) + (2 * l);
    bow      := l * w * h;

    return min([]int{perim_lw, perim_wh, perim_hl}) + bow;
}

min :: proc(nums: []int) -> int {
    min := nums[0];
    for n in nums[1:] {
        if n < min do min = n;
    }
    return min;
}

parse :: proc(input: string) -> (parsed: Dimensions, ok: bool) {
    parts := strings.split(input, "x");
    defer delete(parts);

    if len(parts) != 3 {
        ok = false;
        return;
    }

    h := strconv.parse_int(parts[0]);
    w := strconv.parse_int(parts[1]);
    l := strconv.parse_int(parts[2]);

    return Dimensions{h, w, l}, true;
}

runtests :: proc() {
    fmt.println("\n--- Tests ---");

    test_part_one(Dimensions{2, 3, 4}, 58);
    test_part_one(Dimensions{1, 1, 10}, 43);

    test_parse("1x2x3", Dimensions{1, 2, 3});
    test_parse("10x20x30", Dimensions{10, 20, 30});

    test_part_two(Dimensions{2, 3, 4}, 34);
    test_part_two(Dimensions{1, 1, 10}, 14);
}

test_part_one :: proc(d: Dimensions, expected: int) {
    actual := wrapping_paper_needed(d);
    test_report(d, expected, actual);
}

test_parse :: proc(input: string, expected: Dimensions) {
    if actual, ok := parse(input); ok {
        test_report(input, expected, actual);
    }
    else {
        fmt.printf("not ok - %v\n", input);
        fmt.println("# Parsing failed");
    }
}

test_part_two :: proc(d: Dimensions, expected: int) {
    actual := ribbon_needed(d);
    test_report(d, expected, actual);
}

test_report :: proc(input: $T, expected, actual: $U) {
    if equal(expected, actual) {
        fmt.printf("ok - %v\n", input);
    }
    else {
        fmt.printf("not ok - %v\n", input);
        fmt.printf("# Expected %v\n", expected);
        fmt.printf("# Actual   %v\n", actual);
    }
}

equal_dimensions :: proc(d1, d2: Dimensions) -> bool {
    return (d1.l == d2.l) && (d1.w == d2.w) && (d1.h == d2.h);
}

equal_int :: proc(i1, i2: int) -> bool {
    return i1 == i2;
}

equal :: proc{equal_dimensions, equal_int};
