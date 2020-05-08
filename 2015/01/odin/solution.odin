package main

import "core:fmt"
import "core:os"

main :: proc() {
    runtests();

    input, ok := os.read_entire_file("../input.txt");
    if !ok {
        fmt.println("ERROR: Could not open input file");
        return;
    }
    defer delete(input);

    instructions := string(input);
    final_floor, basement_index := move(instructions);

    fmt.println("\n--- Part 1---");
    fmt.println(final_floor);

    fmt.println("\n--- Part 2 ---");
    fmt.println(basement_index);
}

move :: proc(instructions: string) -> (final, basement_index: int) {
    for c, i in instructions {
        switch c {
        case '(':
            final += 1;
        case ')':
            final -= 1;
        }

        if final < 0 && basement_index == 0 do basement_index = i + 1;
    }
    return;
}

runtests :: proc() {
    fmt.println("--- Tests ---");

    test_part_one("(())", 0);
    test_part_one("()()", 0);
    test_part_one("(((", 3);
    test_part_one("(()(()(", 3);
    test_part_one("())", -1);
    test_part_one("))(", -1);

    test_part_two(")", 1);
    test_part_two("()())", 5);
}

test_part_one :: proc(input: string, expected: int) {
    actual, _ := move(input);
    test_report(input, expected, actual);
}

test_part_two :: proc(input: string, expected: int) {
    _, actual := move(input);
    test_report(input, expected, actual);
}

test_report :: proc(input: string, expected, actual: int) {
    if expected == actual {
        fmt.printf("ok - %s\n", input);
    }
    else {
        fmt.printf("not ok - %s\n", input);
        fmt.printf("# Expected %d\n", expected);
        fmt.printf("# Actual   %d\n", actual);
    }
}
