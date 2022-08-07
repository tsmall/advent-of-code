package main

import "core:fmt"
import "core:io"
import "core:os"
import "core:strings"

Disc :: struct {
    positions: u8,
    position: u8,
}

extra_part2_disc :: Disc{ 11, 0 }

main :: proc() {
    original_discs := parse_input()
    defer delete(original_discs)

    discs := make([dynamic]Disc, 0, len(original_discs) + 1)
    defer delete(discs)

    for d in original_discs do append(&discs, d)
    time := find_winning_start_time(discs[:])
    fmt.printf("Part 1: %d\n", time)

    for d, i in original_discs do discs[i] = d
    append(&discs, extra_part2_disc)
    time = find_winning_start_time(discs[:])
    fmt.printf("Part 2: %d\n", time)
}

find_winning_start_time :: proc(discs: []Disc) -> u64 {
    advance_based_on_position(discs)

    time: u64 = 0
    for {
        advance(discs)
        if all_aligned(discs) do return time

        time += 1
    }
}

advance_based_on_position :: proc(discs: []Disc) {
    for _, i in discs {
        disc := &discs[i]
        advance_disc(disc, u8(i))
    }
}

advance :: proc(discs: []Disc) {
    for _, i in discs {
        disc := &discs[i]
        advance_disc(disc, 1)
    }
}

advance_disc :: #force_inline proc(disc: ^Disc, secs: u8) {
    disc.position = (disc.position + secs) % disc.positions
}

all_aligned :: proc(discs: []Disc) -> bool {
    for disc in discs {
        if disc.position != 0 do return false
    }
    return true
}

// ---------------------------------------------------------------------
// Parsing

parse_input :: proc() -> []Disc {
    instream := os.stream_from_handle(os.stdin)
    reader := io.to_reader(instream)

    buffer: [500]u8
    n, err := io.read(reader, buffer[:])
    if err != nil do fmt.panicf("ERROR: Unable to read input\n")
    s := string(buffer[:n])

    discs := make([dynamic]Disc)
    prev_index := 0
    for {
        s = s[prev_index:]
        newline_index := strings.index_byte(s, '\n')
        if newline_index == -1 do break

        line := s[:newline_index]
        positions, position := parse_line(line)
        disc := Disc{ positions, position }
        append(&discs, disc)

        prev_index = newline_index + 1
    }

    return discs[:]
}

skip_1 :: len("Disc #X has ")
skip_2 :: len(" positions; at time=0, it is at position ")

parse_line :: proc(line: string) -> (positions: u8, position: u8) {
    n := 0
    s := line[skip_1:]
    n, positions = read_u8(s)
    s = s[skip_2 + n:]
    n, position = read_u8(s)

    return
}

read_u8 :: proc(s: string) -> (n: int, result: u8) {
    s := s
    for len(s) > 0 && s[0] >= '0' && s[0] <= '9' {
        result *= 10
        result += s[0] - '0'

        s = s[1:]
        n += 1
    }
    return
}
