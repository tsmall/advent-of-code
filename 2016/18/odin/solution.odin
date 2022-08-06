package main

import "core:bufio"
import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"

Tile :: u8
Row :: string

TRAP: Tile : '^'
SAFE: Tile : '.'

main :: proc() {
    defer free_all(context.allocator)

    row, count_part1, count_part2 := read_input()
    safe_tile_count := count_safe_tiles(row)

    sb1 := strings.make_builder()
    sb2 := strings.make_builder()
    sb := &sb1

    i := 1
    for i < count_part1 {
        generate_next_row(row, sb)
        row = strings.to_string(sb^)
        safe_tile_count += count_safe_tiles(row)
        i += 1
        sb = &sb2 if sb == &sb1 else &sb1
    }
    fmt.printf("Part 1: %d\n", safe_tile_count)

    for i < count_part2 {
        generate_next_row(row, sb)
        row = strings.to_string(sb^)
        safe_tile_count += count_safe_tiles(row)
        i += 1
        sb = &sb2 if sb == &sb1 else &sb1
    }
    fmt.printf("Part 2: %d\n", safe_tile_count)
}

generate_next_row :: proc(prev: Row, sb: ^strings.Builder) {
    strings.reset_builder(sb)
    for _, i in prev {
        fmt.sbprintf(sb, "%c", tile_type(prev, i))
    }
}

tile_type :: proc(prev: Row, i: int) -> Tile {
    left   := SAFE if i == 0 else prev[i-1]
    center := prev[i]
    right  := SAFE if i == len(prev)-1 else prev[i+1]

    trap := (left == TRAP && center == TRAP && right == SAFE) \
         || (left == SAFE && center == TRAP && right == TRAP) \
         || (left == TRAP && center == SAFE && right == SAFE) \
         || (left == SAFE && center == SAFE && right == TRAP)
        
    return trap ? TRAP : SAFE
}

count_safe_tiles :: proc(row: Row) -> int {
    result := 0
    for _, i in row {
        if row[i] == SAFE do result += 1
    }
    return result
}

read_input :: proc() -> (row: string, count_part1, count_part2: int) {
    row = read_stdin()
    count_part1, count_part2 = read_args()
    return
}

read_stdin :: proc() -> string {
    r: bufio.Reader
    buffer: [1024]byte
    bufio.reader_init_with_buf(&r, {os.stream_from_handle(os.stdin)}, buffer[:])
    defer bufio.reader_destroy(&r)

    line, err := bufio.reader_read_string(&r, '\n')
    if err != nil do fmt.panicf("ERROR: Unable to parse stdin")
    line = strings.trim_right(line, "\n")
    return line
}

read_args :: proc() -> (count_part1, count_part2: int) {
    ok: bool

    count_part1, ok = strconv.parse_int(os.args[1], 10)
    if !ok do fmt.panicf("ERROR: Unable to parse args")

    count_part2, ok = strconv.parse_int(os.args[2], 10)
    if !ok do fmt.panicf("ERROR: Unable to parse args")

    return
}
