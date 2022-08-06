package main

import "core:crypto/md5"
import "core:fmt"
import "core:strings"

Direction :: enum { Up, Down, Left, Right }

Point :: struct { x, y: uint }

input :: "pvhmgsws"

main :: proc() {
    queue: Queue
    enqueue(&queue, State{})

    shortest_path: Path
    longest_path_len: uint
    for !queue_is_empty(&queue) {
        state := dequeue(&queue)

        if state.point.x == 3 && state.point.y == 3 {
            if shortest_path.len == 0 {
                shortest_path = state.path
            }
            if state.path.len > longest_path_len {
                longest_path_len = state.path.len
            }

            continue
        }

        next_directions := possible_next_directions(&state)
        for i in 0..<4 {
            dir := Direction(i)
            if dir in next_directions {
                new_state := updated_state(state, dir)
                enqueue(&queue, new_state)
            }
        }
    }

    shortest_path_string := path_str(&shortest_path)
    defer delete(shortest_path_string)

    fmt.printf("Part 1: %s\n", shortest_path_string)
    fmt.printf("Part 2: %d\n", longest_path_len)
}

possible_next_directions :: proc(using s: ^State) -> bit_set[Direction] {
    context.allocator = context.temp_allocator
    defer free_all(context.temp_allocator)

    input_with_path := fmt.aprintf("%s%s", input, path_str(&path))
    hash := md5_string(input_with_path)

    using Direction
    dirs: bit_set[Direction]

    if is_open(hash[int( Up    )]) && point.y > 0 do incl(&dirs, Up)
    if is_open(hash[int( Down  )]) && point.y < 3 do incl(&dirs, Down)
    if is_open(hash[int( Left  )]) && point.x > 0 do incl(&dirs, Left)
    if is_open(hash[int( Right )]) && point.x < 3 do incl(&dirs, Right)

    return dirs
}

md5_string :: proc(input: string, allocator := context.allocator) -> string {
    sb := strings.make_builder(allocator=allocator)
    for b in md5.hash(input) {
        fmt.sbprintf(&sb, "%02x", b)
    }
    return strings.to_string(sb)
}

is_open :: proc(char: u8) -> bool {
    switch char {
    case 'b'..='f':
        return true
    case:
        return false
    }
}


// ------------------------------------------------------------------
// State

State :: struct {
    point: Point,
    path: Path,
}

updated_state :: proc(orig: State, d: Direction) -> State {
    updated := orig
    append_path(&updated.path, d)

    switch d {
    case .Up:    updated.point.y -= 1
    case .Down:  updated.point.y += 1
    case .Left:  updated.point.x -= 1
    case .Right: updated.point.x += 1
    }

    return updated
}


// ------------------------------------------------------------------
// Path

Path :: struct {
    dirs: [1000]Direction,
    len: uint,
}

append_path :: proc(using p: ^Path, d: Direction) {
    dirs[len] = d
    len += 1
}

path_str :: proc(using p: ^Path, allocator := context.allocator) -> string {
    sb := strings.make_builder(allocator=allocator)
    for i in 0..<len {
        switch dirs[i] {
        case .Up:    fmt.sbprint(&sb, 'U')
        case .Down:  fmt.sbprint(&sb, 'D')
        case .Left:  fmt.sbprint(&sb, 'L')
        case .Right: fmt.sbprint(&sb, 'R')
        }
    }
    return strings.to_string(sb)
}


// ------------------------------------------------------------------
// Queue

Queue :: struct {
    data: [1000]State,
    head: uint,
    tail: uint,
}

queue_len :: proc(using q: Queue) -> uint {
    if tail > head {
        return tail - head
    } else {
        return tail + (len(data) - head)
    }
}

queue_is_empty :: proc(using q: ^Queue) -> bool {
    return head == tail
}

enqueue :: proc(using q: ^Queue, s: State) {
    data[tail] = s
    tail = (tail + 1) % len(data)
    assert(tail != head)
}

dequeue :: proc(using q: ^Queue) -> State {
    result := data[head]
    head = (head + 1) % len(data)
    return result
}
