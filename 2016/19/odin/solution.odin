package main

import "core:fmt"
import "core:os"
import "core:strconv"


// ----------------------------------------------------------------------------
// Solution

main :: proc() {
    elf_count := parse_input()

    winner := play_incorrect_rules(elf_count)
    fmt.printf("Part 1: %d\n", winner)

    winner = play_correct_rules(elf_count)
    fmt.printf("Part 2: %d\n", winner)
}

play_incorrect_rules :: proc(elf_count: int) -> (winner: int) {
    q := make_queue(elf_count)
    defer destroy_queues(&q)

    for {
        curr := dequeue(&q)

        // See if we have a winner...
        if is_empty(q) do return curr

        // Take from the next elf...
        dequeue(&q)

        // Add the current elf back to the end of the play queue...
        enqueue(&q, curr)
    }
}

play_correct_rules :: proc(elf_count: int) -> (winner: int) {
    left, right := make_split_queues(elf_count)
    defer destroy_queues(&left, &right)

    for {
        // Take from the elf across from the current elf...
        dequeue(&right)

        // See if we have a winner...
        if is_empty(right) do return dequeue(&left)

        // Add the current elf back to the end of the play queue
        // and queue up the next "across" elf...
        if queue_len(left) == queue_len(right) {
            enqueue(&right, dequeue(&left))
            enqueue(&left, dequeue(&right))
        } else {
            enqueue(&right, dequeue(&left))
        }
    }
}


// ----------------------------------------------------------------------------
// Input Parsing

parse_input :: proc() -> int {
    elf_count, ok := strconv.parse_int(os.args[1], 10)
    if !ok do fmt.panicf("ERROR: Unable to parse arguments\n")
    return elf_count
}


// ----------------------------------------------------------------------------
// Queue

Queue :: struct {
    data: []int,
    head: int,
    tail: int,
}

make_queue :: proc(count: int) -> Queue {
    q: Queue
    q.data = make([]int, count)
    for i in 1..=count do enqueue(&q, i)
    return q
}

make_split_queues :: proc(count: int) -> (left: Queue, right: Queue) {
    left.data = make([]int, count)
    right.data = make([]int, count)

    for i in 1..=count {
        if i <= count/2 do enqueue(&left, i)
        else do enqueue(&right, i)
    }

    return
}

destroy_queues :: proc(qs: ..^Queue) {
    for q in qs do delete(q.data)
}

is_empty :: proc(using q: Queue) -> bool {
    return head == tail
}

queue_len :: proc(using q: Queue) -> int {
    if tail >= head {
        return tail - head
    } else {
        return tail + (len(data) - head)
    }
}

enqueue :: proc(using q: ^Queue, n: int) {
    data[tail] = n
    tail = (tail + 1) % len(data)
}

dequeue :: proc(using q: ^Queue) -> int {
    result := data[head]
    head = (head + 1) % len(data)
    return result
}
