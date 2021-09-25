package main

import "core:fmt"
import "core:slice"
import "core:strings"
import "core:testing"

// -----------------------------------------------------------------------------
// Main

main :: proc() {
    input := string(#load("../input.txt"))

    bots := make([dynamic]Bot)
    defer delete(bots)

    outputs := make(map[int]int)
    defer delete(outputs)

    parse_input(&bots, input)

    bot_id := run_until_no_more_swaps(bots[:], &outputs, 17, 61)

    fmt.println("Part 1:", bot_id)
    fmt.println("Part 2:", outputs[0] * outputs[1] * outputs[2])
}

// -----------------------------------------------------------------------------
// Types

Bot :: struct {
    id: int,
    microchips: [2]int,
    low: Destination,
    high: Destination,
}

Destination :: struct {
    type: Destination_Type,
    id: int,
}

Destination_Type :: enum { Bot, Output }

// -----------------------------------------------------------------------------
// Puzzle Logic

run_until_no_more_swaps :: proc(bots: []Bot, outputs: ^map[int]int, sought_low, sought_high: int) -> (bot_id: int) {
    should_continue := true
    for should_continue {
        id, found, give_count := tick(bots, outputs, sought_low, sought_high)
        if found do bot_id = id
        should_continue = give_count > 0
    }

    return
}

tick :: proc(bots: []Bot, outputs: ^map[int] int, sought_low, sought_high: int) -> (id: int, found: bool, give_count: int) {
    give_count = 0

    for _, i in bots {
        bot := &bots[i]
        if has_two_microchips(bot) {
            give_count += 1

            low, high := give_microchips(bot, bots, outputs)
            if low == sought_low && high == sought_high {
                id = bot.id
                found = true
            }
        }
    }

    return
}

has_two_microchips :: proc(using bot: ^Bot) -> bool {
    return microchips[0] != 0 && microchips[1] != 0
}

give_microchips :: proc(using bot: ^Bot, bots: []Bot, outputs: ^map[int]int) -> (low_microchip: int, high_microchip: int) {
    low_microchip = slice.min(microchips[0:len(microchips)])
    high_microchip = slice.max(microchips[0:len(microchips)])

    switch low.type {
    case .Bot:
        destination_bot := find_bot(bots, low.id)
        take_microchip(destination_bot, low_microchip)

    case .Output:
        outputs[low.id] = low_microchip
    }

    switch high.type {
    case .Bot:
        destination_bot := find_bot(bots, high.id)
        take_microchip(destination_bot, high_microchip)

    case .Output:
        outputs[high.id] = high_microchip
    }

    microchips[0] = 0
    microchips[1] = 0

    return
}

find_bot :: proc(bots: []Bot, id: int) -> ^Bot {
    for _, i in bots {
        bot := &bots[i]
        if bot.id == id do return bot
    }

    return nil
}

find_or_create_bot :: proc(bots: ^[dynamic]Bot, id: int) -> ^Bot {
    bot := find_bot(bots[:], id)
    if bot != nil do return bot

    append(bots, Bot{id=id})
    return &bots[len(bots)-1]
}

take_microchip :: proc(using bot: ^Bot, microchip: int) {
    if microchips[0] == 0 {
        microchips[0] = microchip
    } else if microchips[1] == 0 {
        microchips[1] = microchip
    } else {
        panic("Tried to give microchip to a bot that already has two.")
    }
}

// -----------------------------------------------------------------------------
// Parsing

parse_input :: proc(bots: ^[dynamic]Bot, input: string) {
    input := input

    rest := &input
    for line, ok := strings.split_iterator(rest, "\n"); ok; line, ok = strings.split_iterator(rest, "\n") {
        switch {
        case strings.has_prefix(line, "value"):
            bot_id, microchip := parse_value_line(line)
            bot := find_or_create_bot(bots, bot_id)
            take_microchip(bot, microchip)

        case strings.has_prefix(line, "bot"):
            bot_id, low, high := parse_instruction_line(line)
            bot := find_or_create_bot(bots, bot_id)
            bot.low = low
            bot.high = high

        case:
            fmt.panicf("Unexpected line: %s\n", line)
        }
    }
}

parse_value_line :: proc(line: string) -> (bot_id, microchip: int) {
    line := line

    // Value lines look like this:
    // value <microchip> goes to bot <bot_id>

    // Skip "value "
    line = line[6:]
    microchip = read_next_int(&line)

    // Skip " goes to bot "
    line = line[13:]
    bot_id = read_next_int(&line)

    return
}

parse_instruction_line :: proc(line: string) -> (bot_id: int, low, high: Destination) {
    line := line

    // Instruction lines look like this:
    // bot <bot_id> gives low to (bot|output) <low> and high to (bot|output) <high>

    // Skip "bot "
    line = line[4:]
    bot_id = read_next_int(&line)

    // Skip " gives low to "
    line = line[14:]
    if strings.has_prefix(line, "output ") {
        low.type = .Output
        line = line[7:]
    } else if strings.has_prefix(line, "bot ") {
        low.type = .Bot
        line = line[4:]
    }
    low.id = read_next_int(&line)

    // Skip " and high to "
    line = line[13:]
    if strings.has_prefix(line, "output ") {
        high.type = .Output
        line = line[7:]
    } else if strings.has_prefix(line, "bot ") {
        high.type = .Bot
        line = line[4:]
    }
    high.id = read_next_int(&line)

    return
}

read_next_int :: proc(s: ^string) -> int {
    num: int
    chars_read: int

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

    s^ = s[chars_read:]
    return num
}

// -----------------------------------------------------------------------------
// Tests

@test
test_part1_example :: proc(t: ^testing.T) {
    input := `value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2`

    bots := make([dynamic]Bot)
    defer delete(bots)

    outputs := make(map[int]int)
    defer delete(outputs)

    parse_input(&bots, input)

    bot_id := run_until_no_more_swaps(bots[:], &outputs, 2, 5)
    testing.expect(t, bot_id == 2)
}
