package main

import "core:bytes"
import "core:fmt"
import "core:io"
import "core:os"
import "core:strings"


BUFFER_SIZE :: 50


// ----------------------------------------------------------------------------
// Types

Operation :: union {
	Swap_Position,
	Swap_Letter,
	Reverse,
	Rotate_Left,
	Rotate_Right,
	Rotate_Letter,
	Move
}

Move          :: struct { x, y: int }
Reverse       :: struct { x, y: int }
Rotate_Left   :: struct { steps: int }
Rotate_Right  :: struct { steps: int }
Rotate_Letter :: struct { letter: u8 }
Swap_Letter   :: struct { x, y: u8 }
Swap_Position :: struct { x, y: int }


// ----------------------------------------------------------------------------
// Solution

main :: proc() {
	defer free_all(context.allocator)

	password, scrambled, operations := parse_input()

	part1 := scramble(password, operations)
	fmt.printf("Part 1: %s\n", part1)

	part2: string
	for {
		permute(password)
		if scramble(password, operations) == scrambled {
			part2 = password
			break
		}
	}
	fmt.printf("Part 2: %s\n", part2)
}

scramble :: proc(password: string, operations: []Operation) -> string {
	password := strings.clone(password)
	b := transmute([]u8)password

	for oper in operations {
		execute(b, oper)
	}

	return string(b)
}

execute :: proc(buffer: []u8, oper: Operation) {
	switch o in oper {
	case Move: move_position(buffer, o.x, o.y)
	case Reverse: reverse_positions(buffer, o.x, o.y)
	case Rotate_Left: rotate_left(buffer, o.steps)
	case Rotate_Right: rotate_right(buffer, o.steps)
	case Rotate_Letter: rotate_on_letter(buffer, o.letter)
	case Swap_Letter: swap_letter(buffer, o.x, o.y)
	case Swap_Position: swap_position(buffer, o.x, o.y)
	}
}

move_position :: proc(s: []u8, x, y: int) {
	buffer: [BUFFER_SIZE]u8
	moved := s[x]
	for si, bi := 0, 0; bi < len(s); {
		switch {
		case si == x:
			si += 1
		case bi == y:
			buffer[bi] = moved
			bi += 1
		case:
			buffer[bi] = s[si]
			si += 1
			bi += 1
		}
	}

	for i in 0..<len(s) do s[i] = buffer[i]
}

reverse_positions :: proc(s: []u8, x, y: int) {
	xi, yi := x, y
	for xi < yi {
		swap_position(s, xi, yi)
		xi += 1
		yi -= 1
	}
}

swap_position :: proc(s: []u8, x, y: int) {
	temp := s[x]
	s[x] = s[y]
	s[y] = temp
}

rotate_left :: proc(s: []u8, steps: int) {
	buffer: [BUFFER_SIZE]u8
	steps := steps % len(s)
	i, j := 0, steps
	for {
		buffer[i] = s[j]
		i += 1
		j = (j + 1) % len(s)
		if j == steps do break
	}

	for j in 0..<i do s[j] = buffer[j]
}

rotate_right :: proc(s: []u8, steps: int) {
	buffer: [BUFFER_SIZE]u8
	start := (len(s) - steps) % len(s)
	if start < 0 do start = len(s) + start
	i, j := 0, start
	for {
		buffer[i] = s[j]
		i += 1
		j = (j + 1) % len(s)
		if j == start do break
	}

	for j in 0..<i do s[j] = buffer[j]
}

rotate_on_letter :: proc(s: []u8, letter: u8) {
	index := bytes.index_byte(s, letter)
	extra := index >= 4 ? 1 : 0
	rotate_right(s, 1 + index + extra)
}

swap_letter :: proc(s: []u8, x, y: u8) {
	xi, yi: int
	for c, i in s {
		if c == x do xi = i
		if c == y do yi = i
		if xi > 0 && yi > 0 do break
	}
	swap_position(s, xi, yi)
}


// ----------------------------------------------------------------------------
// Permutations

permute :: proc(current: string) {
	current := transmute([]u8)current

    j := len(current) - 2
    k := len(current) - 1

    for current[j] > current[j+1] do j -= 1
    for current[j] > current[k]   do k -= 1

    swap(current, j, k)

    r := len(current) - 1
    s := j + 1

    for r > s {
        swap(current, r, s)
        r -= 1
        s += 1
    }
}

swap :: proc(data: []u8, j, k: int) {
	tmp := data[j]
	data[j] = data[k]
	data[k] = tmp
}


// ----------------------------------------------------------------------------
// Input Parsing

parse_input :: proc() -> (password, scrambled: string, operations: []Operation) {
	password  = os.args[1]
	scrambled = os.args[2]

    in_stream := os.stream_from_handle(os.stdin)
    in_reader := io.to_byte_reader(in_stream)
	iter := Operation_Iterator{in_reader}
	ops: [dynamic]Operation
	for oper in operation_iterator(iter) {
		append(&ops, oper)
	}
	operations = ops[:]

	return
}

Operation_Iterator :: struct {
	input: io.Byte_Reader,
}

operation_iterator :: proc(iter: Operation_Iterator) -> (Operation, bool) {
	buffer: [BUFFER_SIZE]u8
	result: Operation

    line := read_line(buffer[:], iter.input)
	if len(line) == 0 do return result, false

	switch {
	// "move position X to position Y"
	case strings.has_prefix(line, "move"):
		n, x, y: int
		line = line[len("move position "):]
		n, x = read_int(line)
		line = line[n + len(" to position "):]
		n, y = read_int(line)
		result = Move{x, y}

	// "reverse positions X through Y"
	case strings.has_prefix(line, "reverse"):
		n, x, y: int
		line = line[len("reverse positions "):]
		n, x = read_int(line)
		line = line[n + len(" through "):]
		n, y = read_int(line)
		result = Reverse{x, y}

	// "rotate based on position of letter X"
	case strings.has_prefix(line, "rotate based"):
		line = line[len("rotate based on position of letter "):]
		x := line[0]
		result = Rotate_Letter{x}

	// "rotate left X steps"
	case strings.has_prefix(line, "rotate left"):
		line = line[len("rotate left "):]
		n, x := read_int(line)
		result = Rotate_Left{x}

	// "rotate right X steps"
	case strings.has_prefix(line, "rotate right"):
		line = line[len("rotate right "):]
		n, x := read_int(line)
		result = Rotate_Right{x}

	// "swap letter X with letter Y"
	case strings.has_prefix(line, "swap letter "):
		line = line[len("swap letter "):]
		x := line[0]
		line = line[1 + len(" with letter "):]
		y := line[0]
		result = Swap_Letter{x, y}

	// "swap position X with position Y"
	case strings.has_prefix(line, "swap position "):
		n, x, y: int
		line = line[len("swap position "):]
		n, x = read_int(line)
		line = line[n + len(" with position "):]
		n, y = read_int(line)
		result = Swap_Position{x, y}
	}

	return result, true
}

read_line :: proc(buffer: []u8, r: io.Byte_Reader) -> string {
	i := 0
	for {
		byte, err := io.read_byte(r)
        if err == .EOF || byte == 0 || byte == '\n' do break
        if err != .None do fmt.panicf("ERROR: Unable to parse input")

		buffer[i] = byte
		i += 1
	}
	return string(buffer[:i])
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
