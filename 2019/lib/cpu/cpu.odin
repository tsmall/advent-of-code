package cpu

import "core:fmt"

CPU :: struct {
    pc: uint,
    memory: []int,
}

run_program :: proc(memory: []int, noun: int, verb: int) -> int {
    memory[1] = noun
    memory[2] = verb

    cpu := CPU{pc=0, memory=memory}
    run(&cpu)

    return memory[0]
}

run :: proc(using cpu: ^CPU) {
    loop: for {
        switch opcode := memory[pc]; opcode {
        case 1:
            x_addr := memory[pc + 1]
            y_addr := memory[pc + 2]
            out_addr := memory[pc + 3]
            memory[out_addr] = memory[x_addr] + memory[y_addr]
            pc += 4
        case 2:
            x_addr := memory[pc + 1]
            y_addr := memory[pc + 2]
            out_addr := memory[pc + 3]
            memory[out_addr] = memory[x_addr] * memory[y_addr]
            pc += 4
        case 99:
            break loop
        case:
            fmt.panicf("Encountered an unknown opcode: %d\n", opcode)
        }
    }
}

parse_program :: proc(input: []u8) -> []int {
    program: [dynamic]int
    for n, rest := read_int(input); len(rest) > 0; n, rest = read_int(rest) {
        append(&program, n)
        rest = rest[1:]
    }

    return program[:]
}

read_int :: proc(data: []u8) -> (n: int, rest: []u8) {
    rest = data
    for len(rest) > 0 && rest[0] >= '0' && rest[0] <= '9' {
        n *= 10
        n += int(rest[0] - '0')
        rest = rest[1:]
    }
    return
}
