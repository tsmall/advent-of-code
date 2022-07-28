package main

import "core:fmt"

input :: []u8{ 0,1,0,0,0,1,0,0,0,1,0,0,1,0,1,1,1 }
disk1_length :: 272
disk2_length :: 35_651_584

main :: proc() {
    data_buffer := make([]u8, disk2_length*2)
    checksum_buffer := make([]u8, disk2_length*2)

    data := generate_data(input, disk1_length, data_buffer)
    checksum := generate_checksum(data, checksum_buffer)
    fmt.print("Part 1: ")
    for b in checksum do fmt.printf("%d", b)
    fmt.print("\n")

    data = generate_data(input, disk2_length, data_buffer)
    checksum = generate_checksum(data, checksum_buffer)
    fmt.print("Part 2: ")
    for b in checksum do fmt.printf("%d", b)
    fmt.print("\n")
}

generate_data :: proc(input: []u8, length: int, buffer: []u8) -> []u8 {
    data := input

    for {
        data = apply_dragon_curve(data, buffer)
        if len(data) >= length do return data[0:length]
    }
}

apply_dragon_curve :: proc(bits: []u8, buffer: []u8) -> []u8 {
    i: int

    // Curve starts with original bits ...
    for i = 0; i < len(bits); i += 1 {
        buffer[i] = bits[i]
    }

    // Then a single 0 ...
    buffer[i] = 0

    // Then bits reversed and flipped ...
    for j := len(bits) - 1; j >= 0; j -= 1 {
        i += 1
        buffer[i] = (bits[j] == 0) ? 1 : 0
    }

    return buffer[0:i+1]
}

generate_checksum :: proc(data: []u8, buffer: []u8) -> []u8 {
    assert(len(data) % 2 == 0)

    data := data
    buffer_i: int

    for {
        buffer_i = 0
        for i := 0; i < len(data) - 1; i += 2 {
            a := data[i]
            b := data[i+1]
    
            buffer[buffer_i] = (a == b) ? 1 : 0
            buffer_i += 1
        }

        data = buffer[0:buffer_i]
        if len(data) % 2 == 1 do return data
    }
}
