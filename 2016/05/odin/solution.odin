package main

import "core:fmt"
import "core:crypto/md5"
import "core:os"

main :: proc() {
    door_id := read_input()

    password1 : [8]u8
    char1     : u8
    password2 : [8]u8
    char2     : u8
    index     : int

    fmt.printf("\r%x %x", password1, password2)

    for char1 < 8 || char2 < 8 {
        input := fmt.tprintf("%s%d", door_id, index)
        hash  := md5.hash(input)

        if hash[0] == 0 && hash[1] == 0 && (hash[2] & 0xF0) == 0 {
            if char1 < 8 {
                password1[char1] = hash[2]
                char1 += 1
                fmt.printf("\r%x %x", password1, password2)
            }

            i := hash[2]
            if char2 < 8 && i <= 7 && password2[i] == 0 {
                password2[i] = hash[3] >> 4
                char2 += 1
                fmt.printf("\r%x %x", password1, password2)
            }
        }

        free_all(context.temp_allocator)
        index += 1
    }

    fmt.printf("\n")

    fmt.printf("Part 1: %x\n", password1)
    fmt.printf("Part 2: %x\n", password2)
}

read_input :: proc() -> [8]u8 {
    input: [8]u8

    f, oerr := os.open("../input.txt", os.O_RDONLY, 0)
    if oerr != os.ERROR_NONE do fmt.panicf("Unable to read the input file.")
    defer os.close(f)

    count, rerr := os.read(f, input[:])
    if rerr != os.ERROR_NONE do fmt.panicf("Unable to read the input file.")
    if count != 8 do fmt.panicf("Unable to read the input file.")

    return input
}
