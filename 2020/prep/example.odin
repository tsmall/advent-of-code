package main

import "core:fmt"
import "core:os"

main :: proc() {
	{
		char_file := open_input("chars.txt");
		defer os.close(char_file);

		for {
			char, ok := next_char(char_file);
			if !ok do break;

			fmt.printf("CHAR: %c\n", char);
		}
	}

	{
		line_file := open_input("lines.txt");
		defer os.close(line_file);

		buffer: [100]u8;
		for {
			line, ok := next_line(line_file, buffer[:]);
			if !ok do break;

			fmt.printf("LINE: %s\n", line);
		}
	}
}


open_input :: proc(path: string) -> os.Handle {
	file, err := os.open(path);
	if err != os.ERROR_NONE {
		fmt.fprintln(os.stderr, "ERROR: Couldn't open input file");
		os.exit(1);
	}

	return file;
}


next_char :: proc(fd: os.Handle) -> (u8, bool) {
	buffer: [1]u8;

	bytes_read, err := os.read(fd, buffer[:]);
	if err != os.ERROR_NONE {
		fmt.fprintln(os.stderr, "ERROR: Couldn't read from input file");
		os.exit(1);
	}

	if bytes_read == 0 {
		return 0, false;
	}

	return buffer[0], true;
}


next_line :: proc(fd: os.Handle, buffer: []u8) -> (string, bool) {
	index := 0;
	for {
		char, ok := next_char(fd);
		if !ok {
			return string(buffer[:index]), false;
		}

		if char == '\n' {
			return string(buffer[:index]), true;
		}

		if index > len(buffer) {
			fmt.fprintf(os.stderr, "ERROR: Buffer too small\n");
			return "", false;
		}

		buffer[index] = char;
		index += 1;
	}
	unreachable();
}
