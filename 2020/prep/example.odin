package main

import "core:fmt"
import "core:os"

main :: proc() {
	{
		char_file := open_input("chars.txt");
		defer os.close(char_file);

		for {
			char, done := next_char(char_file);
			if done do break;

			fmt.printf("CHAR: %c\n", char);
		}
	}

	{
		line_file := open_input("lines.txt");
		defer os.close(line_file);

		buffer: [100]u8;
		for {
			line, done := next_line(line_file, buffer[:]);
			if done do break;

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


next_char :: proc(fd: os.Handle) -> (char: u8, done: bool) {
	buffer: [1]u8;

	bytes_read, err := os.read(fd, buffer[:]);
	if err != os.ERROR_NONE {
		fmt.fprintln(os.stderr, "ERROR: Couldn't read from input file");
		os.exit(1);
	}

	if bytes_read == 0 {
		return 0, true;
	}

	return buffer[0], false;
}


next_line :: proc(fd: os.Handle, buffer: []u8) -> (line: string, done: bool) {
	index := 0;
	for {
		char, done := next_char(fd);
		if done {
			return string(buffer[:index]), true;
		}

		if char == '\n' {
			return string(buffer[:index]), false;
		}

		if index > len(buffer) {
			fmt.fprintf(os.stderr, "ERROR: Buffer too small\n");
			return "", true;
		}

		buffer[index] = char;
		index += 1;
	}
	unreachable();
}
