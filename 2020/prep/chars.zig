const std = @import("std");
const fs  = std.fs;
const io  = std.io;

pub fn main() !void {
    const input_file = try fs.cwd().openFile("chars.txt", .{});
    defer input_file.close();

    var elevator = Elevator{};
    var monitor = Monitor{};

    const input = input_file.reader();
    while (true) {
        const char = input.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        elevator.move(char);
        monitor.observe(elevator);
    }

    const stdout = io.getStdOut().writer();
    try stdout.print("Part 1: {d}\n", .{elevator.floor});
    try stdout.print("Part 2: {d}\n", .{monitor.basement});
}

const Elevator = struct {
    floor: i64 = 0,

    pub fn move(self: *Elevator, char: u8) void {
        switch (char) {
            '(' => self.floor += 1,
            ')' => self.floor -= 1,
            else => {},
        }
    }
};

const Monitor = struct {
    index: u64 = 1,
    basement: u64 = 0,

    pub fn observe(self: *Monitor, elevator: Elevator) void {
        if (self.basement != 0) {
            return;
        }
        
        if (elevator.floor < 0) {
            self.basement = self.index;
        }

        self.index += 1;
    }
};
