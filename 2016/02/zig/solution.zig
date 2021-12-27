const std = @import("std");
const fs  = std.fs;
const io  = std.io;

const keypad1 = [3][3]u8{
    [_]u8{ 1, 2, 3 },
    [_]u8{ 4, 5, 6 },
    [_]u8{ 7, 8, 9 },
};

const keypad2 = [5][5]u8{
    [_]u8{ ' ', ' ', '1', ' ', ' ' },
    [_]u8{ ' ', '2', '3', '4', ' ' },
    [_]u8{ '5', '6', '7', '8', '9' },
    [_]u8{ ' ', 'A', 'B', 'C', ' ' },
    [_]u8{ ' ', ' ', 'D', ' ', ' ' },
};

pub fn main() !void {
    const stdout = io.getStdOut().writer();
    const input  = try InputFile.new("../input.txt");
    defer input.close();

    var p1 = Point{ .x = 1, .y = 1 };
    var code1 = [_]u8{0} ** 5;
    var code1_i: u8 = 0;

    var p2 = Point{ .x = 0, .y = 2 };
    var code2 = [_]u8{' '} ** 5;
    var code2_i: u3 = 0;

    while (try input.nextChar()) |char| {
        switch (char) {
            'U' => {
                if (p1.y > 0) { p1.y -= 1; }
                if (p2.y > 0 and keypad2[p2.y-1][p2.x] != ' ') { p2.y -= 1; }
            },
            'D' => {
                if (p1.y < 2) { p1.y += 1; }
                if (p2.y < 4 and keypad2[p2.y+1][p2.x] != ' ') { p2.y += 1; }
            },
            'L' => {
                if (p1.x > 0) { p1.x -= 1; }
                if (p2.x > 0 and keypad2[p2.y][p2.x-1] != ' ') { p2.x -= 1; }
            },
            'R' => {
                if (p1.x < 2) { p1.x += 1; }
                if (p2.x < 4 and keypad2[p2.y][p2.x+1] != ' ') { p2.x += 1; }
            },
            '\n' => {
                code1[code1_i] = keypad1[p1.y][p1.x]; code1_i += 1;
                code2[code2_i] = keypad2[p2.y][p2.x]; code2_i += 1;
            },
            else =>
                try stdout.print("ERROR: Invalid char: {c}\n", .{ char }),
        }
    }

    try stdout.print("Part 1: {d}\n", .{ code1 });
    try stdout.print("Part 2: {c}\n", .{ code2 });
}

const Point = struct {
    x: u8,
    y: u8,
};

const InputFile = struct {
    file: fs.File,
    reader: fs.File.Reader,

    fn new(path: []const u8) !InputFile {
        const file = try fs.cwd().openFile(path, .{});
        return InputFile {
            .file = file,
            .reader = file.reader(),
        };
    }

    fn close(self: *const InputFile) void {
        self.file.close();
    }

    fn nextChar(self: *const InputFile) !?u8 {
        const char = self.reader.readByte() catch |err| switch (err) {
            error.EndOfStream => return null,
            else => |e| return e,
        };

        return char;
    }
};
