const std = @import("std");
const fs  = std.fs;
const io  = std.io;

const N = 0;
const E = 1;
const S = 2;
const W = 3;

pub fn main() !void {
    const stdout = io.getStdOut().writer();
    const input  = try InputFile.new("../input.txt");
    defer input.close();

    var dir: u8 = 0;
    var pos: Point = .{ .x = 0, .y = 0 };

    var buffer: [100_000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var visited = std.AutoHashMap(Point, bool).init(&fba.allocator);
    var revisit: ?Point = null;

    while (try input.nextChar()) |char| {
        if (char == ',' or char == ' ' or char == '\n') { continue; }
        const steps = try input.nextInt(i32);

        switch (char) {
            'L'  => dir = if (dir == 0) 3 else dir - 1,
            'R'  => dir = if (dir == 3) 0 else dir + 1,
            else => {},
        }

        var p = pos;

        switch (dir) {
            N => pos.y += steps,
            E => pos.x += steps,
            S => pos.y -= steps,
            W => pos.x -= steps,
            else => {},
        }

        if (revisit == null) {
            while (!p.equals(pos)) {
                if (visited.contains(p)) {
                    revisit = p;
                    break;
                }

                try visited.put(p, true);

                switch (dir) {
                    N => p.y += 1,
                    E => p.x += 1,
                    S => p.y -= 1,
                    W => p.x -= 1,
                    else => {},
                }
            }
        }
    }

    try stdout.print("Part 1: {d}\n", .{pos.distance()});
    try stdout.print("Part 2: {d}\n", .{(revisit.?).distance()});
}

const Point = struct {
    x: i32,
    y: i32,

    fn distance(self: Point) i32 {
        const x = std.math.absInt(self.x) catch 0;
        const y = std.math.absInt(self.y) catch 0;
        return x + y;
    }

    fn equals(self: Point, other: Point) bool {
        return (self.x == other.x) and (self.y == other.y);
    }
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

    fn nextLine(self: *const InputFile, buffer: []u8) !?[]u8 {
        return try self.reader.readUntilDelimiterOrEof(buffer, '\n');
    }

    fn nextInt(self: *const InputFile, comptime T: type) !T {
        var n: T = 0;
        while (try self.nextChar()) |char| {
            if (char < '0' or char > '9') { break; }

            n *= 10;
            n += char - '0';
        }

        return n;
    }
};
