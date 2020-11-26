const std = @import("std");
const fmt = std.fmt;
const fs  = std.fs;
const io  = std.io;

pub fn main() !void {
    const stdout = io.getStdOut().writer();

    {
        const char_file = try InputFile.new("chars.txt");
        defer char_file.close();

        while (try char_file.nextChar()) |char| {
            try stdout.print("CHAR: {c}\n", .{char});
        }
    }

    {
        const line_file = try InputFile.new("lines.txt");
        defer line_file.close();

        var buffer: [100]u8 = undefined;
        while (try line_file.nextLine(buffer[0..])) |line| {
            try stdout.print("LINE: {s}\n", .{line});
        }
    }

    try stdout.print("\n", .{});

    {
        const number_file = try InputFile.new("numbers.txt");
        defer number_file.close();

        var buffer: [100]u8 = undefined;
        while (try number_file.nextInt(i64, buffer[0..])) |n| {
            try stdout.print("NUMBER: {d}\n", .{n});
        }
    }
}


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

    fn nextInt(self: *const InputFile, comptime T: type, buffer: []u8) !?T {
        const string = try self.reader.readUntilDelimiterOrEof(buffer, ' ');
        if (string) |s| {
            return try fmt.parseInt(T, s, 10);
        } else {
            return null;
        }
    }
};
