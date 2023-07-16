const std = @import("std");
const lex = @import("lex.zig");
const parse = @import("parse.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer switch (gpa.deinit()) {
        .ok => {},
        .leak => std.debug.print("leaked memory\n", .{}),
    };

    // Lex
    const lexed = try lex.lexFile("examples/test.desk", alloc);
    defer lexed.deinit();
    std.debug.print("input:\n{s}", .{lexed.input});

    std.debug.print("\n\ntokens:", .{});
    for (lexed.tokens.items) |token| {
        std.debug.print("\n  {}", .{token});
    }
}
