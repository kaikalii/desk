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

    // Debug tokens
    std.debug.print("tokens:\n", .{});
    for (lexed.tokens.items) |token| {
        std.debug.print("  {}\n", .{token});
    }
    std.debug.print("\n", .{});

    // Parse
    const ast = parse.parse(&lexed, alloc);
    defer ast.deinit();

    for (ast.errors.items) |err| {
        std.debug.print("{}\n", .{err});
    }
}
