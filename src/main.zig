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

    // Parse
    const ast = parse.parse(&lexed, alloc);
    defer ast.deinit();

    if (ast.errors.items.len == 0) {
        for (ast.items.items) |item|
            std.debug.print("{}\n", .{item});
    } else {
        for (ast.errors.items) |err|
            std.debug.print("{}\n", .{err});
    }
}
