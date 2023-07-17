const std = @import("std");
const lex = @import("lex.zig");
const Sp = lex.Sp;
const Span = lex.Span;
const Token = lex.Token;
const TokenTy = lex.TokenTy;

pub const Ast = struct {
    items: std.ArrayList(Item),
    errors: std.ArrayList(ParseError),
    arena: std.heap.ArenaAllocator,
    pub fn deinit(self: *const Ast) void {
        self.arena.deinit();
    }
};

pub const ItemTy = enum {
    layout,
};

pub const Item = union(ItemTy) {
    layout: Layout,
};

pub const Layout = struct {
    name: []const u8,
    name_span: Span,
    fields: std.ArrayList(Field),
};

pub const Field = struct {
    name: []const u8,
    ty: Type,
};

pub const Type = []const u8;

pub const ExpectationTy = enum {
    token,
    name,
    field,
    ty,
};

pub const Expectation = union(ExpectationTy) {
    token: TokenTy,
    name,
    field,
    ty,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .token => |token_ty| try writer.print("{}", .{token_ty}),
            .name => try writer.print("name", .{}),
            .field => try writer.print("field", .{}),
            .ty => try writer.print("type", .{}),
        }
    }
};

pub const ParseErrorKind = enum {
    expected_found,
};

pub const ParseErrorCause = union(ParseErrorKind) {
    expected_found: struct { expected: []const Expectation, found: Token },

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .expected_found => |ef| {
                try writer.print("expected ", .{});
                for (ef.expected, 0..) |token_ty, i| {
                    if (i > 0) try writer.print(" or ", .{});
                    try writer.print("{}", .{token_ty});
                }
                try writer.print(", found {}\n", .{ef.found});
            },
        }
    }
};

pub const ParseError = struct {
    cause: ParseErrorCause,
    span: ?Span,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("error", .{});
        if (self.span) |s| try writer.print(" at {}", .{s});
        try writer.print(": ", .{});
        try writer.print("{}", .{self.cause});
    }
};

pub fn parse(lexed: *const lex.LexedFile, parent_alloc: std.mem.Allocator) Ast {
    var arena = std.heap.ArenaAllocator.init(parent_alloc);
    const alloc = arena.allocator();
    var parser = Parser{
        .lexed = lexed,
        .curr_token = 0,
        .items = std.ArrayList(Item).init(alloc),
        .errors = std.ArrayList(ParseError).init(alloc),
        .alloc = arena.allocator(),
    };
    while (parser.item() catch false) {}
    return .{
        .items = parser.items,
        .errors = parser.errors,
        .arena = arena,
    };
}

const Parser = struct {
    lexed: *const lex.LexedFile,
    curr_token: usize,
    items: std.ArrayList(Item),
    errors: std.ArrayList(ParseError),
    alloc: std.mem.Allocator,

    fn item(self: *Parser) !bool {
        if (try self.tryLayout()) |layout| {
            try self.items.append(.{ .layout = layout });
            return true;
        }
        return false;
    }

    fn tryLayout(self: *Parser) !?Layout {
        _ = self.tryExact(.layout) orelse return null;
        // Name
        const name = try self.expectIdent(.name);
        // Fields
        _ = try self.expect(.open_curly, &.{});
        var fields = std.ArrayList(Field).init(self.alloc);
        while (self.tryIdent()) |ident| {
            _ = try self.expect(.colon, &.{});
            const ty = try self.expectIdent(.ty);
            try fields.append(.{
                .name = ident.val,
                .ty = ty.val,
            });
        }
        _ = try self.expect(.close_curly, &.{.field});
        return .{
            .name = name.val,
            .name_span = name.span,
            .fields = fields,
        };
    }

    fn currToken(self: *const Parser) ?Sp(Token) {
        if (self.curr_token >= self.lexed.tokens.items.len)
            return null;
        return self.lexed.tokens.items[self.curr_token];
    }

    fn tryExact(self: *Parser, token_ty: TokenTy) ?Span {
        var token = self.currToken() orelse return null;
        if (token.val == token_ty) {
            self.curr_token += 1;
            return token.span;
        }
        return null;
    }

    fn expect(self: *Parser, comptime token_ty: TokenTy, comptime other_options: []const Expectation) !Span {
        return self.expected(Span, self.tryExact(token_ty), .{.{ .token = token_ty }} ++ other_options);
    }

    fn expected(self: *Parser, comptime T: type, got: ?T, comptime expectations: []const Expectation) !T {
        return got orelse {
            const token = self.currToken();
            try self.errors.append(.{
                .cause = .{ .expected_found = .{
                    .expected = expectations,
                    .found = if (token) |t| t.val else .eof,
                } },
                .span = if (token) |t| t.span else null,
            });
            return error.err;
        };
    }

    fn expectIdent(self: *Parser, comptime expectation: Expectation) !Sp([]const u8) {
        return self.expected(Sp([]const u8), self.tryIdent(), &.{expectation});
    }

    fn tryIdent(self: *Parser) ?Sp([]const u8) {
        var token = self.currToken() orelse return null;
        switch (token.val) {
            .ident => |ident| {
                self.curr_token += 1;
                return token.span.sp([]const u8, ident);
            },
            else => return null,
        }
    }

    fn tryNum(self: *Parser) ?Sp([]const u8) {
        var token = self.currToken() orelse return null;
        switch (token.val) {
            .num => |num| {
                self.curr_token += 1;
                return token.span.sp([]const u8, num);
            },
            else => return null,
        }
    }
};
