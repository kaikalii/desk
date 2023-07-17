const std = @import("std");
const lex = @import("lex.zig");
const Sp = lex.Sp;
const Span = lex.Span;
const Token = lex.Token;

/// The abstract syntax tree
///
/// All items and sub-items are allocated in the same arena allocator,
/// which can be deinitialized by calling `Ast.deinit`.
pub const Ast = struct {
    items: std.ArrayList(Item),
    errors: std.ArrayList(ParseError),
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *const Ast) void {
        self.arena.deinit();
    }
};

// Items
pub const Item = union(enum) {
    layout: Layout,
    stmt: Stmt,
    put: Put,
};

// Layout
pub const Layout = struct {
    name: []const u8,
    name_span: Span,
    fields: std.ArrayList(Field),
};
pub const Field = struct { name: []const u8, ty: Type };
pub const Type = []const u8;

// Statements
pub const Stmt = union(enum) { expr: Expr };
pub const Put = struct { name: []const u8, at: Expr };

// Expressions
pub const Expr = union(enum) {
    ident: []const u8,
    num: f64,
    vec: *VecExpr,
    bin: *BinExpr,
    paren: *Expr,
};
pub const VecExpr = struct { x: Expr, y: Expr, z: Expr };
pub const BinOp = enum { add, sub, mul, div };
pub const BinExpr = struct { op: BinOp, lhs: Expr, rhs: Expr };

/// Parse tokens into an abstract syntax tree
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
    while (parser.item() catch null) |item|
        parser.items.append(item) catch break;
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

    fn item(self: *Parser) Err!?Item {
        if (try self.tryLayout()) |layout|
            return .{ .layout = layout };
        if (try self.tryStmt()) |stmt|
            return .{ .stmt = stmt };
        if (try self.tryPut()) |put|
            return .{ .put = put };
        return null;
    }

    fn tryLayout(self: *Parser) Err!?Layout {
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

    fn tryStmt(self: *Parser) Err!?Stmt {
        if (try self.tryExpr()) |expr|
            return .{ .expr = expr };
        return null;
    }

    fn tryPut(self: *Parser) Err!?Put {
        _ = self.tryExact(.put) orelse return null;
        return .{
            .name = (try self.expectIdent(.name)).val,
            .at = try self.expectExpr(),
        };
    }

    fn expectExpr(self: *Parser) Err!Expr {
        return self.expectOf(Expr, try self.tryExpr(), &.{.expr});
    }
    fn tryExpr(self: *Parser) Err!?Expr {
        return self.tryAsExpr();
    }

    fn tryAsExpr(self: *Parser) Err!?Expr {
        const lhs = try self.tryMdExpr() orelse return null;
        for ([_]Token.Tag{ .plus, .minus }, [_]BinOp{ .add, .sub }) |token, op| {
            if (self.tryExact(token)) |_| {
                const rhs = try self.expectOf(Expr, try self.tryAsExpr(), &.{.expr});
                const alloced = try self.alloc.create(BinExpr);
                alloced.* = .{ .op = op, .lhs = lhs, .rhs = rhs };
                return .{ .bin = alloced };
            }
        }
        return lhs;
    }

    fn tryMdExpr(self: *Parser) Err!?Expr {
        const lhs = try self.tryTerm() orelse return null;
        for ([_]Token.Tag{ .star, .slash }, [_]BinOp{ .mul, .div }) |token, op| {
            if (self.tryExact(token)) |_| {
                const rhs = try self.expectOf(Expr, try self.tryMdExpr(), &.{.expr});
                const alloced = try self.alloc.create(BinExpr);
                alloced.* = .{ .op = op, .lhs = lhs, .rhs = rhs };
                return .{ .bin = alloced };
            }
        }
        return lhs;
    }

    fn tryTerm(self: *Parser) Err!?Expr {
        if (self.tryIdent()) |ident|
            return .{ .ident = ident.val };
        if (try self.tryNum(f64, 0.0)) |num|
            return .{ .num = num.val };
        if (try self.tryVec()) |vec| {
            const alloced = try self.alloc.create(VecExpr);
            alloced.* = vec;
            return .{ .vec = alloced };
        }
        if (self.tryExact(.open_paren)) |_| {
            const expr = try self.expectExpr();
            _ = try self.expect(.close_paren, &.{});
            const alloced = try self.alloc.create(Expr);
            alloced.* = expr;
            return .{ .paren = alloced };
        }
        return null;
    }

    fn tryVec(self: *Parser) Err!?VecExpr {
        _ = self.tryExact(.open_curly) orelse return null;
        const x = try self.expectExpr();
        const y = try self.expectExpr();
        const z = try self.expectExpr();
        _ = try self.expect(.close_curly, &.{});
        return .{ .x = x, .y = y, .z = z };
    }

    fn currToken(self: *const Parser) ?Token {
        if (self.curr_token >= self.lexed.tokens.items.len)
            return null;
        return self.lexed.tokens.items[self.curr_token];
    }

    fn tryExact(self: *Parser, tag: Token.Tag) ?Span {
        var token = self.currToken() orelse return null;
        if (token.tag == tag) {
            self.curr_token += 1;
            return token.span;
        }
        return null;
    }

    fn expect(self: *Parser, comptime tag: Token.Tag, comptime other_options: []const Expectation) Err!Span {
        return self.expectOf(Span, self.tryExact(tag), .{.{ .tag = tag }} ++ other_options);
    }

    fn expected(self: *Parser, comptime expectations: []const Expectation) Err!void {
        try self.expectOf(void, null, expectations);
    }

    fn expectOf(self: *Parser, comptime T: type, got: ?T, comptime expectations: []const Expectation) Err!T {
        return got orelse {
            const token = self.currToken();
            try self.errors.append(.{
                .kind = .{ .expected_found = .{
                    .expected = expectations,
                    .found = if (token) |t| t else .{ .tag = .eof, .span = self.lastSpan() },
                } },
                .span = if (token) |t| t.span else blk: {
                    var span = self.lastSpan();
                    span.start = span.end;
                    break :blk span;
                },
            });
            return error.err;
        };
    }

    fn lastSpan(self: *Parser) Span {
        return self.lexed.tokens.items[self.lexed.tokens.items.len - 1].span;
    }

    fn tryIdent(self: *Parser) ?Sp([]const u8) {
        var token = self.currToken() orelse return null;
        switch (token.tag) {
            .ident => {
                self.curr_token += 1;
                return token.span.sp([]const u8, token.span.str());
            },
            else => return null,
        }
    }

    fn expectIdent(self: *Parser, comptime expectation: Expectation) Err!Sp([]const u8) {
        return self.expectOf(Sp([]const u8), self.tryIdent(), &.{expectation});
    }

    fn tryNum(self: *Parser, comptime T: type, default: T) Err!?Sp(T) {
        var token = self.currToken() orelse return null;
        switch (token.tag) {
            .num => {
                self.curr_token += 1;
                const n = std.fmt.parseFloat(T, token.span.str()) catch blk: {
                    try self.errors.append(.{
                        .kind = .{ .invalid_number = token.span.str() },
                        .span = token.span,
                    });
                    break :blk default;
                };
                return token.span.sp(T, n);
            },
            else => return null,
        }
    }

    fn expectNum(self: *Parser, comptime expectation: Expectation) Err!Sp([]const u8) {
        return self.expectOf(Sp([]const u8), try self.tryNum(), &.{expectation});
    }
};

// Expectations
pub const Expectation = union(enum) {
    tag: Token.Tag,
    name,
    field,
    ty,
    layout,
    stmt,
    expr,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .tag => |tag| try writer.print("{}", .{tag}),
            .name => try writer.print("name", .{}),
            .field => try writer.print("field", .{}),
            .ty => try writer.print("type", .{}),
            .layout => try writer.print("layout", .{}),
            .stmt => try writer.print("statement", .{}),
            .expr => try writer.print("expression", .{}),
        }
    }
};

// Errors
pub const ParseErrorKind = union(enum) {
    expected_found: struct { expected: []const Expectation, found: Token },
    invalid_number: []const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .expected_found => |ef| {
                try writer.print("expected ", .{});
                for (ef.expected, 0..) |token_ty, i| {
                    if (i > 0) {
                        if (i == ef.expected.len - 1) try writer.print(" or ", .{}) else try writer.print(", ", .{});
                    }
                    try writer.print("{}", .{token_ty});
                }
                try writer.print(", found {}\n", .{ef.found});
            },
            .invalid_number => |num| try writer.print("invalid number: {s}", .{num}),
        }
    }
};
pub const ParseError = struct {
    kind: ParseErrorKind,
    span: Span,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("error", .{});
        try writer.print(" at {}", .{self.span});
        try writer.print(": ", .{});
        try writer.print("{}", .{self.kind});
    }
};
const Err = error{err} || std.mem.Allocator.Error;
