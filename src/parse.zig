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
    field: Field,
    shape: Shape,
    proc: Proc,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .field => try writer.print("{}", .{self.field}),
            .shape => try writer.print("{}", .{self.shape}),
            .proc => try writer.print("{}", .{self.proc}),
        }
    }
};

// Shape
pub const Shape = union(enum) {
    def: ShapeDef,
    alias: ShapeAlias,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .def => try writer.print("{}", .{self.def}),
            .alias => try writer.print("{}", .{self.alias}),
        }
    }
};
pub const ShapeDef = struct {
    name: []const u8,
    name_span: Span,
    items: std.ArrayList(Item),

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("shape {s} {{\n", .{self.name});
        for (self.items.items) |item|
            try writer.print("    {}\n", .{item});
        try writer.print("}}", .{});
    }
};
pub const ShapeAlias = struct {
    name: []const u8,
    name_span: Span,
    ty: Type,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("shape {s} {}", .{ self.name, self.ty });
    }
};
pub const Field = struct {
    name: []const u8,
    ty: Type,
    start: Expr,
    axis: ?Axis,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{s} {} {} {};", .{ self.name, self.ty, self.start, self.axis orelse .x });
    }
};
pub const Type = union(enum) {
    named: []const u8,
    array: struct { len: ?Expr, ty: *Type },

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .named => try writer.print("{s}", .{self.named}),
            .array => |array| if (array.len) |len| try writer.print("[{}]{}", .{ len, array.ty }) else try writer.print("[]{}", .{array.ty}),
        }
    }
};

// Proc
pub const Proc = struct {
    name: []const u8,
    name_span: Span,
    params: std.ArrayList(Sp([]const u8)),
    body: std.ArrayList(Stmt),

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("proc {s}", .{self.name});
        for (self.params.items) |param|
            try writer.print(" {s}", .{param.val});
        try writer.print(" {{\n", .{});
        for (self.body.items) |stmt|
            try writer.print("    {};\n", .{stmt});
        try writer.print("}}", .{});
    }
};

// Statements
pub const Stmt = union(enum) {
    call: Call,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .call => try writer.print("{}", .{self.call}),
        }
    }
};
pub const Call = struct {
    proc: []const u8,
    proc_span: Span,
    args: std.ArrayList(Expr),

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{s}", .{self.proc});
        for (self.args.items) |arg|
            try writer.print(" {}", .{arg});
    }
};

// Expressions
pub const Expr = union(enum) {
    ident: []const u8,
    raw_len: []const u8,
    typed_len: []const u8,
    num: f64,
    vec: *VecExpr,
    bin: *BinExpr,
    axis: *AxisExpr,
    paren: *Expr,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .ident => try writer.print("{s}", .{self.ident}),
            .raw_len => try writer.print("@{s}", .{self.raw_len}),
            .typed_len => try writer.print("#{s}", .{self.typed_len}),
            .num => try writer.print("{d}", .{self.num}),
            .vec => try writer.print("{}", .{self.vec}),
            .bin => try writer.print("{}", .{self.bin}),
            .axis => try writer.print("{}", .{self.axis}),
            .paren => try writer.print("({})", .{self.paren}),
        }
    }
};
pub const VecExpr = struct {
    x: Expr,
    y: Expr,
    z: Expr,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{{{} {} {}}}", .{ self.x, self.y, self.z });
    }
};
pub const BinOp = enum { add, sub, mul, div };
pub const BinExpr = struct { op: BinOp, lhs: Expr, rhs: Expr };
pub const Axis = enum {
    x,
    y,
    z,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .x => try writer.print("x", .{}),
            .y => try writer.print("y", .{}),
            .z => try writer.print("z", .{}),
        }
    }
};
pub const AxisExpr = struct {
    vec: Expr,
    axis: Axis,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{}:{}", .{ self.vec, self.axis });
    }
};

/// Parse tokens into an abstract syntax tree
pub fn parse(lexed: *const lex.LexedFile, parent_alloc: std.mem.Allocator) Ast {
    var arena = std.heap.ArenaAllocator.init(parent_alloc);
    const alloc = arena.allocator();
    var parser = Parser{
        .lexed = lexed,
        .curr_token = 0,
        .root_items = std.ArrayList(Item).init(alloc),
        .errors = std.ArrayList(ParseError).init(alloc),
        .alloc = arena.allocator(),
    };
    // Parse root items
    while (parser.tryItem() catch null) |item|
        parser.root_items.append(item) catch break;
    // Make sure all tokens were consumed
    if (parser.errors.items.len > 0)
        if (parser.currToken()) |_|
            parser.expected(&.{ .field, .{ .tag = .shape }, .{ .tag = .proc } }) catch {};
    return .{
        .items = parser.root_items,
        .errors = parser.errors,
        .arena = arena,
    };
}

const Parser = struct {
    lexed: *const lex.LexedFile,
    curr_token: usize,
    root_items: std.ArrayList(Item),
    errors: std.ArrayList(ParseError),
    alloc: std.mem.Allocator,

    fn tryItem(self: *Parser) Err!?Item {
        if (try self.tryShape()) |shape|
            return .{ .shape = shape };
        if (try self.tryField()) |field|
            return .{ .field = field };
        if (try self.tryProc()) |proc|
            return .{ .proc = proc };
        return null;
    }

    fn tryField(self: *Parser) Err!?Field {
        const ident = self.tryIdent() orelse return null;
        const ty = try self.expectType();
        const start = try self.expectExpr();
        const axis = self.tryAxis();
        _ = self.expect(.semicolon, &.{.field}) catch {};
        return .{
            .name = ident.val,
            .ty = ty,
            .start = start,
            .axis = if (axis) |a| a.val else null,
        };
    }

    fn tryShape(self: *Parser) Err!?Shape {
        _ = self.tryExact(.shape) orelse return null;
        // Name
        const name = try self.expectIdent(.name);
        // Items
        if (self.tryExact(.open_curly)) |_| {
            var items = std.ArrayList(Item).init(self.alloc);
            errdefer items.deinit();
            while (try self.tryItem()) |item|
                try items.append(item);
            _ = try self.expect(.close_curly, &.{ .field, .{ .tag = .shape } });
            return .{ .def = .{
                .name = name.val,
                .name_span = name.span,
                .items = items,
            } };
        } else if (try self.tryType()) |ty| {
            return .{ .alias = .{
                .name = name.val,
                .name_span = name.span,
                .ty = ty,
            } };
        } else {
            try self.expected(&.{ .{ .tag = .open_curly }, .ty });
            return error.err;
        }
    }

    fn expectType(self: *Parser) Err!Type {
        return self.expectOf(Type, try self.tryType(), &.{.ty});
    }
    fn tryType(self: *Parser) Err!?Type {
        if (self.tryIdent()) |ident|
            return .{ .named = ident.val };
        if (self.tryExact(.open_bracket)) |_| {
            const len = try self.expectExpr();
            _ = try self.expect(.close_bracket, &.{});
            const ty = try self.expectType();
            var alloced = try self.alloc.create(Type);
            alloced.* = ty;
            return .{ .array = .{ .len = len, .ty = alloced } };
        }
        return null;
    }

    fn tryProc(self: *Parser) Err!?Proc {
        _ = self.tryExact(.proc) orelse return null;
        const name = try self.expectIdent(.name);
        var params = std.ArrayList(Sp([]const u8)).init(self.alloc);
        errdefer params.deinit();
        while (self.tryIdent()) |param| {
            try params.append(param);
        }
        _ = try self.expect(.open_curly, &.{.param});
        var body = std.ArrayList(Stmt).init(self.alloc);
        errdefer body.deinit();
        while (try self.tryStmt()) |stmt| {
            try body.append(stmt);
            _ = try self.expect(.semicolon, &.{.arg});
        }
        _ = try self.expect(.close_curly, &.{.stmt});
        return .{
            .name = name.val,
            .name_span = name.span,
            .params = params,
            .body = body,
        };
    }

    fn tryStmt(self: *Parser) Err!?Stmt {
        if (try self.tryCall()) |call|
            return .{ .call = call };
        return null;
    }

    fn tryCall(self: *Parser) Err!?Call {
        const proc = self.tryIdent() orelse return null;
        var args = std.ArrayList(Expr).init(self.alloc);
        errdefer args.deinit();
        while (try self.tryExpr()) |arg|
            try args.append(arg);
        return .{ .proc = proc.val, .proc_span = proc.span, .args = args };
    }

    fn expectExpr(self: *Parser) Err!Expr {
        return self.expectOf(Expr, try self.tryExpr(), &.{.expr});
    }
    fn tryExpr(self: *Parser) Err!?Expr {
        return self.tryAsExpr();
    }

    fn tryAsExpr(self: *Parser) Err!?Expr {
        var lhs = try self.tryMdExpr() orelse return null;
        while (try self.tryAsOp()) |op| {
            const rhs = try self.expectOf(Expr, try self.tryMdExpr(), &.{.expr});
            const alloced = try self.alloc.create(BinExpr);
            alloced.* = .{ .op = op, .lhs = lhs, .rhs = rhs };
            lhs = .{ .bin = alloced };
        }
        return lhs;
    }
    fn tryAsOp(self: *Parser) Err!?BinOp {
        if (self.tryExact(.plus)) |_| return .add;
        if (self.tryExact(.minus)) |_| return .sub;
        return null;
    }

    fn tryMdExpr(self: *Parser) Err!?Expr {
        var lhs = try self.tryAxisExpr() orelse return null;
        while (try self.tryMdOp()) |op| {
            const rhs = try self.expectOf(Expr, try self.tryAxisExpr(), &.{.expr});
            const alloced = try self.alloc.create(BinExpr);
            alloced.* = .{ .op = op, .lhs = lhs, .rhs = rhs };
            lhs = .{ .bin = alloced };
        }
        return lhs;
    }
    fn tryMdOp(self: *Parser) Err!?BinOp {
        if (self.tryExact(.star)) |_| return .mul;
        if (self.tryExact(.slash)) |_| return .div;
        return null;
    }

    fn tryAxisExpr(self: *Parser) Err!?Expr {
        const vec = try self.tryTerm() orelse return null;
        if (self.tryExact(.colon)) |_| {
            const axis = try self.expectAxis();
            const alloced = try self.alloc.create(AxisExpr);
            alloced.* = .{ .vec = vec, .axis = axis.val };
            return .{ .axis = alloced };
        } else {
            return vec;
        }
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
        if (self.tryExact(.octothorpe)) |_| {
            const name = try self.expectIdent(.name);
            return .{ .typed_len = name.val };
        }
        if (self.tryExact(.at)) |_| {
            const name = try self.expectIdent(.name);
            return .{ .raw_len = name.val };
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
                const n = std.fmt.parseFloat(T, token.span.str()) catch blk: {
                    try self.errors.append(.{
                        .kind = .{ .invalid_number = token.span.str() },
                        .span = token.span,
                    });
                    break :blk default;
                };
                self.curr_token += 1;
                return token.span.sp(T, n);
            },
            else => return null,
        }
    }

    fn expectNum(self: *Parser, comptime expectation: Expectation) Err!Sp([]const u8) {
        return self.expectOf(Sp([]const u8), try self.tryNum(), &.{expectation});
    }

    fn expectAxis(self: *Parser) Err!Sp(Axis) {
        return self.tryAxis() orelse {
            if (self.tryIdent()) |ident| {
                try self.errors.append(.{
                    .kind = .{ .not_an_axis = ident.val },
                    .span = ident.span,
                });
                return error.err;
            } else {
                return self.expectOf(Sp(Axis), null, &.{.axis});
            }
        };
    }
    fn tryAxis(self: *Parser) ?Sp(Axis) {
        var token = self.currToken() orelse return null;
        switch (token.tag) {
            .ident => {
                const axis: Axis = if (token.span.str().len != 1) return null else switch (token.span.str()[0]) {
                    'x' => .x,
                    'y' => .y,
                    'z' => .z,
                    else => return null,
                };
                self.curr_token += 1;
                return token.span.sp(Axis, axis);
            },
            else => return null,
        }
    }
};

// Expectations
pub const Expectation = union(enum) {
    tag: Token.Tag,
    name,
    field,
    ty,
    stmt,
    expr,
    axis,
    param,
    arg,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .tag => |tag| try writer.print("{}", .{tag}),
            .name => try writer.print("name", .{}),
            .field => try writer.print("field", .{}),
            .ty => try writer.print("type", .{}),
            .stmt => try writer.print("statement", .{}),
            .expr => try writer.print("expression", .{}),
            .axis => try writer.print("axis", .{}),
            .param => try writer.print("parameter", .{}),
            .arg => try writer.print("argument", .{}),
        }
    }
};

// Errors
pub const ParseErrorKind = union(enum) {
    expected_found: struct { expected: []const Expectation, found: Token },
    invalid_number: []const u8,
    not_an_axis: []const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .expected_found => |ef| {
                try writer.print("expected ", .{});
                for (ef.expected, 0..) |token_ty, i| {
                    if (i > 0)
                        if (i == ef.expected.len - 1) try writer.print(" or ", .{}) else try writer.print(", ", .{});
                    try writer.print("{}", .{token_ty});
                }
                try writer.print(", found {}\n", .{ef.found});
            },
            .invalid_number => |num| try writer.print("invalid number: {s}", .{num}),
            .not_an_axis => |axis| try writer.print("`{s}` is not an axis", .{axis}),
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
