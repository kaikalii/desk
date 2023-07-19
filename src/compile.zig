const std = @import("std");
const parse = @import("parse.zig");
const lex = @import("lex.zig");
const Axis = parse.Axis;
const Span = lex.Span;

const Vec = packed struct(u63) {
    x: u21,
    y: u21,
    z: u21,
    pub const zero: @This() = .{ .x = 0, .y = 0, .z = 0 };
    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{{{} {} {}}}", .{ self.x, self.y, self.z });
    }
};
const FVec = struct {
    x: f64,
    y: f64,
    z: f64,
    pub const zero: @This() = .{ .x = 0.0, .y = 0.0, .z = 0.0 };
    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{{{} {} {}}}", .{ self.x, self.y, self.z });
    }
};

const Shape = struct {
    shapes: std.StringHashMap(Shape),
    fields: std.StringHashMap(Field),
    aliases: std.StringHashMap(Type),
    size: Vec,

    fn init(alloc: std.mem.Allocator) Shape {
        return .{
            .shapes = std.StringHashMap(Shape).init(alloc),
            .fields = std.StringHashMap(Field).init(alloc),
            .aliases = std.StringHashMap(Type).init(alloc),
            .size = Vec.zero,
        };
    }

    pub fn format(self: @This(), comptime s: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("(shape {s}(shapes", .{s});

        var shapeIter = self.shapes.iterator();
        while (shapeIter.next()) |entry| {
            try writer.print("{s} {s}: {},", .{ s, entry.key_ptr.*, entry.value_ptr.* });
        }

        try writer.print("{s}) {s}(aliases", .{ s, s });
        var typeIter = self.aliases.iterator();
        while (typeIter.next()) |entry| {
            try writer.print("{s} {s}: {},", .{ s, entry.key_ptr.*, entry.value_ptr.* });
        }

        try writer.print("{s}) {s}(fields", .{ s, s });
        var fieldIter = self.fields.iterator();
        while (fieldIter.next()) |entry| {
            try writer.print("{s} {s}: {},", .{ s, entry.key_ptr.*, entry.value_ptr.* });
        }

        try writer.print("{s}))", .{s});
    }
};

const Field = struct {
    type: Type,
    start: Vec,
    axis: Axis,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{} {} {}", .{ self.type, self.start, self.axis });
    }
};

const Type = union(enum) {
    byte,
    int,
    real,
    array: struct { len: u64, type: *Type },
    slice: struct { type: *Type },
    shape: *Shape,
    err,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .byte => try writer.print("byte", .{}),
            .int => try writer.print("int", .{}),
            .real => try writer.print("real", .{}),
            .array => |arr| try writer.print("[{}]{}", .{ arr.len, arr.type.* }),
            .slice => |slice| try writer.print("[]{}", .{slice.type.*}),
            .shape => |sh| try writer.print("{}", .{sh}),
            .err => try writer.print("<error>", .{}),
        }
    }
};

pub const Compiled = struct {
    root: Shape,
    errors: std.ArrayList(CompileError),
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *const Compiled) void {
        self.arena.deinit();
    }
};

/// Compile an AST into a shape tree.
pub fn compile(ast: parse.Ast, parent_alloc: std.mem.Allocator) Compiled {
    var arena = std.heap.ArenaAllocator.init(parent_alloc);
    const alloc = arena.allocator();
    var root = Shape.init(alloc);
    root.aliases.put("byte", .byte) catch {};
    root.aliases.put("int", .int) catch {};
    root.aliases.put("real", .real) catch {};
    var compiler = Compiler{
        .ast = ast,
        .root = root,
        .errors = std.ArrayList(CompileError).init(alloc),
        .alloc = arena.allocator(),
    };
    for (ast.items.items) |it|
        compiler.item(&compiler.root, it) catch {};
    return .{
        .root = compiler.root,
        .errors = compiler.errors,
        .arena = arena,
    };
}

const Err = error{err} || std.mem.Allocator.Error;

const Compiler = struct {
    ast: parse.Ast,
    root: Shape,
    errors: std.ArrayList(CompileError),
    alloc: std.mem.Allocator,

    fn shape(self: *Compiler, parent: *Shape, pshape: parse.Shape) Err!void {
        switch (pshape) {
            .def => |def| {
                var sh = Shape.init(self.alloc);
                for (def.items.items) |it|
                    try self.item(&sh, it);
                try parent.shapes.put(def.name, sh);
            },
            .alias => |alias| {
                var t = try self.ty(parent, alias.ty);
                try parent.aliases.put(alias.name, t);
            },
        }
    }

    fn item(self: *Compiler, parent: *Shape, it: parse.Item) Err!void {
        try switch (it) {
            .shape => |sh| self.shape(parent, sh),
            .field => |fld| self.field(parent, fld),
            .proc => |pr| self.proc(parent, pr),
        };
    }

    fn proc(self: *Compiler, parent: *Shape, pr: parse.Proc) void {
        _ = pr;
        _ = parent;
        _ = self;
    }

    fn ty(self: *Compiler, parent: *const Shape, t: parse.Type) Err!Type {
        switch (t) {
            .named => |name| {
                if (self.getType(parent, name.val)) |typ|
                    return typ;
                try self.errors.append(.{ .kind = .{ .unknown_type = name.val }, .span = name.span });
                return .err;
            },
            .array => |arr| {
                const subty = try self.ty(parent, arr.ty.*);
                var alloced: *Type = try self.alloc.create(Type);
                errdefer self.alloc.destroy(alloced);
                alloced.* = subty;
                if (arr.len) |len_expr| {
                    const len = try self.numExpr(parent, len_expr);
                    return .{ .array = .{ .len = @intFromFloat(len), .type = alloced } };
                } else {
                    return .{ .slice = .{ .type = alloced } };
                }
            },
        }
    }

    fn field(self: *Compiler, parent: *Shape, fld: parse.Field) Err!void {
        var f = Field{
            .type = try self.ty(parent, fld.ty),
            .start = try self.valueAsVec(try self.expr(parent, fld.start), fld.start.span()),
            .axis = fld.axis orelse .x,
        };
        try parent.fields.put(fld.name, f);
    }

    fn getField(self: *const Compiler, parent: *const Shape, name: []const u8) ?Field {
        if (parent.fields.get(name)) |fld|
            return fld;
        if (self.root.fields.get(name)) |fld|
            return fld;
        return null;
    }

    fn getType(self: *const Compiler, parent: *const Shape, name: []const u8) ?Type {
        if (parent.aliases.get(name)) |alias|
            return alias;
        if (self.root.aliases.get(name)) |alias|
            return alias;
        if (parent.shapes.getPtr(name)) |sh|
            return .{ .shape = sh };
        if (self.root.shapes.getPtr(name)) |sh|
            return .{ .shape = sh };
        return null;
    }

    fn valueAsNum(self: *Compiler, val: Value, span: Span) Err!f64 {
        switch (val) {
            .num => |n| return n,
            .vec => {
                try self.errors.append(.{ .kind = .{ .expected_num = val }, .span = span });
                return 0.0;
            },
            .err => return 0.0,
        }
    }

    fn valueAsVec(self: *Compiler, val: Value, span: Span) Err!Vec {
        switch (val) {
            .num => {
                try self.errors.append(.{ .kind = .{ .expected_vec = val }, .span = span });
                return Vec.zero;
            },
            .vec => |v| return .{
                .x = @intFromFloat(v.x),
                .y = @intFromFloat(v.y),
                .z = @intFromFloat(v.z),
            },
            .err => return Vec.zero,
        }
    }

    fn numExpr(self: *Compiler, parent: *const Shape, ex: parse.Expr) Err!f64 {
        return self.valueAsNum(try self.expr(parent, ex), ex.span());
    }

    fn expr(self: *Compiler, parent: *const Shape, ex: parse.Expr) Err!Value {
        switch (ex) {
            .num => |num| return .{ .num = num.val },
            .vec => |vec| {
                const x = try self.numExpr(parent, vec.x);
                const y = try self.numExpr(parent, vec.y);
                const z = try self.numExpr(parent, vec.z);
                return .{ .vec = .{ .x = x, .y = y, .z = z } };
            },
            .ident => |ident| {
                if (std.mem.eql(u8, ident.val, "origin"))
                    return .{ .vec = FVec.zero };
                if (self.getField(parent, ident.val)) |fld|
                    return .{ .vec = .{
                        .x = @floatFromInt(fld.start.x),
                        .y = @floatFromInt(fld.start.y),
                        .z = @floatFromInt(fld.start.z),
                    } };
                try self.errors.append(.{ .kind = .{ .unknown_ident = ident.val }, .span = ident.span });
                return .err;
            },
            .typed_len => |ident| {
                _ = ident;
                @panic("TODO: typed_len");
            },
            .raw_len => |ident| {
                _ = ident;
                @panic("TODO: raw_len");
            },
            .paren => |child| return self.expr(parent, child.*),
            .bin => |bin| {
                const lhs = try self.expr(parent, bin.lhs);
                const rhs = try self.expr(parent, bin.rhs);
                switch (bin.op) {
                    .add => return lhs.binOp(rhs, add),
                    .sub => return lhs.binOp(rhs, sub),
                    .mul => return lhs.binOp(rhs, mul),
                    .div => return lhs.binOp(rhs, div),
                }
            },
            .axis => |axis| {
                const v = try self.expr(parent, axis.vec);
                return switch (v) {
                    .num => |n| {
                        switch (axis.axis) {
                            .x => return .{ .vec = .{ .x = n, .y = 0, .z = 0 } },
                            .y => return .{ .vec = .{ .x = 0, .y = n, .z = 0 } },
                            .z => return .{ .vec = .{ .x = 0, .y = 0, .z = n } },
                        }
                    },
                    .vec => |vec| {
                        switch (axis.axis) {
                            .x => return .{ .vec = .{ .x = vec.x, .y = 0, .z = 0 } },
                            .y => return .{ .vec = .{ .x = 0, .y = vec.y, .z = 0 } },
                            .z => return .{ .vec = .{ .x = 0, .y = 0, .z = vec.z } },
                        }
                    },
                    .err => return .err,
                };
            },
        }
    }
};

pub const Value = union(enum) {
    num: f64,
    vec: FVec,
    err,

    fn binOp(self: Value, other: Value, comptime f: fn (f64, f64) f64) Value {
        return switch (self) {
            .num => |a| switch (other) {
                .num => |b| .{ .num = f(a, b) },
                .vec => |b| .{ .vec = .{ .x = f(a, b.x), .y = f(a, b.y), .z = f(a, b.z) } },
                .err => .err,
            },
            .vec => |a| switch (other) {
                .num => |b| .{ .vec = .{ .x = f(a.x, b), .y = f(a.y, b), .z = f(a.z, b) } },
                .vec => |b| .{ .vec = .{ .x = f(a.x, b.x), .y = f(a.y, b.y), .z = f(a.z, b.z) } },
                .err => .err,
            },
            .err => .err,
        };
    }

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .num => |n| try writer.print("{}", .{n}),
            .vec => |v| try writer.print("{{{} {} {}}}", .{ v.x, v.y, v.z }),
            .err => try writer.print("<error>", .{}),
        }
    }
};

fn add(a: f64, b: f64) f64 {
    return a + b;
}
fn sub(a: f64, b: f64) f64 {
    return a - b;
}
fn mul(a: f64, b: f64) f64 {
    return a * b;
}
fn div(a: f64, b: f64) f64 {
    return a / b;
}

pub const CompileErrorKind = union(enum) {
    unknown_type: []const u8,
    unknown_ident: []const u8,
    expected_num: Value,
    expected_vec: Value,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .unknown_type => |name| try writer.print("unknown type: {s}", .{name}),
            .unknown_ident => |name| try writer.print("unknown ident: {s}", .{name}),
            .expected_num => |val| try writer.print("expected number, got {}", .{val}),
            .expected_vec => |val| try writer.print("expected vector, got {}", .{val}),
        }
    }
};

pub const CompileError = struct {
    kind: CompileErrorKind,
    span: Span,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{}: {}", .{ self.span, self.kind });
    }
};
