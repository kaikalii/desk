const std = @import("std");
const parse = @import("parse.zig");
const Axis = parse.Axis;

const Vec = packed struct(u63) { x: u21, y: u21, z: u21 };

const Shape = struct {
    shapes: std.StringHashMap(Shape),
    fields: std.StringHashMap(Field),
    aliases: std.StringHashMap(Type),

    fn init(alloc: std.mem.Allocator) Shape {
        return .{
            .shapes = std.StringHashMap(Shape).init(alloc),
            .fields = std.StringHashMap(Field).init(alloc),
            .aliases = std.StringHashMap(Type).init(alloc),
        };
    }
};

const Field = struct {
    type: Type,
    start: Vec,
    axis: Axis,
};

const Type = union(enum) {
    byte,
    int,
    real,
    array: struct { len: u64, type: *Type },
    slice: struct { type: *Type },
    shape: *Shape,
};

pub const Compiled = struct {
    root: Shape,
};

pub fn compile(ast: parse.Ast) Compiled {
    var the_ast = ast;
    var compiler = Compiler{
        .ast = the_ast,
        .root = Shape.init(the_ast.arena.allocator()),
    };
    for (the_ast.items.items) |it|
        compiler.item(&compiler.root, it) catch {};
    return .{ .root = compiler.root };
}

const Err = error{err} || std.mem.Allocator.Error;

const Compiler = struct {
    ast: parse.Ast,
    root: Shape,

    fn alloc(self: *Compiler) std.mem.Allocator {
        return self.ast.arena.allocator();
    }

    fn shape(self: *Compiler, parent: *Shape, pshape: parse.Shape) Err!void {
        switch (pshape) {
            .def => |def| {
                var sh = Shape.init(self.alloc());
                for (def.items.items) |it|
                    try self.item(&sh, it);
                try parent.shapes.put(def.name, sh);
            },
            .alias => |alias| {
                var t = self.ty(alias.ty);
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

    fn ty(self: *Compiler, parent: *const Shape, t: parse.Type) Type {
        switch (t) {
            .named => |name| {
                if (std.mem.eql(u8, name, "byte"))
                    return .byte;
                if (std.mem.eql(u8, name, "int"))
                    return .int;
                if (std.mem.eql(u8, name, "real"))
                    return .real;
            },
            .array => |arr| {
                const subty = self.ty(parent, arr.ty.*);
                var alloced = try self.alloc().create(Type);
                errdefer self.alloc().destroy(Type, alloced);
                alloced.* = subty;
                if (arr.len) |len_expr| {
                    const len = self.expr(len_expr);
                    return .{ .array = .{ .len = len, .type = alloced } };
                } else {
                    return .{ .slice = .{ .type = alloced } };
                }
            },
        }
    }

    fn field(self: *Compiler, parent: *Shape, fld: parse.Field) Err!void {
        var f = Field{
            .type = self.ty(parent, fld.ty),
            .start = Vec{ .x = 0, .y = 0, .z = 0 },
            .axis = fld.axis orelse .x,
        };
        try parent.fields.put(fld.name, f);
    }

    fn expr(self: *const Compiler, ex: parse.Expr) u64 {
        _ = ex;
        _ = self;
        unreachable;
    }
};
