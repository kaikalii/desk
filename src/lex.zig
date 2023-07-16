const std = @import("std");

pub const Loc = struct {
    line: usize,
    col: usize,
    pos: usize,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{}:{}", .{ self.line, self.col });
    }
};

pub const Span = struct {
    start: Loc,
    end: Loc,
    file: []const u8,
    src: []const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{s}:{}", .{ self.file, self.start });
    }

    pub fn sp(self: @This(), comptime T: type, val: T) T {
        return .{ .val = val, .span = self };
    }
};

pub fn Sp(comptime T: type) type {
    return struct {
        val: T,
        span: Span,

        pub fn format(self: @This(), comptime s: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
            try writer.print("{}: {" ++ s ++ "}", .{ self.span, self.val });
        }
    };
}

pub const TokenTy = enum {
    ident,
    num,
    open_paren,
    close_paren,
    open_bracket,
    close_bracket,
    open_curly,
    close_curly,
};

pub const Token = union(TokenTy) {
    ident: []const u8,
    num: []const u8,
    open_paren,
    close_paren,
    open_bracket,
    close_bracket,
    open_curly,
    close_curly,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            .ident => writer.print("ident({s})", .{self.ident}),
            .num => writer.print("num({s})", .{self.num}),
            .open_paren => writer.print("(", .{}),
            .close_paren => writer.print(")", .{}),
            .open_bracket => writer.print("[", .{}),
            .close_bracket => writer.print("]", .{}),
            .open_curly => writer.print("{{", .{}),
            .close_curly => writer.print("}}", .{}),
        };
    }
};

pub const LexedFile = struct {
    tokens: std.ArrayList(Sp(Token)),
    input: []const u8,
    alloc: std.mem.Allocator,

    pub fn deinit(self: *const LexedFile) void {
        self.tokens.deinit();
        self.alloc.free(self.input);
    }
};

pub fn lexFile(path: []const u8, alloc: std.mem.Allocator) !LexedFile {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const input = try file.readToEndAlloc(alloc, std.math.maxInt(usize));
    const tokens = try lex(path, input, alloc);
    return .{ .tokens = tokens, .input = input, .alloc = alloc };
}

pub fn lex(path: []const u8, src: []const u8, alloc: std.mem.Allocator) !std.ArrayList(Sp(Token)) {
    var lexer = Lexer{
        .loc = .{ .line = 1, .col = 1, .pos = 0 },
        .file = path,
        .src = src,
        .iter = (try std.unicode.Utf8View.init(src)).iterator(),
        .tokens = std.ArrayList(Sp(Token)).init(alloc),
    };
    try lexer.go();
    return lexer.tokens;
}

const Lexer = struct {
    loc: Loc,
    file: []const u8,
    src: []const u8,
    iter: std.unicode.Utf8Iterator,
    tokens: std.ArrayList(Sp(Token)),

    fn currChar(self: *Lexer) []const u8 {
        return self.iter.peek(1);
    }

    fn progress(self: *Lexer, cp: []const u8) void {
        switch (cp[0]) {
            '\n' => {
                self.loc.line += 1;
                self.loc.col = 1;
            },
            '\r' => {},
            else => self.loc.col += 1,
        }
        self.loc.pos += cp.len;
        _ = self.iter.nextCodepoint();
    }

    fn nextIf(self: *Lexer, comptime f: fn ([]const u8) bool) ?[]const u8 {
        const cp = self.currChar();
        if (cp.len == 0) return null;
        if (!f(cp)) return null;
        self.progress(cp);
        return cp;
    }

    fn nextIfIn(self: *Lexer, if_in: []const u8) bool {
        const cp = self.currChar();
        for (if_in) |c| {
            if (cp[0] == c) {
                self.progress(cp);
                return true;
            }
        }
        return false;
    }

    fn next(self: *Lexer) ?[]const u8 {
        return self.nextIf(always);
    }
    fn always(_: []const u8) bool {
        return true;
    }

    fn addToken(self: *Lexer, start: Loc, token: Token) !void {
        const span = .{ .start = start, .end = self.loc, .file = self.file, .src = self.src };
        try self.tokens.append(.{ .val = token, .span = span });
    }

    fn go(self: *Lexer) !void {
        while (true) {
            const start = self.loc;
            const cp = self.next() orelse break;
            switch (cp[0]) {
                ' ', '\t', '\r', '\n' => {},
                '(' => try self.addToken(start, .open_paren),
                ')' => try self.addToken(start, .close_paren),
                '[' => try self.addToken(start, .open_bracket),
                ']' => try self.addToken(start, .close_bracket),
                '{' => try self.addToken(start, .open_curly),
                '}' => try self.addToken(start, .close_curly),
                else => {
                    if (isIdentHead(cp)) {
                        // Idents and keywords
                        while (self.nextIf(isIdentTail)) |_| {}
                        const ident = self.src[start.pos..self.loc.pos];
                        try self.addToken(start, .{ .ident = ident });
                    } else if (isDigit(cp)) {
                        // Numbers
                        // Whole part
                        while (self.nextIf(isDigit)) |_| {}
                        // Fractional part
                        if (self.nextIfIn(".")) {
                            while (self.nextIf(isDigit)) |_| {}
                        }
                        // Exponent
                        if (self.nextIfIn("eE")) {
                            _ = self.nextIfIn("+-");
                            while (self.nextIf(isDigit)) |_| {}
                        }
                        const num = self.src[start.pos..self.loc.pos];
                        try self.addToken(start, .{ .num = num });
                    }
                },
            }
        }
    }
};

fn isIdentHead(cp: []const u8) bool {
    if (cp.len == 0) return false;
    if (cp.len > 1) return true;
    const c = cp[0];
    return ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or c == '_';
}

fn isDigit(cp: []const u8) bool {
    if (cp.len == 0) return false;
    if (cp.len > 1) return true;
    const c = cp[0];
    return '0' <= c and c <= '9';
}

fn isIdentTail(cp: []const u8) bool {
    return isIdentHead(cp) or isDigit(cp);
}
