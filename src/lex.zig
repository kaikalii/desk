const std = @import("std");

pub const Loc = struct {
    line: usize,
    col: usize,

    pub fn inSrc(src: []const u8, pos: usize) Loc {
        var line: usize = 1;
        var col: usize = 1;
        for (src[0..pos]) |c| {
            if (c == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        return .{ .line = line, .col = col };
    }

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{}:{}", .{ self.line, self.col });
    }
};

pub const Span = struct {
    start: usize,
    end: usize,
    file: []const u8,
    src: []const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{s}:{}", .{ self.file, self.startLoc() });
    }

    pub fn str(self: @This()) []const u8 {
        return self.src[self.start..self.end];
    }

    pub fn startLoc(self: @This()) Loc {
        return Loc.inSrc(self.src, self.start);
    }

    pub fn sp(self: @This(), comptime T: type, val: T) Sp(T) {
        return .{ .val = val, .span = self };
    }

    pub fn merge(self: @This(), other: Span) Span {
        return .{
            .start = self.start,
            .end = other.end,
            .file = self.file,
            .src = self.src,
        };
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

pub const Token = struct {
    tag: Tag,
    span: Span,

    pub const keywords = [_]Tag{ .shape, .layout, .origin, .proc };

    pub const Tag = enum {
        ident,
        num,
        open_paren,
        close_paren,
        open_bracket,
        close_bracket,
        open_curly,
        close_curly,
        colon,
        semicolon,
        comma,
        dot,
        equals,
        eof,
        plus,
        minus,
        star,
        slash,
        // Keywords
        shape,
        layout,
        origin,
        proc,

        pub fn format(self: Tag, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
            return switch (self) {
                .ident => writer.print("identifier", .{}),
                .num => writer.print("number", .{}),
                .open_paren => writer.print("`(`", .{}),
                .close_paren => writer.print("`)`", .{}),
                .open_bracket => writer.print("`[`", .{}),
                .close_bracket => writer.print("`]`", .{}),
                .open_curly => writer.print("`{{`", .{}),
                .close_curly => writer.print("`}}`", .{}),
                .colon => writer.print("`:`", .{}),
                .semicolon => writer.print("`;`", .{}),
                .comma => writer.print("`,`", .{}),
                .dot => writer.print("`.`", .{}),
                .equals => writer.print("`=`", .{}),
                .eof => writer.print("end of file", .{}),
                .plus => writer.print("`+`", .{}),
                .minus => writer.print("`-`", .{}),
                .star => writer.print("`*`", .{}),
                .slash => writer.print("`/`", .{}),
                .shape => writer.print("`shape`", .{}),
                .layout => writer.print("`layout`", .{}),
                .origin => writer.print("`origin`", .{}),
                .proc => writer.print("`proc`", .{}),
            };
        }
    };

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self.tag) {
            .ident => try writer.print("`{s}`", .{self.span.str()}),
            .num => try writer.print("`{s}`", .{self.span.str()}),
            else => try writer.print("{}", .{self.tag}),
        }
    }
};

pub const LexedFile = struct {
    tokens: std.ArrayList(Token),
    inlayout: []const u8,
    alloc: std.mem.Allocator,

    pub fn deinit(self: *const LexedFile) void {
        self.tokens.deinit();
        self.alloc.free(self.inlayout);
    }
};

pub fn lexFile(path: []const u8, alloc: std.mem.Allocator) !LexedFile {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const inlayout = try file.readToEndAlloc(alloc, std.math.maxInt(usize));
    const tokens = try lex(path, inlayout, alloc);
    return .{ .tokens = tokens, .inlayout = inlayout, .alloc = alloc };
}

pub fn lex(path: []const u8, src: []const u8, alloc: std.mem.Allocator) !std.ArrayList(Token) {
    var lexer = Lexer{
        .curr = 0,
        .file = path,
        .src = src,
        .iter = (try std.unicode.Utf8View.init(src)).iterator(),
        .tokens = std.ArrayList(Token).init(alloc),
    };
    try lexer.go();
    return lexer.tokens;
}

const Lexer = struct {
    curr: usize,
    file: []const u8,
    src: []const u8,
    iter: std.unicode.Utf8Iterator,
    tokens: std.ArrayList(Token),

    fn currChar(self: *Lexer) []const u8 {
        return self.iter.peek(1);
    }

    fn progress(self: *Lexer, cp: []const u8) void {
        self.curr += cp.len;
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
        if (cp.len == 0) return false;
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

    fn addToken(self: *Lexer, start: usize, tag: Token.Tag) !void {
        const span = .{ .start = start, .end = self.curr, .file = self.file, .src = self.src };
        try self.tokens.append(.{ .tag = tag, .span = span });
    }

    fn go(self: *Lexer) !void {
        lex_loop: while (true) {
            const start = self.curr;
            const cp = self.next() orelse break;
            switch (cp[0]) {
                ' ', '\t', '\r', '\n' => {},
                '(' => try self.addToken(start, .open_paren),
                ')' => try self.addToken(start, .close_paren),
                '[' => try self.addToken(start, .open_bracket),
                ']' => try self.addToken(start, .close_bracket),
                '{' => try self.addToken(start, .open_curly),
                '}' => try self.addToken(start, .close_curly),
                ':' => try self.addToken(start, .colon),
                ';' => try self.addToken(start, .semicolon),
                ',' => try self.addToken(start, .comma),
                '.' => try self.addToken(start, .dot),
                '=' => try self.addToken(start, .equals),
                '+' => try self.addToken(start, .plus),
                '-' => try self.addToken(start, .minus),
                '*' => try self.addToken(start, .star),
                '/' => try self.addToken(start, .slash),
                else => {
                    if (isIdentHead(cp)) {
                        // Idents and keywords
                        while (self.nextIf(isIdentTail)) |_| {}
                        const ident = self.src[start..self.curr];
                        inline for (Token.keywords) |keyword| {
                            if (std.mem.eql(u8, ident, @tagName(keyword))) {
                                try self.addToken(start, keyword);
                                continue :lex_loop;
                            }
                        }
                        try self.addToken(start, .ident);
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
                        try self.addToken(start, .num);
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
