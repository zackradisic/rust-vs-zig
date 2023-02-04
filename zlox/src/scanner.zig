const std = @import("std");
const debug = std.debug;

pub const TokenType = enum {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,

    /// From: https://zig.news/rbino/using-comptime-to-invert-bijective-functions-on-enums-3pmk
    /// explanation in the comments
    pub fn name(self: TokenType) []const u8 {
        // This won't work for some reason says `idx` is not comptime known
        // const str = comptime blk: {
        //     const idx = @enumToInt(self);
        //     const str = @typeInfo(TokenType).Enum.fields[idx].name;
        //     break :blk str;
        // };
        // return str;

        switch (self) {
            inline else => |shape| {
                const str = comptime blk: {
                    const idx = @enumToInt(shape);
                    const str = @typeInfo(TokenType).Enum.fields[idx].name;
                    break :blk str;
                };
                return str;
            },
        }
    }
};

pub const TokenLen = u47;
pub const Token = struct {
    type: TokenType,
    line: u16,
    len: u32,
    content: [*]const u8,

    pub fn debug(self: Token, writer: anytype) void {
        writer.print("Token{{type: {s}, content: \"{s}\"}}", .{ @tagName(self.type), self.content[0..self.len] });
    }
};

const Scanner = @This();

end: [*]const u8,
start: [*]const u8,
current: [*]const u8,
line: u16,

pub fn init(source: []const u8) Scanner {
    var start = @ptrCast([*]const u8, source);
    return Scanner{
        .end = @ptrCast([*]const u8, if (source.len == 0) &start[0] else &source[source.len - 1]),
        .start = start,
        .current = @ptrCast([*]const u8, source),
        .line = 1,
    };
}

pub fn scan(self: *Scanner) void {
    var line: i64 = -1;

    while (true) {
        const token = self.scan_token();
        if (token.line != line) {
            line = token.line;
            debug.print("line {}: ", .{line});
        }

        token.debug(debug);
        debug.print("\n", .{});

        if (token.type == .Eof) break;
    }
}

pub fn scan_token(self: *Scanner) Token {
    self.skip_whitespace();
    self.start = self.current;
    if (self.is_at_end()) return self.make_token(TokenType.Eof);

    const c = self.advance();

    if (is_digit(c)) return self.number();
    if (is_alpha(c)) return self.identifier();

    switch (c) {
        '(' => return self.make_token(TokenType.LeftParen),
        ')' => return self.make_token(TokenType.RightParen),
        '{' => return self.make_token(TokenType.LeftBrace),
        '}' => return self.make_token(TokenType.RightBrace),
        ';' => return self.make_token(TokenType.Semicolon),
        ',' => return self.make_token(TokenType.Comma),
        '.' => return self.make_token(TokenType.Dot),
        '-' => return self.make_token(TokenType.Minus),
        '+' => return self.make_token(TokenType.Plus),
        '/' => return self.make_token(TokenType.Slash),
        '*' => return self.make_token(TokenType.Star),

        '!' => return if (self.match('=')) self.make_token(TokenType.BangEqual) else self.make_token(TokenType.Bang),
        '=' => return if (self.match('=')) self.make_token(TokenType.EqualEqual) else self.make_token(TokenType.Equal),
        '<' => return if (self.match('=')) self.make_token(TokenType.LessEqual) else self.make_token(TokenType.Less),
        '>' => return if (self.match('=')) self.make_token(TokenType.GreaterEqual) else self.make_token(TokenType.Greater),

        '"' => return self.string(),
        else => {},
    }

    debug.print("unknown character: {c}\n", .{c});
    return self.error_token("Unexpected character.");
}

pub fn string(self: *Scanner) Token {
    while (self.peek() != '"' and !self.is_at_end()) {
        if (self.peek() == '\n') self.line += 1;
        _ = self.advance();
    }

    if (self.is_at_end()) return self.error_token("Unterminated string.");
    _ = self.advance();
    return self.make_token(TokenType.String);
}

pub fn number(self: *Scanner) Token {
    while (is_digit(self.peek())) {
        _ = self.advance();
    }

    if (self.peek() == '.' and is_digit(self.peek_next())) {
        _ = self.advance();
        while (is_digit(self.peek())) {
            _ = self.advance();
        }
    }

    return self.make_token(TokenType.Number);
}

pub fn identifier(self: *Scanner) Token {
    while (is_alpha(self.peek()) or is_digit(self.peek())) {
        _ = self.advance();
    }

    return self.make_token(self.identifier_type());
}

pub fn match(self: *Scanner, comptime expected: u8) bool {
    if (self.is_at_end()) return false;
    if (self.peek() != expected) return false;

    self.current += 1;
    return true;
}

pub fn make_token(self: *Scanner, token_type: TokenType) Token {
    if (token_type != TokenType.Eof) {
        return Token{
            .type = token_type,
            .content = self.start,
            .len = @intCast(u16, @ptrToInt(self.current) - @ptrToInt(self.start)),
            .line = self.line,
        };
    } else {
        return Token{
            .type = token_type,
            .content = "",
            .len = 0,
            .line = self.line,
        };
    }
}

pub fn error_token(self: *Scanner, message: []const u8) Token {
    return Token{
        .type = TokenType.Error,
        .content = @ptrCast([*]const u8, message),
        .len = @intCast(u16, message.len),
        .line = self.line,
    };
}

pub fn is_at_end(self: *Scanner) bool {
    return @ptrToInt(self.current) > @ptrToInt(self.end);
}

pub fn advance(self: *Scanner) u8 {
    const ret = self.current[0];
    self.current += 1;
    return ret;
}

pub fn peek(self: *Scanner) u8 {
    return self.current[0];
}

pub fn peek_next(self: *Scanner) u8 {
    if (self.is_at_end()) return 0;
    return self.current[1];
}

pub fn skip_whitespace(self: *Scanner) void {
    while (true) {
        const c = self.peek();
        switch (c) {
            ' ', '\r', '\t' => {
                _ = self.advance();
            },
            '\n' => {
                self.line += 1;
                _ = self.advance();
            },
            '/' => {
                if (self.peek_next() == '/') {
                    // A comment goes until the end of the line.
                    while (self.peek() != '\n' and !self.is_at_end()) {
                        _ = self.advance();
                    }
                } else {
                    return;
                }
            },
            else => return,
        }
    }
}

pub fn is_digit(c: u8) bool {
    return '0' <= c and c <= '9';
}

pub fn is_alpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}

pub fn identifier_type(self: *Scanner) TokenType {
    switch (self.start[0]) {
        'a' => return self.check_keyword(1, "nd", TokenType.And),
        'c' => return self.check_keyword(1, "lass", TokenType.Class),
        'e' => return self.check_keyword(1, "lse", TokenType.Else),
        'f' => {
            if (@ptrToInt(self.current) - @ptrToInt(self.start) > 1) {
                switch (self.start[1]) {
                    'a' => return self.check_keyword(2, "lse", TokenType.False),
                    'o' => return self.check_keyword(2, "r", TokenType.For),
                    'u' => return self.check_keyword(2, "n", TokenType.Fun),
                    else => return TokenType.Identifier,
                }
            } else {
                return TokenType.Identifier;
            }
        },
        'i' => return self.check_keyword(1, "f", TokenType.If),
        'n' => return self.check_keyword(1, "il", TokenType.Nil),
        'o' => return self.check_keyword(1, "r", TokenType.Or),
        'p' => return self.check_keyword(1, "rint", TokenType.Print),
        'r' => return self.check_keyword(1, "eturn", TokenType.Return),
        's' => return self.check_keyword(1, "uper", TokenType.Super),
        't' => {
            if (@ptrToInt(self.current) - @ptrToInt(self.start) > 1) {
                switch (self.start[1]) {
                    'h' => return self.check_keyword(2, "is", TokenType.This),
                    'r' => return self.check_keyword(2, "ue", TokenType.True),
                    else => return TokenType.Identifier,
                }
            } else {
                return TokenType.Identifier;
            }
        },
        'v' => return self.check_keyword(1, "ar", TokenType.Var),
        'w' => return self.check_keyword(1, "hile", TokenType.While),
        else => return TokenType.Identifier,
    }
}

pub fn check_keyword(self: *Scanner, start: usize, rest: []const u8, token_type: TokenType) TokenType {
    const len = rest.len;
    const tgt = self.start[start .. len + 1];
    const lhs = @ptrToInt(self.current) - @ptrToInt(self.start);
    const rhs = start + len;
    _ = lhs;
    _ = rhs;
    if (@ptrToInt(self.current) - @ptrToInt(self.start) == start + len and std.mem.eql(u8, tgt, rest)) {
        return token_type;
    }

    return TokenType.Identifier;
}
