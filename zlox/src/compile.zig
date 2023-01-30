const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Scanner = @import("scanner.zig");
const Token = Scanner.Token;
const TokenType = Scanner.TokenType;

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const Opcode = _chunk.Opcode;

const Value = @import("value.zig").Value;
const common = @import("common.zig");

const Precedence = enum {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equalitu, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,
};

pub fn Compiler(comptime EW: type) type {
    return struct {
        const Self = @This();

        const ParseRule = struct {
            prefix: ?*const fn (*Self) anyerror!void = null,
            infix: ?*const fn (*Self) anyerror!void = null,
            precedence: Precedence = .None,
        };
        const ParseRuleTable = std.EnumArray(TokenType, ParseRule);

        // const noob = std.math.log2_int(comptime T: type, x: T)
        const rules = ParseRuleTable.init(.{
            .LeftParen = ParseRule{
                .prefix = Self.grouping,
            },
            .RightParen = ParseRule{},
            .LeftBrace = ParseRule{},
            .RightBrace = ParseRule{},
            .Comma = ParseRule{},
            .Dot = ParseRule{},
            .Minus = ParseRule{ .prefix = Self.unary, .infix = Self.binary, .precedence = Precedence.Term },
            .Plus = ParseRule{ .infix = Self.binary, .precedence = Precedence.Term },
            .Semicolon = ParseRule{},
            .Slash = ParseRule{ .infix = Self.binary, .precedence = Precedence.Factor },
            .Star = ParseRule{ .infix = Self.binary, .precedence = Precedence.Factor },
            .Bang = ParseRule{ .prefix = Self.unary },
            .BangEqual = ParseRule{ .infix = Self.binary, .precedence = Precedence.Equalitu},
            .Equal = ParseRule{},
            .EqualEqual = ParseRule{ .infix = Self.binary, .precedence = Precedence.Equalitu},
            .Greater = ParseRule{ .infix = Self.binary, .precedence = Precedence.Comparison},
            .GreaterEqual = ParseRule{ .infix = Self.binary, .precedence = Precedence.Comparison },
            .Less = ParseRule{ .infix = Self.binary, .precedence = Precedence.Comparison },
            .LessEqual = ParseRule{ .infix = Self.binary, .precedence = Precedence.Comparison },
            .Identifier = ParseRule{},
            .String = ParseRule{},
            .Number = ParseRule{ .prefix = Self.number },
            .And = ParseRule{},
            .Class = ParseRule{},
            .Else = ParseRule{},
            .False = ParseRule{ .prefix = Self.literal },
            .For = ParseRule{},
            .Fun = ParseRule{},
            .If = ParseRule{},
            .Nil = ParseRule{ .prefix = Self.literal },
            .Or = ParseRule{},
            .Print = ParseRule{},
            .Return = ParseRule{},
            .Super = ParseRule{},
            .This = ParseRule{},
            .True = ParseRule{ .prefix = Self.literal },
            .Var = ParseRule{},
            .While = ParseRule{},
            .Error = ParseRule{},
            .Eof = ParseRule{},
        });

        allocator: Allocator,
        errw: EW,
        scanner: Scanner,
        parser: *Parser,
        chunk: *Chunk,

        pub fn init(allocator: Allocator, errw: EW, source: []const u8, chunk: *Chunk) !Self {
            return Self{
                .allocator = allocator,
                .errw = errw,
                .scanner = Scanner.init(allocator, source),
                .parser = try allocator.create(Parser),
                .chunk = chunk,
            };
        }

        pub fn compile(self: *Self) !bool {
            // self.scanner.scan();
            self.advance();
            try self.expression();
            self.consume(TokenType.Eof, "Expect end of expression.");

            return !self.parser.had_error;
        }

        fn emit_op(self: *Self, op: Opcode) !void {
            try self.emit_byte(@enumToInt(op));
        }

        fn emit_byte(self: *Self, byte: u8) !void {
            try self.current_chunk().write_byte(self.allocator, byte, self.parser.previous.line);
        }

        fn emit_bytes(self: *Self, comptime n: usize, bytes: *const [n]u8) !void {
            inline for (bytes) |byte| {
                try self.emit_byte(byte);
            }
        }

        fn emit_return(self: *Self) !void {
            try self.current_chunk().write_op(self.allocator, .Return, self.parser.previous.line);
        }

        fn emit_constant(self: *Self, value: Value) !void {
            const bytes = &[_]u8{ @enumToInt(Opcode.Constant), try self.make_constant(value) };
            try self.emit_bytes(bytes.len, bytes);
        }

        fn make_constant(self: *Self, value: Value) !u8 {
            const constant = try self.current_chunk().add_constant(self.allocator, value);
            if (constant > std.math.maxInt(u8)) {
                self.report_error("Too many constants in one chunk.");
                return 0;
            }
            return constant;
        }

        pub inline fn current_chunk(self: *Self) *Chunk {
            return self.chunk;
        }

        pub fn end(self: *Self) void {
            if (comptime common.PRINT_CODE) {
                self.current_chunk().disassemble("code");
            }
            self.emit_return();
        }

        pub fn expression(self: *Self) !void {
            try self.parse_precedence(.Assignment);
        }

        pub fn grouping(self: *Self) !void {
            try self.expression();
            self.consume(TokenType.RightParen, "Expect ')' after expression.");
        }

        pub fn unary(self: *Self) !void {
            const token_type = self.parser.previous.type;
            try self.parse_precedence(.Unary);
            switch (token_type) {
                .Minus => try self.emit_op(.Negate),
                .Bang => try self.emit_op(.Not),
                else => unreachable,
            }
        }

        pub fn binary(self: *Self) !void {
            const operator_type = self.parser.previous.type;
            const rule = Self.get_rule(operator_type);
            try self.parse_precedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

            switch (operator_type) {
                .BangEqual => try self.emit_bytes(2, &[_]u8{ @enumToInt(Opcode.Equal), @enumToInt(Opcode.Not) }),
                .EqualEqual => try self.emit_op(.Equal),
                .Greater => try self.emit_op(Opcode.Greater),
                .GreaterEqual => try self.emit_bytes(2, &[_]u8{ @enumToInt(Opcode.Less), @enumToInt(Opcode.Not) }),
                .Less => try self.emit_op(Opcode.Less),
                .LessEqual => try self.emit_bytes(2, &[_]u8{ @enumToInt(Opcode.Greater), @enumToInt(Opcode.Not) }),

                .Plus => try self.emit_op(.Add),
                .Minus => try self.emit_op(.Subtract),
                .Star => try self.emit_op(.Multiply),
                .Slash => try self.emit_op(.Divide),
                else => unreachable,
            }
        }

        pub fn parse_precedence(self: *Self, precedence: Precedence) !void {
            self.advance();
            const prefix_rule = Self.get_rule(self.parser.previous.type).prefix orelse {
                self.report_error("Expect expression.");
                return;
            };
            try prefix_rule(self);

            const precedence_int = @enumToInt(precedence);
            while (precedence_int <= @enumToInt(Self.get_rule(self.parser.current.type).precedence)) {
                self.advance();
                const infix_rule = Self.get_rule(self.parser.previous.type).infix orelse {
                    @panic(std.fmt.allocPrint(self.allocator, "No infix expression found for {s}\n", .{self.parser.previous.type.name()}) catch unreachable);
                };
                try infix_rule(self);
            }
        }

        fn number(self: *Self) !void {
            const value = std.fmt.parseFloat(f64, self.parser.previous.content[0..self.parser.previous.len]) catch {
                self.error_at(self.parser.previous, "Invalid number");
                return;
            };

            try self.emit_constant(Value.number(value));
        }

        fn literal(self: *Self) !void {
            switch (self.parser.previous.type) {
                .True => try self.emit_constant(Value.boolean(true)),
                .False => try self.emit_constant(Value.boolean(false)),
                .Nil => try self.emit_constant(Value.nil()),
                else => {},
            }
        }

        pub fn get_rule(token_type: TokenType) ParseRule {
            return rules.get(token_type);
        }

        pub fn consume(self: *Self, comptime token_type: TokenType, message: []const u8) void {
            if (self.parser.current.type == token_type) {
                self.advance();
            } else {
                self.error_at_current(message);
            }
        }

        pub fn advance(self: *Self) void {
            self.parser.previous = self.parser.current;

            while (true) {
                const token = self.scanner.scan_token();
                if (token.type != TokenType.Error) {
                    self.parser.current = token;
                    break;
                }

                self.error_at_current(token.content[0..token.len]);
            }
        }

        pub fn error_at_current(self: *Self, message: []const u8) void {
            self.error_at(self.parser.current, message);
        }

        pub fn report_error(self: *Self, message: []const u8) void {
            self.error_at(self.parser.previous, message);
        }

        pub fn error_at(self: *Self, token: Token, message: []const u8) void {
            if (self.parser.panic_mode) return;

            self.parser.panic_mode = true;
            self.errw.print("[line {d}] Error", .{token.line}) catch |err| {
                debug.print("Error writing to errw: {}\n", .{err});
            };

            if (token.type == .Eof) {
                self.errw.print(" at end", .{}) catch |err| {
                    debug.print("Error writing to errw: {}\n", .{err});
                };
            } else if (token.type == .Error) {
                // Nothing.
            } else {
                self.errw.print(" at '{s}'", .{token.content[0..token.len]}) catch |err| {
                    debug.print("Error writing to errw: {}\n", .{err});
                };
            }

            self.errw.print(": {s}\n", .{message}) catch |err| {
                debug.print("Error writing to errw: {}\n", .{err});
            };
            self.parser.had_error = true;
        }
    };
}

const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool = false,
    panic_mode: bool = false,

    pub fn init() Parser {
        return Parser{
            .current = undefined,
            .previous = undefined,
        };
    }
};
