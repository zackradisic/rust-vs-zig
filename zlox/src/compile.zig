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
const GC = @import("gc.zig");

const Obj = @import("obj.zig");

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

pub const Local = struct {
    name: Token,
    depth: i32,
};

pub fn Compiler(comptime EW: type) type {
    return struct {
        const Self = @This();

        gc: *GC,
        errw: EW,
        scanner: *Scanner,
        parser: *Parser,
        chunk: *Chunk,
        locals: [std.math.maxInt(u8)]Local,
        local_count: u32,
        scope_depth: u32,

        const ParseRule = struct {
            prefix: ?*const fn (*Self, bool) Allocator.Error!void = null,
            infix: ?*const fn (*Self, bool) Allocator.Error!void = null,
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
            .BangEqual = ParseRule{ .infix = Self.binary, .precedence = Precedence.Equalitu },
            .Equal = ParseRule{},
            .EqualEqual = ParseRule{ .infix = Self.binary, .precedence = Precedence.Equalitu },
            .Greater = ParseRule{ .infix = Self.binary, .precedence = Precedence.Comparison },
            .GreaterEqual = ParseRule{ .infix = Self.binary, .precedence = Precedence.Comparison },
            .Less = ParseRule{ .infix = Self.binary, .precedence = Precedence.Comparison },
            .LessEqual = ParseRule{ .infix = Self.binary, .precedence = Precedence.Comparison },
            .Identifier = ParseRule{ .prefix = Self.variable },
            .String = ParseRule{ .prefix = Self.string },
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

        pub fn init(gc: *GC, errw: EW, source: []const u8, chunk: *Chunk, scanner: *Scanner, parser: *Parser) Allocator.Error!Self {
            _ = source;
            return Self{
                .gc = gc,
                .errw = errw,
                .scanner = scanner,
                .parser = parser,
                .chunk = chunk,
                .locals = undefined,
                .local_count = 0,
                .scope_depth = 0,
            };
        }

        pub fn init_parser() Parser {
            return Parser.init();
        }

        // pub fn free();

        pub fn compile(self: *Self) Allocator.Error!bool {
            self.advance();

            while (!self.match_tok(.Eof)) {
                try self.declaration();
            }

            // self.consume(TokenType.Eof, "Expect end of expression.");
            try self.end();

            return !self.parser.had_error;
        }

        fn emit_op(self: *Self, op: Opcode) Allocator.Error!void {
            try self.emit_byte(@enumToInt(op));
        }

        fn emit_byte(self: *Self, byte: u8) Allocator.Error!void {
            try self.current_chunk().write_byte(self.gc.allocator, byte, self.parser.previous.line);
        }

        fn emit_bytes(self: *Self, comptime n: usize, bytes: *const [n]u8) Allocator.Error!void {
            if (comptime n != bytes.len) {
                @compileError("emit_bytes: n != bytes.len");
            }
            comptime var i = 0;
            inline while (i < n) : (i += 1) {
                try self.emit_byte(bytes[i]);
            }
        }

        fn emit_return(self: *Self) Allocator.Error!void {
            try self.current_chunk().write_op(self.gc.allocator, .Return, self.parser.previous.line);
        }

        fn emit_constant(self: *Self, value: Value) Allocator.Error!void {
            const bytes = &[_]u8{ @enumToInt(Opcode.Constant), try self.make_constant(value) };
            try self.emit_bytes(bytes.len, bytes);
        }

        fn make_constant(self: *Self, value: Value) Allocator.Error!u8 {
            const constant = try self.current_chunk().add_constant(self.gc.allocator, value);
            if (constant > std.math.maxInt(u8)) {
                self.report_error("Too many constants in one chunk.");
                return 0;
            }
            return constant;
        }

        inline fn current_chunk(self: *Self) *Chunk {
            return self.chunk;
        }

        fn end(self: *Self) Allocator.Error!void {
            if (comptime common.PRINT_CODE) {
                self.current_chunk().disassemble("code");
            }
            try self.emit_return();
        }

        fn begin_scope(self: *Self) void {
            self.scope_depth += 1;
        }

        fn end_scope(self: *Self) Allocator.Error!void {
            self.scope_depth -= 1;

            while (self.local_count > 0 and self.locals[self.local_count - 1].depth > self.scope_depth) {
                try self.emit_op(.Pop);
                self.local_count -= 1;
            }
        }

        fn expression(self: *Self) Allocator.Error!void {
            try self.parse_precedence(.Assignment);
        }

        fn block(self: *Self) Allocator.Error!void {
            while (!self.check(.RightBrace) and !self.check(.Eof)) {
                _ = try self.declaration();
            }

            self.consume(.RightBrace, "Expect '}' after block.");
        }

        fn var_declaration(self: *Self) Allocator.Error!void {
            const global = try self.parse_variable("Expect variable name.");
            if (self.match_tok(TokenType.Equal)) {
                try self.expression();
            } else {
                try self.emit_op(.Nil);
            }
            self.consume(TokenType.Semicolon, "Expect ';' after variable declaration.");
            try self.define_variable(global);
        }

        fn declaration(self: *Self) Allocator.Error!void {
            if (self.match_tok(TokenType.Var)) {
                try self.var_declaration();
            } else {
                try self.statement();
            }
            if (self.parser.panic_mode) {
                self.synchronize();
            }
        }

        fn statement(self: *Self) Allocator.Error!void {
            if (self.match_tok(TokenType.Print)) {
                try self.print_statement();
            } else if (self.match_tok(TokenType.LeftBrace)) {
                self.begin_scope();
                try self.block();
                try self.end_scope();
            } else {
                try self.expression_statement();
            }
        }

        fn expression_statement(self: *Self) Allocator.Error!void {
            try self.expression();
            self.consume(TokenType.Semicolon, "Expect ';' after expression.");
            try self.emit_op(.Pop);
        }

        fn print_statement(self: *Self) Allocator.Error!void {
            try self.expression();
            self.consume(TokenType.Semicolon, "Expect ';' after value.");
            try self.emit_op(.Print);
        }

        fn grouping(self: *Self, can_assign: bool) Allocator.Error!void {
            _ = can_assign;
            try self.expression();
            self.consume(TokenType.RightParen, "Expect ')' after expression.");
        }

        fn unary(self: *Self, can_assign: bool) Allocator.Error!void {
            _ = can_assign;
            const token_type = self.parser.previous.type;
            try self.parse_precedence(.Unary);
            switch (token_type) {
                .Minus => try self.emit_op(.Negate),
                .Bang => try self.emit_op(.Not),
                else => unreachable,
            }
        }

        fn binary(self: *Self, can_assign: bool) Allocator.Error!void {
            _ = can_assign;
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

        fn parse_precedence(self: *Self, precedence: Precedence) Allocator.Error!void {
            self.advance();
            const prefix_rule = Self.get_rule(self.parser.previous.type).prefix orelse {
                self.report_error("Expect expression.");
                return;
            };
            const can_assign = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
            try prefix_rule(self, can_assign);

            const precedence_int = @enumToInt(precedence);
            while (precedence_int <= @enumToInt(Self.get_rule(self.parser.current.type).precedence)) {
                self.advance();
                const infix_rule = Self.get_rule(self.parser.previous.type).infix orelse {
                    @panic(std.fmt.allocPrint(self.gc.allocator, "No infix expression found for {s}\n", .{self.parser.previous.type.name()}) catch unreachable);
                };
                try infix_rule(self, can_assign);
            }

            if (can_assign and self.match_tok(TokenType.Equal)) {
                self.report_error("Invalid assignment target.");
            }
        }

        fn parse_variable(self: *Self, error_message: []const u8) Allocator.Error!u8 {
            self.consume(TokenType.Identifier, error_message);

            self.declare_variable();
            if (self.scope_depth > 0) return 0;

            return self.identifier_constant(&self.parser.previous);
        }

        fn mark_initialized(self: *Self) void {
            self.locals[self.local_count - 1].depth = @intCast(i32, self.scope_depth);
        }

        fn define_variable(self: *Self, global: u8) Allocator.Error!void {
            if (self.scope_depth > 0) {
                self.mark_initialized();
                return;
            }
            return self.emit_bytes(2, &[_]u8{ @enumToInt(Opcode.DefineGlobal), global });
        }

        fn declare_variable(self: *Self) void {
            if (self.scope_depth == 0) return;

            const name = self.parser.previous;
            const locals = self.locals[0..self.local_count];
            var i = @intCast(i64, self.local_count) - 1;
            while (i >= 0) : (i -= 1) {
                const local = locals[@intCast(usize, i)];
                if (local.depth != -1 and local.depth < self.scope_depth) {
                    break;
                }

                if (identifiers_equal(&name, &local.name)) {
                    self.report_error("Already a variable with this name in this scope.");
                }
            }

            self.add_local(name);
        }

        fn add_local(self: *Self, name: Token) void {
            if (self.local_count == std.math.maxInt(u8)) {
                self.report_error("Too many local variables in function.");
                return;
            }
            var local = &self.locals[self.local_count];
            self.local_count += 1;
            local.name = name;
            local.depth = -1;
        }

        fn resolve_local(self: *Self, name: *Token) i32 {
            const locals = self.locals[0..self.local_count];
            var i = @intCast(i64, self.local_count) - 1;
            while (i >= 0) : (i -= 1) {
                const local = locals[@intCast(usize, i)];
                if (identifiers_equal(name, &local.name)) {
                    if (local.depth == -1) {
                        self.report_error("Can't read local variable in its own initializer.");
                    }
                    return @intCast(i32, i);
                }
            }
            return -1;
        }

        fn identifier_constant(self: *Self, name: *Token) Allocator.Error!u8 {
            return try self.make_constant(Value.obj((try self.gc.copy(name.content, name.len)).widen()));
        }

        fn identifiers_equal(a: *const Token, b: *const Token) bool {
            return std.mem.eql(u8, a.content[0..a.len], b.content[0..b.len]);
        }

        fn number(self: *Self, can_assign: bool) Allocator.Error!void {
            _ = can_assign;
            const value = std.fmt.parseFloat(f64, self.parser.previous.content[0..self.parser.previous.len]) catch {
                self.error_at(self.parser.previous, "Invalid number");
                return;
            };

            try self.emit_constant(Value.number(value));
        }

        fn literal(self: *Self, can_assign: bool) Allocator.Error!void {
            _ = can_assign;
            switch (self.parser.previous.type) {
                .True => try self.emit_constant(Value.boolean(true)),
                .False => try self.emit_constant(Value.boolean(false)),
                .Nil => try self.emit_constant(Value.nil()),
                else => {},
            }
        }

        fn string(self: *Self, can_assign: bool) Allocator.Error!void {
            _ = can_assign;
            const obj_string = try self.gc.copy(self.parser.previous.content + 1, self.parser.previous.len - 2);
            try self.emit_constant(Value.obj(obj_string.widen()));
        }

        fn variable(self: *Self, can_assign: bool) Allocator.Error!void {
            try self.named_variable(&self.parser.previous, can_assign);
        }

        fn named_variable(self: *Self, name: *Token, can_assign: bool) Allocator.Error!void {
            var get_op: Opcode = undefined;
            var set_op: Opcode = undefined;
            var arg = self.resolve_local(name);

            if (arg != -1) {
                get_op = .GetLocal;
                set_op = .SetLocal;
            } else {
                arg = try self.identifier_constant(name);
                get_op = .GetGlobal;
                set_op = .SetGlobal;
            }

            if (can_assign and self.match_tok(.Equal)) {
                try self.expression();
                try self.emit_bytes(2, &[_]u8{ @enumToInt(set_op), @intCast(u8, arg) });
            } else {
                try self.emit_bytes(2, &[_]u8{ @enumToInt(get_op), @intCast(u8, arg) });
            }
        }

        fn get_rule(token_type: TokenType) ParseRule {
            return rules.get(token_type);
        }

        fn consume(self: *Self, comptime token_type: TokenType, message: []const u8) void {
            if (self.parser.current.type == token_type) {
                self.advance();
            } else {
                self.error_at_current(message);
            }
        }

        fn advance(self: *Self) void {
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

        fn match_tok(self: *Self, ty: TokenType) bool {
            if (!self.check(ty)) {
                return false;
            }
            self.advance();
            return true;
        }

        fn check(self: *Self, ty: TokenType) bool {
            return self.parser.current.type == ty;
        }

        fn error_at_current(self: *Self, message: []const u8) void {
            self.error_at(self.parser.current, message);
        }

        fn report_error(self: *Self, message: []const u8) void {
            self.error_at(self.parser.previous, message);
        }

        fn error_at(self: *Self, token: Token, message: []const u8) void {
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

        fn synchronize(self: *Self) void {
            self.parser.panic_mode = false;

            while (self.parser.current.type != .Eof) {
                if (self.parser.previous.type == .Semicolon) return;

                switch (self.parser.current.type) {
                    .Class, .Fun, .Var, .For, .If, .While, .Print, .Return => return,
                    else => {},
                }

                self.advance();
            }
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
