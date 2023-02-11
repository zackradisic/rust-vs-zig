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
    is_captured: bool,
};

pub const Upvalue = struct {
    index: u8,
    is_local: bool,
};

pub const FunctionType = enum { Function, Script };

pub fn Compiler(comptime EW: type) type {
    return struct {
        const Self = @This();

        gc: *GC,
        errw: EW,
        enclosing: ?*Self,
        scanner: *Scanner,
        parser: *Parser,
        function: *Obj.Function,
        function_ty: FunctionType,
        local_count: u32,
        upvalues: [std.math.maxInt(u8)]Upvalue,
        scope_depth: u32,
        locals: [std.math.maxInt(u8)]Local,

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
                .infix = Self.call,
                .precedence = Precedence.Call,
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
            .And = ParseRule{ .infix = Self.and_, .precedence = Precedence.And },
            .Class = ParseRule{},
            .Else = ParseRule{},
            .False = ParseRule{ .prefix = Self.literal },
            .For = ParseRule{},
            .Fun = ParseRule{},
            .If = ParseRule{},
            .Nil = ParseRule{ .prefix = Self.literal },
            .Or = ParseRule{ .infix = Self.or_, .precedence = Precedence.Or },
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

        pub fn init(gc: *GC, errw: EW, enclosing: ?*Self, scanner: *Scanner, parser: *Parser, function_ty: FunctionType) Allocator.Error!Self {
            var function = try gc.alloc_obj(Obj.Function);
            try function.init(gc);
            var self = Self{
                .gc = gc,
                .errw = errw,
                .enclosing = enclosing,
                .scanner = scanner,
                .parser = parser,
                .function_ty = function_ty,
                .function = function,
                .locals = undefined,
                .upvalues = undefined,
                .local_count = 0,
                .scope_depth = 0,
            };

            if (function_ty != FunctionType.Script) {
                self.function.name = try gc.copy_string(parser.previous.content, parser.previous.len);
            }

            var local = &self.locals[0];
            self.local_count += 1;
            local.depth = 0;
            local.name.content = "";
            local.name.len = 0;
            local.is_captured = false;

            return self;
        }

        pub fn init_parser() Parser {
            return Parser.init();
        }

        // pub fn free();

        pub fn compile(self: *Self) Allocator.Error!?*Obj.Function {
            self.advance();

            while (!self.match_tok(.Eof)) {
                try self.declaration();
            }

            // self.consume(TokenType.Eof, "Expect end of expression.");
            const function = try self.end();

            if (self.parser.had_error) {
                return null;
            } else {
                return function;
            }
        }

        fn emit_op(self: *Self, op: Opcode) Allocator.Error!void {
            try self.emit_byte(@enumToInt(op));
        }

        fn emit_byte(self: *Self, byte: u8) Allocator.Error!void {
            try self.current_chunk().write_byte(self.gc.allocator, byte, self.parser.previous.line);
        }

        fn emit_u16(self: *Self, val: u16) Allocator.Error!void {
            try self.current_chunk().write_u16(self.gc.allocator, val, self.parser.previous.line);
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

        fn emit_loop(self: *Self, loop_start: usize) Allocator.Error!void {
            try self.emit_op(.Loop);

            const offset = self.current_chunk().code.items.len - loop_start + 2;
            if (offset > std.math.maxInt(u16)) {
                self.report_error("Loop body too large.");
            }

            try self.emit_u16(@truncate(u16, offset));
        }

        fn emit_jump(self: *Self, op: Opcode) Allocator.Error!usize {
            try self.emit_op(op);
            try self.emit_byte(0xff);
            try self.emit_byte(0xff);
            return self.current_chunk().code.items.len - 2;
        }

        fn emit_return(self: *Self) Allocator.Error!void {
            try self.emit_op(.Nil);
            try self.emit_op(.Return);
        }

        fn emit_constant(self: *Self, value: Value) Allocator.Error!void {
            const bytes = &[_]u8{ @enumToInt(Opcode.Constant), try self.make_constant(value) };
            try self.emit_bytes(bytes.len, bytes);
        }

        fn patch_jump(self: *Self, offset: usize) Allocator.Error!void {
            const jump_usize = self.current_chunk().code.items.len - offset - 2;

            if (jump_usize > std.math.maxInt(u16)) {
                self.report_error("Too much code to jump over.");
            }

            const jump = @intCast(u16, jump_usize);

            self.current_chunk().code.items[offset] = @truncate(u8, jump >> 8);
            self.current_chunk().code.items[offset + 1] = @truncate(u8, jump);
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
            return &self.function.chunk;
        }

        fn end(self: *Self) Allocator.Error!*Obj.Function {
            try self.emit_return();
            var function = self.function;
            if (comptime common.PRINT_CODE_AFTER_COMPILE) {
                self.current_chunk().disassemble(if (function.name) |name| name.chars[0..name.len] else "script");
            }
            return function;
        }

        fn begin_scope(self: *Self) void {
            self.scope_depth += 1;
        }

        fn end_scope(self: *Self) Allocator.Error!void {
            self.scope_depth -= 1;

            while (self.local_count > 0 and self.locals[self.local_count - 1].depth > self.scope_depth) {
                if (self.locals[self.local_count - 1].is_captured) {
                    try self.emit_op(.CloseUpvalue);
                } else { 
                    try self.emit_op(.Pop);
                }
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

        fn func(self: *Self, function_type: FunctionType) Allocator.Error!void {
            var compiler = try Self.init(self.gc, self.errw, self, self.scanner, self.parser, function_type);
            compiler.begin_scope();

            compiler.consume(.LeftParen, "Expect '(' after function name.");
            if (!compiler.check(.RightParen)) {
                while(true) {
                    if (compiler.function.arity == std.math.maxInt(u8)) {
                        compiler.error_at_current("Cannot have more than 255 parameters.");
                        break;
                    }
                    compiler.function.arity += 1;
                    const constant = try compiler.parse_variable("Expect parameter name.");
                    try compiler.define_variable(constant);
                    if (!compiler.match_tok(.Comma)) {
                        break;
                    }
                }
            }
            compiler.consume(.RightParen, "Expect ')' after function name.");
            compiler.consume(.LeftBrace, "Expect '{' after function name.");
            try compiler.block();

            const function = try compiler.end();
            try self.emit_bytes(2, &[_]u8{ @enumToInt(Opcode.Closure), try self.make_constant(Value.obj(function.widen())) });

            var i: usize = 0;
            while (i < function.upvalue_count): (i += 1) {
                try self.emit_byte(if (compiler.upvalues[i].is_local) 1 else 0);
                try self.emit_byte(compiler.upvalues[i].index);
            } 
        }

        fn fn_declaration(self: *Self) Allocator.Error!void {
            const global = try self.parse_variable("Expect function name.");
            self.mark_initialized();
            try self.func(FunctionType.Function);
            try self.define_variable(global);
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
            if (self.match_tok(TokenType.Fun)) {
                try self.fn_declaration();
            } else if (self.match_tok(TokenType.Var)) {
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
            } else if (self.match_tok(TokenType.For)) {
                try self.for_statement();
            } else if (self.match_tok(TokenType.If)) {
                try self.if_statement();
            } else if (self.match_tok(TokenType.Return)) {
                try self.return_statement();
            } else if (self.match_tok(TokenType.While)) {
                try self.while_statement();
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

        fn if_statement(self: *Self) Allocator.Error!void {
            self.consume(TokenType.LeftParen, "Expect '(' after 'if'.");
            try self.expression();
            self.consume(TokenType.RightParen, "Expect ')' after condition.");

            const then_jump = try self.emit_jump(.JumpIfFalse);
            try self.emit_op(.Pop);
            try self.statement();

            const else_jump = try self.emit_jump(.Jump);

            try self.patch_jump(then_jump);
            try self.emit_op(.Pop);

            if (self.match_tok(TokenType.Else)) {
                try self.statement();
            }

            try self.patch_jump(else_jump);
        }

        fn print_statement(self: *Self) Allocator.Error!void {
            try self.expression();
            self.consume(TokenType.Semicolon, "Expect ';' after value.");
            try self.emit_op(.Print);
        }

        fn return_statement(self: *Self) Allocator.Error!void {
            if (self.function_ty == FunctionType.Script) {
                self.report_error("Cannot return from top-level code.");
            }

            if (self.match_tok(TokenType.Semicolon)) {
                try self.emit_return();
            } else {
                try self.expression();
                self.consume(TokenType.Semicolon, "Expect ';' after return value.");
                try self.emit_op(.Return);
            }
        }

        fn for_statement(self: *Self) Allocator.Error!void {
            self.begin_scope();

            self.consume(.LeftParen, "Expect '(' after 'for'.");
            if (self.match_tok(.Semicolon)) {
                // No initializer.
            } else if (self.match_tok(.Var)) {
                try self.var_declaration();
            } else {
                try self.expression_statement();
            }

            var loop_start = self.current_chunk().code.items.len;
            var exit_jump: i64 = -1;
            if (!self.match_tok(.Semicolon)) {
                try self.expression();
                self.consume(.Semicolon, "Expect ';' after loop condition.");

                exit_jump = @intCast(i64, try self.emit_jump(.JumpIfFalse));
                try self.emit_op(.Pop);
            }

            if (!self.match_tok(.RightParen)) {
                const body_jump = try self.emit_jump(.Jump);
                const increment_start = self.current_chunk().code.items.len;
                try self.expression();
                try self.emit_op(.Pop);
                self.consume(.RightParen, "Expect ')' after for clauses.");

                try self.emit_loop(loop_start);
                loop_start = increment_start;
                try self.patch_jump(body_jump);
            }

            try self.statement();
            try self.emit_loop(loop_start);

            if (exit_jump != -1) {
                try self.patch_jump(@intCast(usize, exit_jump));
                try self.emit_op(.Pop); // Condition
            }

            try self.end_scope();
        }

        fn while_statement(self: *Self) Allocator.Error!void {
            const loop_start = self.current_chunk().code.items.len;
            self.consume(TokenType.LeftParen, "Expect '(' after 'while'.");
            try self.expression();
            self.consume(TokenType.RightParen, "Expect ')' after condition.");

            const exit_jump = try self.emit_jump(.JumpIfFalse);
            try self.emit_op(.Pop);
            try self.statement();
            try self.emit_loop(loop_start);

            try self.patch_jump(exit_jump);
            try self.emit_op(.Pop);
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

        fn call(self: *Self, can_assign: bool) Allocator.Error!void {
            _ = can_assign;
            const arg_count = try self.argument_list();
            try self.emit_bytes(2, &[_]u8{ @enumToInt(Opcode.Call),  arg_count });
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
            if (self.scope_depth == 0) return;
            self.locals[self.local_count - 1].depth = @intCast(i32, self.scope_depth);
        }

        fn argument_list(self: *Self) Allocator.Error!u8 {
            var arg_count: u8 = 0;

            if (!self.check(.RightParen)) {
                while(true) {
                    try self.expression();
                    if (arg_count == std.math.maxInt(u8)) {
                        self.report_error("Can't have more than 255 arguments.");
                    }
                    arg_count += 1;
                    if (!self.match_tok(.Comma)) {
                        break;
                    }
                }
            }

            self.consume(.RightParen, "Expect ')' after arguments.");

            return arg_count;
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

        fn and_(self: *Self, can_assign: bool) Allocator.Error!void {
            _ = can_assign;
            const end_jump = try self.emit_jump(.JumpIfFalse);

            try self.emit_op(.Pop);
            try self.parse_precedence(.And);

            try self.patch_jump(end_jump);
        }

        fn or_(self: *Self, can_assign: bool) Allocator.Error!void {
            _ = can_assign;
            const else_jump = try self.emit_jump(.JumpIfFalse);
            const end_jump = try self.emit_jump(.Jump);

            try self.patch_jump(else_jump);
            try self.emit_op(.Pop);

            try self.parse_precedence(.Or);
            try self.patch_jump(end_jump);
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
            local.is_captured = false;
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

        fn resolve_upvalue(self: *Self, name: *Token) i32 {
            const enclosing = self.enclosing orelse return -1;

            const str = name.content[0..name.len];
            _ = str;

            const local = enclosing.resolve_local(name);
            if (local != -1) {
                enclosing.locals[@intCast(usize, local)].is_captured = true;
                return self.add_upvalue(@intCast(u8, local), true);
            }

            const upvalue = enclosing.resolve_upvalue(name);
            if (upvalue != -1) {
                return self.add_upvalue(@intCast(u8, upvalue), false);
            }

            return -1;
        }

        fn add_upvalue(self: *Self, index: u8, is_local: bool) i32 {
            const upvalue_count = self.function.upvalue_count;

            var i: u32 = 0;
            while (i < upvalue_count) {
                const upvalue = &self.upvalues[i];
                if (upvalue.index == index and upvalue.is_local == is_local) {
                    return @intCast(i32, i);
                }
            }

            if (upvalue_count == std.math.maxInt(u8)) {
                self.report_error("Too many closure variables in function.");
                return 0;
            }

            self.upvalues[upvalue_count].is_local = is_local;
            self.upvalues[upvalue_count].index = index;
            self.function.upvalue_count += 1;
            return @intCast(i32, upvalue_count);
        }

        fn identifier_constant(self: *Self, name: *Token) Allocator.Error!u8 {
            return try self.make_constant(Value.obj((try self.gc.copy_string(name.content, name.len)).widen()));
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
            const obj_string = try self.gc.copy_string(self.parser.previous.content + 1, self.parser.previous.len - 2);
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
                arg = self.resolve_upvalue(name);
                if (arg != -1) {
                    get_op = .GetUpvalue;
                    set_op = .SetUpvalue;
                } else {
                    arg = try self.identifier_constant(name);
                    get_op = .GetGlobal;
                    set_op = .SetGlobal;
                }
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
