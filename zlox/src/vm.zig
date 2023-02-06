const std = @import("std");
const debug = std.debug;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const Opcode = _chunk.Opcode;
const Value = @import("value.zig").Value;
const TRACING = @import("common.zig").TRACING;
const Obj = @import("obj.zig");
const GC = @import("gc.zig");
const Table = @import("table.zig");

const Self = @This();
pub var VM: Self = .{
    .chunk = undefined,
    .ip = undefined,
    // .stack = std.mem.zeroes([STACK_MAX]Value),
    .stack = undefined,
    .stack_top = undefined,
    .gc = undefined,
};

pub fn get_vm() *Self {
    return &VM;
}

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

const STACK_MAX = 1024;

chunk: *Chunk,
ip: [*]u8,
stack_top: [*]Value,
gc: GC,
objs: ?*Obj = null,
stack: [STACK_MAX]Value,

pub fn init(self: *Self, gc: GC, chunk: *Chunk) void {
    self.stack_top = self.stack[0..];
    self.chunk = chunk;
    self.ip = @ptrCast([*]u8, chunk.code.items.ptr);
    self.gc = gc;
}

pub fn free(self: *Self) !void {
    self.chunk = undefined;
    try self.gc.free_objects();
}

pub fn run(self: *Self) !void {
    while (true) {
        if (comptime TRACING) blk: {
            // Print stack trace
            const top = @ptrToInt(self.stack_top);
            const stack = @ptrToInt(self.stack[0..]);

            if (top == stack) {
                debug.print("        STACK: []\n", .{});
            } else {
                const idx = (top - stack) / @sizeOf(Value);
                debug.print("        STACK: ", .{});
                debug.print(" [ ", .{});
                for (self.stack[0..idx]) |value| {
                    value.print(debug);
                }
                debug.print("                ]\n", .{});
            }

            // Print the current instruction
            const offset = @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items.ptr);
            if (offset >= self.chunk.code.items.len) {
                break :blk;
            }
            _ = self.chunk.disassemble_instruction(offset);
        }

        const instruction = @intToEnum(Opcode, self.read_byte());

        switch (instruction) {
            .Loop => {
                const offset = self.read_u16();
                self.ip -= offset;
            },
            .Jump => {
                const offset = self.read_u16();
                self.ip += offset;
            },
            .JumpIfFalse => {
                const offset = self.read_u16();
                if (self.peek(0).is_falsey()) {
                    self.ip += offset;
                }
            },
            .SetLocal => {
                const slot = self.read_byte();
                self.stack[slot] = self.peek(0);
            },
            .GetLocal => {
                const slot = self.read_byte();
                self.push(self.stack[slot]);
            },
            .SetGlobal => {
                const name = self.read_string();
                if (try self.gc.globals.insert(&self.gc, name, self.peek(0))) {
                    _ = self.gc.globals.delete(name);
                    self.runtime_error_fmt("redefinition of global variable '{}'", .{name});
                    return InterpretError.RuntimeError;
                }
            },
            .GetGlobal => {
                const name = self.read_string();
                if (self.gc.globals.get(name)) |value| {
                    self.push(value);
                } else {
                    return InterpretError.RuntimeError;
                }
            },
            .DefineGlobal => {
                const name = self.read_string();
                const value = self.peek(0);
                _ = try self.gc.globals.insert(&self.gc, name, value);
                _ = self.pop();
            },
            .Pop => {
                _ = self.pop();
            },
            .Print => {
                self.pop().print(debug);
            },
            .Not => self.push(Value.boolean(self.pop().is_falsey())),
            .Nil => self.push(Value.nil()),
            .True => self.push(Value.boolean(true)),
            .False => self.push(Value.boolean(false)),
            .Equal => {
                const b = self.pop();
                const a = self.pop();

                self.push(Value.boolean(Value.eq(a, b)));
            },
            .Greater => self.binary_op(.Greater),
            .Less => self.binary_op(.Less),
            .Add => {
                if (self.peek(0).as_obj()) |a| {
                    if (self.peek(1).as_obj()) |b| {
                        if (a.is(Obj.String) and b.is(Obj.String)) {
                            try self.concatenate();
                            continue;
                        }
                    }
                }
                self.binary_op(.Add);
            },
            .Subtract => self.binary_op(.Subtract),
            .Multiply => self.binary_op(.Multiply),
            .Divide => self.binary_op(.Divide),
            .Negate => {
                const number = self.peek(0).as_number() orelse {
                    self.runtime_error("Operand must be a number.");
                    return error.RuntimeError;
                };
                self.push(Value.number(-number));
            },
            .Constant => {
                const constant = self.read_constant();
                self.push(constant);
            },
            .Return => {
                return;
            },
        }
    }
}

pub fn concatenate(self: *Self) !void {
    const b = self.pop().Obj.narrow(Obj.String);
    const a = self.pop().Obj.narrow(Obj.String);

    const new_len = a.len + b.len;
    const new_chars = try self.gc.allocator.alloc(u8, new_len);

    mem.copy(u8, new_chars, a.chars[0..a.len]);
    mem.copy(u8, new_chars[a.len..], b.chars[0..b.len]);

    const str = try self.gc.take_string(@ptrCast([*]const u8, new_chars), new_len);
    self.push(Value.obj(str.widen()));
}

pub fn binary_op(self: *Self, comptime op: Opcode) void {
    if (self.peek(0).as_number() == null or self.peek(1).as_number() == null) {
        self.runtime_error("Operands must be numbers.");
        return;
    }

    const b = self.pop().Number;
    const a = self.pop().Number;

    var val: Value =
        switch (op) {
        .Add => Value.number(a + b),
        .Subtract => Value.number(a - b),
        .Multiply => Value.number(a * b),
        .Divide => Value.number(a / b),
        .Less => Value.boolean(a < b),
        .Greater => Value.boolean(a > b),
        else => unreachable,
    };

    self.push(val);
}

pub inline fn peek(self: *Self, distance: usize) Value {
    return (self.stack_top - 1 - distance)[0];
}

pub inline fn push(self: *Self, value: Value) void {
    self.stack_top[0] = value;
    self.stack_top += 1;
}

pub inline fn pop(self: *Self) Value {
    var val = (self.stack_top - 1)[0];
    self.stack_top -= 1;
    return val;
}

pub inline fn read_byte(self: *Self) u8 {
    const byte = self.ip[0];
    self.ip += 1;
    return byte;
}

pub inline fn read_u16(self: *Self) u16 {
    const byte1 = self.read_byte();
    const byte2 = self.read_byte();
    return (@intCast(u16, byte1) << 8) | @intCast(u16, byte2);
}

pub inline fn read_constant(self: *Self) Value {
    const byte = self.read_byte();
    return self.chunk.constants.items[byte];
}

pub inline fn read_string(self: *Self) *Obj.String {
    const byte = self.read_byte();
    return self.chunk.constants.items[byte].as_obj().?.narrow(Obj.String);
}

/// Only used for debugging purposes
pub inline fn get_string(self: *Self, str: []const u8) !*Obj.String {
    return self.gc.copy(@ptrCast([*]const u8, str), @intCast(u32, str.len));
}

pub fn runtime_error(self: *Self, message: []const u8) void {
    debug.print("Runtime error: {s}\n", .{message});
    self.reset_stack();
}

pub fn runtime_error_fmt(self: *Self, comptime fmt: []const u8, args: anytype) void {
    debug.print(fmt, args);
    self.reset_stack();
}

pub fn reset_stack(self: *Self) void {
    self.stack_top = self.stack[0..];
}
