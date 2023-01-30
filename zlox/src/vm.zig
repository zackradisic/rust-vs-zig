const std = @import("std");
const debug = std.debug;
const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const Opcode = _chunk.Opcode;
const Value = @import("value.zig").Value;
const TRACING = @import("common.zig").TRACING;

const Self = @This();
pub var VM: Self = .{
    .chunk = undefined,
    .ip = undefined,
    // .stack = std.mem.zeroes([STACK_MAX]Value),
    .stack = undefined,
    .stack_top = undefined,
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
stack: [STACK_MAX]Value,
stack_top: [*]Value,

pub fn init(self: *Self, chunk: *Chunk) void {
    self.stack_top = self.stack[0..];
    self.chunk = chunk;
    self.ip = @ptrCast([*]u8, chunk.code.items.ptr);
}

pub fn free(self: *Self) void {
    self.chunk = undefined;
}

pub fn run(self: *Self) !void {
    while (true) {
        if (comptime TRACING) {
            // Print stack trace
            const top = @ptrToInt(self.stack_top);
            const stack = @ptrToInt(self.stack[0..]);

            const idx = (top - stack) / @sizeOf(Value);
            for (self.stack[0..idx]) |value| {
                debug.print("[ ", .{});
                value.print(debug);
                debug.print(" ]", .{});
            }

            // Print the current instruction
            _ = self.chunk.disassemble_instruction(@ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items.ptr));
        }

        const instruction = @intToEnum(Opcode, self.read_byte());

        switch (instruction) {
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
            .Add => self.binary_op(.Add),
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
                self.pop().print(debug);
                debug.print("\n", .{});
                return;
            },
        }
    }
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

pub inline fn read_constant(self: *Self) Value {
    const byte = self.read_byte();
    return self.chunk.constants.items[byte];
}

pub fn runtime_error(self: *Self, message: []const u8) void {
    debug.print("Runtime error: {s}\n", .{message});
    self.reset_stack();
}

pub fn reset_stack(self: *Self) void {
    self.stack_top = self.stack[0..];
}
