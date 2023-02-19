const Value = @import("value.zig").Value;
const std = @import("std");
const Obj = @import("obj.zig");
const mem = std.mem;
const debug = std.debug;
const Allocator = mem.Allocator;

const ArrayList = std.ArrayListUnmanaged;

pub const Opcode = enum(u8) { Return, Constant, Negate, Add, Subtract, Multiply, Divide, Nil, True, False, Not, Equal, Greater, Less, Print, Pop, DefineGlobal, GetGlobal, SetGlobal, GetLocal, SetLocal, JumpIfFalse, Jump, Loop, Call, Closure, GetUpvalue, SetUpvalue, CloseUpvalue, Class, GetProperty, SetProperty, Method, Invoke };

pub const Chunk = struct {
    const Self = @This();

    code: ArrayList(u8),
    constants: ArrayList(Value),
    lines: ArrayList(u16),

    pub fn init(allocator: Allocator) Allocator.Error!Self {
        return Chunk{
            .code = try ArrayList(u8).initCapacity(allocator, 64),
            .constants = try ArrayList(Value).initCapacity(allocator, 64),
            .lines = try ArrayList(u16).initCapacity(allocator, 4),
        };
    }

    pub fn write_op(self: *Chunk, allocator: Allocator, op: Opcode, line: u16) !void {
        try self.write_byte(allocator, @enumToInt(op), line);
    }

    pub fn write_byte(self: *Chunk, allocator: Allocator, byte: u8, line: u16) !void {
        try self.code.append(allocator, byte);
        try self.lines.append(allocator, line);
    }

    pub fn write_u16(self: *Chunk, allocator: Allocator, val: u16, line: u16) !void {
        try self.write_byte(allocator, @intCast(u8, val >> 8), line);
        try self.write_byte(allocator, @intCast(u8, val), line);
    }

    pub fn free(self: *Chunk, allocator: Allocator) void {
        self.code.clearAndFree(allocator);
        self.constants.clearAndFree(allocator);
        self.lines.clearAndFree(allocator);
    }

    pub fn add_constant(self: *Chunk, allocator: Allocator, value: Value) !u8 {
        const index = self.constants.items.len;
        try self.constants.append(allocator, value);
        return @intCast(u8, index);
    }

    pub fn disassemble(self: *const Chunk, name: []const u8) void {
        debug.print("== {s} ==\n", .{name});
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassemble_instruction(offset);
        }
        debug.print("== end {s} ==\n", .{name});
    }

    pub fn disassemble_instruction(self: *const Chunk, offset: usize) usize {
        debug.print("{d:04} ", .{offset});
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            debug.print("   | ", .{});
        } else {
            debug.print("{d:4} ", .{self.lines.items[offset]});
        }

        const instruction: Opcode = @intToEnum(Opcode, self.code.items[offset]);
        switch (instruction) {
            .Equal => {
                return simple_instruction("OP_EQUAL", offset);
            },
            .Greater => {
                return simple_instruction("OP_GREATER", offset);
            },
            .Less => {
                return simple_instruction("OP_LESS", offset);
            },
            .Not => {
                return simple_instruction("OP_NOT", offset);
            },
            .Nil => {
                return simple_instruction("OP_NIL", offset);
            },
            .True => {
                return simple_instruction("OP_TRUE", offset);
            },
            .False => {
                return simple_instruction("OP_FALSE", offset);
            },
            .Add => {
                return simple_instruction("ADD", offset);
            },
            .Subtract => {
                return simple_instruction("SUBTRACT", offset);
            },
            .Multiply => {
                return simple_instruction("MULTIPLY", offset);
            },
            .Divide => {
                return simple_instruction("DIVIDE", offset);
            },
            .Negate => {
                return simple_instruction("OP_NEGATE", offset);
            },
            .Return => {
                return simple_instruction("OP_RETURN", offset);
            },
            .Constant => {
                return self.constant_instruction("OP_CONSTANT", offset);
            },
            .Print => {
                return simple_instruction("OP_PRINT", offset);
            },
            .Pop => {
                return simple_instruction("OP_POP", offset);
            },
            .DefineGlobal => {
                return self.constant_instruction("OP_DEFINE_GLOBAL", offset);
            },
            .GetGlobal => {
                return self.constant_instruction("OP_GET_GLOBAL", offset);
            },
            .SetGlobal => {
                return self.constant_instruction("OP_SET_GLOBAL", offset);
            },
            .GetLocal => {
                return self.byte_instruction("OP_GET_LOCAL", offset);
            },
            .SetLocal => {
                return self.byte_instruction("OP_SET_LOCAL", offset);
            },
            .Jump => {
                return self.jump_instruction("OP_JUMP", 1, offset);
            },
            .JumpIfFalse => {
                return self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset);
            },
            .Loop => {
                return self.jump_instruction("OP_LOOP", -1, offset);
            },
            .Call => {
                return self.byte_instruction("OP_CALL", offset);
            },
            .Closure => {
                var ret = offset + 1;
                const constant = self.code.items.ptr[ret];
                ret += 1;
                self.constants.items[constant].print(debug);

                const function: *Obj.Function = self.constants.items[constant].as_obj().?.narrow(Obj.Function);
                var j: usize = 0;
                while (j < function.upvalue_count): (j += 1) {
                    const is_local = self.code.items[ret];
                    ret += 1;
                    const index = self.code.items[ret];
                    ret += 1;
                    debug.print("{d:04}    |   {s} {d}\n", .{ret - 2, if (is_local == 1) "local" else "upvalue", index});
                }

                return ret;
            },
            .GetUpvalue => {
                return self.byte_instruction("OP_GET_UPVALUE", offset);
            },
            .SetUpvalue => {
                return self.byte_instruction("OP_SET_UPVALUE", offset);
            },
            .CloseUpvalue => {
                return simple_instruction("OP_CLOSE_UPVALUE", offset);
            },
            .Class => {
                return simple_instruction("OP_CLASS", offset);
            },
            .GetProperty => {
                return self.constant_instruction("OP_GET_PROPERTY", offset);
            },
            .SetProperty => {
                return self.constant_instruction("OP_SET_PROPERTY", offset);
            },
            .Method => {
                return self.constant_instruction("OP_METHOD", offset);
            },
            .Invoke => {
                return self.invoke_instruction("OP_INVOKE", offset);
            }
        }

        return 0;
    }

    fn simple_instruction(name: [:0]const u8, offset: usize) usize {
        debug.print("{s}\n", .{name});
        return offset + 1;
    }

    fn constant_instruction(self: *const Chunk, name: [:0]const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        debug.print("{s} {d} ", .{ name, constant });
        self.constants.items[constant].print(debug);
        debug.print("\n", .{});
        return offset + 2;
    }

    fn byte_instruction(self: *const Chunk, name: [:0]const u8, offset: usize) usize {
        const slot = self.code.items[offset + 1];
        debug.print("{s} {d}\n", .{ name, slot });
        return offset + 2;
    }

    fn jump_instruction(self: *const Chunk, name: [:0]const u8, sign: i32, offset: usize) usize {
        const jump: u16 = (@intCast(u16, self.code.items[offset + 1]) << 8) | self.code.items[offset + 2];
        debug.print("{s} {d} -> {d}\n", .{ name, offset, @intCast(i64, offset) + 3 + sign * jump });
        return offset + 3;
    }


    fn invoke_instruction(self: *const Chunk, name: [:0]const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        const arg_count = self.code.items[offset + 2];
        debug.print("{s} ({d} args) {d} '", .{ name, arg_count, constant });
        debug.print("'\n", .{});
        return offset + 3;
    }
};
