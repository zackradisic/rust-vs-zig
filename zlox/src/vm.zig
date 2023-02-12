const std = @import("std");
const debug = std.debug;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const Opcode = _chunk.Opcode;
const Value = @import("value.zig").Value;
const Conf = @import("conf.zig");
const Obj = @import("obj.zig");
const GC = @import("gc.zig");
const Table = @import("table.zig");
const native_fns = @import("native_fns.zig");

const CallFrame = struct {
    closure: *Obj.Closure,
    ip: [*]u8,
    slots: [*]Value,

    pub inline fn read_byte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    pub inline fn read_u16(self: *CallFrame) u16 {
        const byte1 = self.read_byte();
        const byte2 = self.read_byte();
        return (@intCast(u16, byte1) << 8) | @intCast(u16, byte2);
    }

    pub inline fn read_constant(self: *CallFrame) Value {
        const byte = self.read_byte();
        return self.closure.function.chunk.constants.items[byte];
    }

    pub inline fn read_string(self: *CallFrame) *Obj.String {
        const byte = self.read_byte();
        return self.closure.function.chunk.constants.items[byte].as_obj().?.narrow(Obj.String);
    }
};

const Self = @This();
pub var VM: Self = .{
    .call_frames = undefined,
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

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * std.math.maxInt(u8);

call_frames: [FRAMES_MAX]CallFrame,
call_frame_count: u32 = 0,
gc: GC,
objs: ?*Obj = null,
openup_values: ?*Obj.Upvalue = null,
stack_top: [*]Value,
stack: [STACK_MAX]Value,

pub fn init(self: *Self, gc: GC, closure: *Obj.Closure) !void {
    self.stack_top = self.stack[0..];
    self.gc = gc;
    try self.define_native("clock", native_fns.clock);
    try self.define_native("__dummy", native_fns.__dummy);

    self.stack[0] = Value.obj(closure.widen());
    self.call_frames[0] = .{
        .closure = closure,
        .ip = closure.function.chunk.code.items.ptr,
        .slots = self.stack[0..],
    };
    self.stack_top = self.stack[1..];
    debug.assert(@ptrToInt(self.call_frames[0].slots) == @ptrToInt(self.stack_top) - @sizeOf(Value));
    self.call_frame_count = 1;

}

pub fn collect(self: *Self) !void {
    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("-- gc begin\n", .{});
    }

    try self.mark_roots();
    try self.trace_references();
    self.gc.interned_strings.remove_white();
    try self.sweep();

    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("-- gc end\n", .{});
    }
}

pub fn mark_roots(self: *Self) !void {
    var slot = @ptrCast([*]Value, &self.stack[0]);
    while (@ptrToInt(slot) < @ptrToInt(self.stack_top)): (slot += 1) {
        try self.gc.mark_value(slot[0]);
    }

    var i: usize = 0;
    while (i < self.call_frame_count): (i += 1) {
        try self.gc.mark_obj(self.call_frames[i].closure.widen());
    }

    var upvalue: ?*Obj.Upvalue = self.openup_values;
    while (upvalue) |upval|: (upvalue = upval.next) {
        try self.gc.mark_obj(upval.widen());
    }

    try self.gc.mark_table(&self.gc.globals);
}

pub fn trace_references(self: *Self) !void {
    while (self.gc.gray_stack.items.len > 0) {
        const obj = self.gc.gray_stack.pop();
        try self.gc.blacken_object(obj);
    }
}

pub fn sweep(self: *Self) !void {
    var previous: ?*Obj = null;
    var object: ?*Obj = self.objs;

    while (object) |obj| {
        if (obj.is_marked) {
            obj.is_marked = false;
            previous = obj;
            object = obj.next;
        } else {
            if (previous) |prev| {
                prev.next = obj.next;
            } else {
                self.objs = obj.next;
            }

            try self.gc.free_object(obj);
        }
    }
}

pub fn free(self: *Self) !void {
    try self.gc.free_objects();
}

pub fn run(self: *Self) !void {
    var frame: *CallFrame = &self.call_frames[self.call_frame_count - 1];
    while (true) {
        if (comptime Conf.TRACING) blk: {
            // Print stack trace
            const top: usize = @ptrToInt(self.stack_top);
            const stack: usize = @ptrToInt(self.stack[0..]);

            if (top == stack) {
                debug.print("        STACK: []\n", .{});
            } else {
                var sub: usize = 0;
                _ = @subWithOverflow(usize, top, stack, &sub);
                // debug.print("\nTOP {d} STACK {d} {d} {d}\n", .{top, stack, sub, @sizeOf(Value)});
                const value_size: usize = @sizeOf(Value);
                const idx: usize = (top - stack) / value_size;
                debug.print("        STACK: ", .{});
                debug.print(" [\n", .{});
                for (self.stack[0..idx]) |value| {
                    debug.print("                  ", .{});
                    value.print(debug);
                    debug.print("\n", .{});
                }
                debug.print("                ]\n", .{});
            }

            // Print the current instruction
            const offset = @ptrToInt(frame.ip) - @ptrToInt(frame.closure.function.chunk.code.items.ptr);
            if (offset >= frame.closure.function.chunk.code.items.len) {
                break :blk;
            }
            _ = frame.closure.function.chunk.disassemble_instruction(offset);
        }

        const instruction = @intToEnum(Opcode, frame.read_byte());
        try self.collect();

        switch (instruction) {
            .CloseUpvalue => {
                self.close_upvalues(self.stack_top - 1);
                _ = self.pop();
            },
            .GetUpvalue => {
                const slot = frame.read_byte();
                const val: Value = frame.closure.upvalues[slot].location.*;
                val.print(debug);
                self.push(val);
            },
            .SetUpvalue => {
                const slot = frame.read_byte();
                const val = self.peek(0);
                frame.closure.upvalues[slot].location.* = val;
            },
            .Closure => {
                var function = frame.read_constant().as_obj().?.narrow(Obj.Function);
                var closure = try self.gc.alloc_obj(Obj.Closure);
                try closure.init(&self.gc, function);
                self.push(Value.obj(closure.widen()));
                var i: usize = 0;
                while (i < closure.upvalues_len): (i += 1) {
                    const is_local = frame.read_byte() == 1;
                    const index = frame.read_byte();
                    if (is_local) {
                        closure.upvalues[i] = try self.capture_upvalue(&frame.slots[index]);
                    } else {
                        closure.upvalues[i] = frame.closure.upvalues[index];
                    }

                }
            },
            .Call => {
                const arg_count = frame.read_byte();
                if (!self.call_value(self.peek(arg_count), arg_count)) {
                    return InterpretError.RuntimeError;
                }
                frame = &self.call_frames[self.call_frame_count - 1];
            },
            .Loop => {
                const offset = frame.read_u16();
                frame.ip -= offset;
            },
            .Jump => {
                const offset = frame.read_u16();
                frame.ip += offset;
            },
            .JumpIfFalse => {
                const offset = frame.read_u16();
                if (self.peek(0).is_falsey()) {
                    frame.ip += offset;
                }
            },
            .SetLocal => {
                const slot = frame.read_byte();
                frame.slots[slot] = self.peek(0);
            },
            .GetLocal => {
                const slot = frame.read_byte();
                self.push(frame.slots[slot]);
            },
            .SetGlobal => {
                const name = frame.read_string();
                const val = self.peek(0);
                val.print(debug);
                if (try self.gc.globals.insert(&self.gc, name, val)) {
                    _ = self.gc.globals.delete(name);
                    self.runtime_error_fmt("redefinition of global variable '{}'", .{name});
                    return InterpretError.RuntimeError;
                }
            },
            .GetGlobal => {
                const name = frame.read_string();
                if (self.gc.globals.get(name)) |value| {
                    self.push(value);
                } else {
                    return InterpretError.RuntimeError;
                }
            },
            .DefineGlobal => {
                const name = frame.read_string();
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
                const constant = frame.read_constant();
                self.push(constant);
            },
            .Return => {
                if (self.call_frame_count == 1) {
                    _ = self.pop();
                    return;
                }

                const result = self.pop();
                self.close_upvalues(frame.slots);
                self.call_frame_count -= 1;
                if (self.call_frame_count == 0) {
                    _ = self.pop();
                    return;
                }

                self.stack_top = frame.slots;
                self.push(result);
                frame = &self.call_frames[self.call_frame_count - 1];
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

pub fn capture_upvalue(self: *Self, local: *Value) !*Obj.Upvalue {
    var prev_upvalue: ?*Obj.Upvalue = null;
    var upvalue = self.openup_values;
    
    while (upvalue != null and @ptrToInt(upvalue.?.location) > @ptrToInt(local))  {
        prev_upvalue = upvalue;
        upvalue = upvalue.?.next;
    }

    if (upvalue != null and @ptrToInt(upvalue.?.location) == @ptrToInt(local)) {
        return upvalue.?;
    }

    const created_upvalue = try self.gc.alloc_obj(Obj.Upvalue);
    created_upvalue.init(local);
    created_upvalue.next = upvalue;

    if (prev_upvalue == null) {
        self.openup_values = created_upvalue;
    } else {
        prev_upvalue.?.next = created_upvalue;
    }

    return created_upvalue;
}

pub fn close_upvalues(self: *Self, last: [*]Value) void {
   while (self.openup_values) |open_upvalues| {
    if (!(@ptrToInt(open_upvalues) >= @ptrToInt(last))) {
        break;
    }

    var upvalue = open_upvalues;
    upvalue.closed = upvalue.location.*;
    upvalue.location = &upvalue.closed;
    self.openup_values = upvalue.next;
   }
}

pub fn call_value(self: *Self, callee: Value, arg_count: u8) bool {
    if (callee.as_obj()) |obj| {
        switch (obj.type) {
            .Closure => {
                const closure = obj.narrow(Obj.Closure);
                return self.call(closure, arg_count);
            },
            .NativeFunction => {
                const native = obj.narrow(Obj.NativeFunction);
                const result = native.function(arg_count, (self.stack_top - arg_count)[0..arg_count]);
                self.stack_top -= arg_count + 1;
                self.push(result);
                return true;
            },
            else => {}, 
        }
    }
    self.runtime_error("Can only call functions and classes.");
    return false;
}

pub fn call(self: *Self, closure: *Obj.Closure, arg_count: u8) bool {
    if (arg_count != closure.function.arity) {
        self.runtime_error_fmt("Expected {d} arguments but got {d}", .{ closure.function.arity, arg_count });
        return false;
    }

    if (self.call_frame_count == FRAMES_MAX) {
        self.runtime_error("Stack overflow.");
        return false;
    }

    var frame = &self.call_frames[self.call_frame_count];
    self.call_frame_count += 1;
    frame.closure = closure;
    frame.ip = closure.function.chunk.code.items.ptr;
    frame.slots = self.stack_top - arg_count - 1;
    return true;
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

/// Only used for debugging purposes
pub inline fn get_string(self: *Self, str: []const u8) !*Obj.String {
    return self.gc.copy_string(@ptrCast([*]const u8, str), @intCast(u32, str.len));
}

pub fn runtime_error(self: *Self, message: []const u8) void {
    {
        const frame = &self.call_frames[self.call_frame_count - 1];
        const instr = @ptrToInt(frame.ip) - @ptrToInt(frame.closure.function.chunk.code.items.ptr) - 1;
        debug.print("[line {d}] Runtime error: {s}\n", .{ frame.closure.function.chunk.lines.items[instr], message });
    }

    var i = self.call_frame_count - 1;
    while (i >= 0) : (i -= 1) {
        const frame = &self.call_frames[i];
        const function = frame.closure.function;
        const instr = @ptrToInt(frame.ip) - @ptrToInt(frame.closure.function.chunk.code.items.ptr) - 1;
        debug.print("[line {d}] in ", .{function.chunk.lines.items[instr]});
        if (function.name) |name| {
            debug.print("{s}()\n", .{name.as_string()});
        } else {
            debug.print("script\n", .{});
        }
    }

    self.reset_stack();
}

pub fn runtime_error_fmt(self: *Self, comptime fmt: []const u8, args: anytype) void {
    debug.print(fmt, args);
    self.reset_stack();
}

/// It is important that these are defined first before anything is put on the stack
pub fn define_native(self: *Self, name: []const u8, function: Obj.NativeFunction.NativeFn) !void {
    const name_str = (try self.gc.copy_string(@ptrCast([*]const u8, name), @intCast(u32, name.len))).widen();
    self.push(Value.obj(name_str));

    var native = try self.gc.alloc_obj(Obj.NativeFunction);
    native.init(function);
    self.push(Value.obj(native.widen()));

    _ = try self.gc.globals.insert(&self.gc, self.stack[0].as_obj().?.narrow(Obj.String), self.stack[1]);
    _ = self.pop();
    _ = self.pop();
}

pub fn reset_stack(self: *Self) void {
    self.stack_top = self.stack[0..];
    self.call_frame_count = 0;
    self.openup_values = null;
}
