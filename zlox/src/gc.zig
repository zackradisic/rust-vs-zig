const std = @import("std");
const debug = std.debug;
const Conf = @import("conf.zig");
const Obj = @import("obj.zig");
const Table = @import("table.zig");
const Value = @import("value.zig").Value;
const CallFrameStack = @import("vm.zig").CallFrameStack;
const ValueStack = @import("vm.zig").ValueStack;
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;

const GC = @This();

/// The allocator used by the GC, this should only be used to implement the std.mem.Allocator interface.
inner_allocator: Allocator,
/// The GC represented as an allocator. This is so the GC can be used as an allocator for std library for example ArrayList. This
/// should mostly be used.
this_allocator: Allocator,

obj_list: ?*Obj,
open_upvalues: ?*Obj.Upvalue = null,
interned_strings: Table,
globals: Table,
gray_stack: ArrayList(*Obj),
call_frames: ?*CallFrameStack = null,
stack: ?*ValueStack = null,
bytes_allocated: usize = 0,
next_gc: usize = 1024 * 1024,

pub fn init(gc: *GC, allocator: Allocator) !void {
    gc.inner_allocator = allocator;
    gc.this_allocator = undefined;
    gc.obj_list = null;
    gc.open_upvalues = null;
    gc.call_frames = null;
    gc.stack = null;
    gc.interned_strings = Table.init();
    gc.globals = Table.init();
    gc.gray_stack = try ArrayList(*Obj).initCapacity(allocator, 64);

    gc.this_allocator = Allocator.init(gc, alloc, resize, free);
}

pub inline fn as_allocator(self: *GC) Allocator {
    return self.this_allocator;
}

fn alloc(self: *GC, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) ![]u8 {
    const bytes = try self.inner_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);
    self.bytes_allocated += bytes.len;
    try self.maybe_collect();
    return bytes;
}

fn resize(self: *GC, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {
    const calculated_new_len = self.inner_allocator.rawResize(buf, buf_align, new_len, len_align, ret_addr) orelse return null;
    self.bytes_allocated += calculated_new_len - buf.len;
    if (calculated_new_len > buf.len) {
        self.maybe_collect() catch @panic("GC failed to collect");
    }
    return calculated_new_len;
}

fn free(self: *GC, buf: []u8, buf_align: u29, ret_addr: usize) void {
    self.inner_allocator.rawFree(buf, buf_align, ret_addr);
    self.bytes_allocated -= buf.len;
}

fn maybe_collect(self: *GC) !void {
    if (comptime Conf.DEBUG_STRESS_GC and (self.bytes_allocated > self.next_gc)) {
        try self.collect();
    }
}

pub fn collect(self: *GC) !void {
    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("-- gc begin\n", .{});
    }

    try self.mark_roots();
    try self.trace_references();
    self.interned_strings.remove_white();
    try self.sweep();

    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("-- gc end\n", .{});
    }
}

pub fn mark_roots(self: *GC) !void {
    if (self.stack) |values| {
        var slot = @ptrCast([*]Value, &values.stack[0]);
        while (@ptrToInt(slot) < @ptrToInt(values.top)): (slot += 1) {
            try self.mark_value(slot[0]);
        }
    }

    if (self.call_frames) |call_frames| {
        var i: usize = 0;
        while (i < call_frames.count): (i += 1) {
            try self.mark_obj(call_frames.stack[i].closure.widen());
        }
    }

    var upvalue: ?*Obj.Upvalue = self.open_upvalues;
    while (upvalue) |upval|: (upvalue = upval.next) {
        try self.mark_obj(upval.widen());
    }

    try self.mark_table(&self.globals);
}

pub fn trace_references(self: *GC) !void {
    while (self.gray_stack.items.len > 0) {
        const obj = self.gray_stack.pop();
        try self.blacken_object(obj);
    }
}

pub fn sweep(self: *GC) !void {
    var previous: ?*Obj = null;
    var object: ?*Obj = self.obj_list;

    while (object) |obj| {
        if (obj.is_marked) {
            obj.is_marked = false;
            previous = obj;
            object = obj.next;
        } else {
            if (previous) |prev| {
                prev.next = obj.next;
            } else {
                self.obj_list = obj.next;
            }

            try self.free_object(obj);
        }
    }
}

pub fn mark_table(self: *GC, table: *Table) !void {
    var i: usize = 0;
    while (i < table.cap): (i += 1) {
        var entry: *Table.Entry = &table.entries.?[i];
        if (entry.key) |key| {
            try self.mark_obj(key.widen());
        }
        try self.mark_value(entry.val);
    }

}

pub fn mark_value(self: *GC, value: Value) !void {
    switch (value) {
        .Obj => |obj| try self.mark_obj(obj),
        else => {},
    }
}

pub fn mark_obj(self: *GC, obj: *Obj) !void {
    if (obj.is_marked) return;

    if (comptime Conf.DEBUG_LOG_GC) {
        std.debug.print("{d} mark ", .{@ptrToInt(self)});
        Value.obj(obj).print(std.debug);
        std.debug.print("\n", .{});
    }

    obj.is_marked = true;
    try self.gray_stack.append(self.as_allocator(), obj);
}

pub fn mark_array(self: *GC, array: []Value) !void {
    for (array) |value| {
        try self.mark_value(value);
    }
}

pub fn blacken_object(self: *GC, obj: *Obj) !void {
    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("{d} blacken ", .{@ptrToInt(self)});
        Value.obj(obj).print(debug);
        debug.print("\n", .{});
    }

    switch (obj.type) {
        .NativeFunction, .String => {},
        .Upvalue => {
            const upvalue: *Obj.Upvalue = obj.narrow(Obj.Upvalue);
            try self.mark_value(upvalue.closed);
        },
        .Function => {
            const function: *Obj.Function = obj.narrow(Obj.Function);
            if (function.name) |name| {
                try self.mark_obj(name.widen());
            }

            try self.mark_array(function.chunk.constants.items.ptr[0..function.chunk.constants.items.len]);
        },
        .Closure => {
            const closure: *Obj.Closure = obj.narrow(Obj.Closure);
            try self.mark_obj(closure.function.widen());
            var i: usize = 0;
            while (i < closure.upvalues_len): (i += 1) {
                try self.mark_obj(closure.upvalues[i].widen());
            }
        },
    }
}

pub fn free_objects(self: *GC) !void {
    var obj = self.obj_list;
    while (obj) |next| {
        obj = next.next;
        self.free_object(next) catch {};
    }
    self.obj_list = null;

    self.interned_strings.free(self.as_allocator());
    self.globals.free(self.as_allocator());
    self.gray_stack.clearAndFree(self.as_allocator());
}

pub fn free_object(self: *GC, obj: *Obj) !void {
    switch (obj.type) {
        .String => {
            const string = obj.narrow(Obj.String);
            self.as_allocator().destroy(string.chars);
            self.free_obj(string);
        },
        .Function => {
            const function = obj.narrow(Obj.Function);
            function.chunk.free(self.as_allocator());
            self.free_obj(function);
        },
        .NativeFunction => {
            self.free_obj(obj.narrow(Obj.NativeFunction));
        },
        .Closure => {
            const closure = obj.narrow(Obj.Closure);
            self.as_allocator().free(closure.upvalues[0..closure.upvalues_len]);
            self.free_obj(closure);
        },
        .Upvalue => {
            self.free_obj(obj.narrow(Obj.Upvalue));
        }
    }
}

fn validate_obj_pun_type(comptime ParentType: type) void {
    const ty_info = @typeInfo(ParentType).Struct;
    const obj_type = ty_info.fields[0];
    if (!mem.eql(u8, obj_type.name, "obj")) {
        @compileError("expected first field to be named 'obj'");
    }
    if (obj_type.field_type != Obj) {
        @compileError("expected first field to be of type 'Obj'");
    }
    var found_widen = false;
    for (ty_info.decls) |decl| {
        if (mem.eql(u8, decl.name, "widen")) {
            found_widen = true;
        }
    }
    if (!found_widen) {
        @compileError("expected punnable obj type to have a 'widen' method");
    }
}

pub fn alloc_obj(self: *GC, comptime ParentType: type) !*ParentType {
    comptime validate_obj_pun_type(ParentType);
    const ptr = try self.this_allocator.create(ParentType);

    ptr.widen().type = Obj.Type.from_obj(ParentType);
    ptr.widen().is_marked = false;
    ptr.widen().next = self.obj_list;
    self.obj_list = ptr.widen();

    if (comptime Conf.DEBUG_LOG_GC) {
        debug.print("{d} allocate {d} for {s}\n", .{ @ptrToInt(ptr), @sizeOf(ParentType), @tagName(Obj.Type.from_obj(ParentType))});
    }

    return ptr;
}

pub fn free_obj(self: *GC, ptr: anytype) void {
    // const bytes = comptime blk: {
    //     const Ptr = @typeInfo(@TypeOf(ptr));
    //     if (@as(std.builtin.TypeId, Ptr) != std.builtin.TypeId.Pointer) {
    //         @compileError("expected pointer");
    //     }
    //     if (Obj.Type.from_obj_safe(Ptr.Pointer.child) == null) {
    //         @compileError("expected pointer to a narrow Obj type but got " ++ @typeName(Ptr.Pointer.child));
    //     }
    //     break :blk @sizeOf(Ptr.Pointer.child);
    // };
    // self.bytes_allocated -= bytes;
    self.as_allocator().destroy(ptr);
}

pub fn alloc_string(self: *GC, chars: [*]const u8, len: u32, hash: u32) !*Obj.String {
    var ptr = try self.alloc_obj(Obj.String);
    ptr.len = len;
    ptr.chars = chars;
    ptr.hash = hash;
    _ = try self.interned_strings.insert(self.as_allocator(), ptr, Value.nil());
    return ptr;
}

pub fn take_string(self: *GC, chars: [*]const u8, len: u32) !*Obj.String {
    const hash = Table.hash_string(chars, len);
    if (self.interned_strings.find_string(chars, len, hash)) |interned| {
        self.as_allocator().free(chars[0..len]);
        return interned;
    }
    return self.alloc_string(chars, len, hash);
}

pub fn copy_string(self: *GC, chars: [*]const u8, len: u32) !*Obj.String {
    const hash = Table.hash_string(chars, len);
    if (self.interned_strings.find_string(chars, len, hash)) |interned| {
        return interned;
    }
    const alloced_chars = try self.as_allocator().alloc(u8, len);
    mem.copy(u8, alloced_chars, chars[0..len]);
    return self.alloc_string(@ptrCast([*]const u8, alloced_chars), len, hash);
}

pub inline fn get_array_byte_size(array: anytype) usize {
    const bytes = comptime blk: {
        const ty_info = @typeInfo(@TypeOf(array));
        if (@as(std.builtin.TypeId, ty_info) != std.builtin.TypeId.Pointer) {
            @compileError("expected pointer to slice");
        }
        if (ty_info.Pointer.size != std.builtin.Type.Pointer.Size.Slice) {
            @compileError("expected pointer to slice");
        }
        const ElemTy = ty_info.Pointer.child;
        break :blk @sizeOf(ElemTy) * array.len;
    };
    return bytes;
}