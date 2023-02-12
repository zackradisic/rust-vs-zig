const std = @import("std");
const debug = std.debug;
const Conf = @import("conf.zig");
const Obj = @import("obj.zig");
const Table = @import("table.zig");
const Value = @import("value.zig").Value;
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;

const GC = @This();

allocator: Allocator,
obj_list: ?*Obj,
interned_strings: Table,
globals: Table,
gray_stack: ArrayList(*Obj),
/// For right now this only tracks Obj allocations, not other memory for arrays, etc. This is because we can easily track
/// Obj allocations because they are allocated thru the `GC.alloc_obj()` method which we can augment to increment `bytes_allocated`. 
/// Some other types of memory (for example for std.ArrayList) use the allocator directly so we can't track how much memory they alloc/dealloc.
/// To support this we would have to make a custom allocator that wraps the allocator and tracks the memory allocations.
bytes_allocated: usize = 0,
next_gc: usize = 1024 * 1024,

pub fn init(allocator: Allocator) !GC {
    return .{
        .allocator = allocator,
        .obj_list = null,
        .interned_strings = Table.init(),
        .globals = Table.init(),
        .gray_stack = try ArrayList(*Obj).initCapacity(allocator, 64),
    };
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
    try self.gray_stack.append(self.allocator, obj);
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

    self.interned_strings.free(self);
    self.globals.free(self);
    self.gray_stack.clearAndFree(self.allocator);
}

pub fn free_object(self: *GC, obj: *Obj) !void {
    switch (obj.type) {
        .String => {
            const string = obj.narrow(Obj.String);
            self.allocator.destroy(string.chars);
            self.free_obj(string);
        },
        .Function => {
            const function = obj.narrow(Obj.Function);
            function.chunk.free(self.allocator);
            self.free_obj(function);
        },
        .NativeFunction => {
            self.free_obj(obj.narrow(Obj.NativeFunction));
        },
        .Closure => {
            const closure = obj.narrow(Obj.Closure);
            self.allocator.free(closure.upvalues[0..closure.upvalues_len]);
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

pub fn alloc(self: *GC, comptime T: type, n: usize) ![]T {
    // self.bytes_allocated += @sizeOf(T) * n;
    return self.allocator.alloc(T, n);
}

pub fn create(self: *GC, comptime T: type) !*T {
    // self.bytes_allocated += @sizeOf(T);
    return self.allocator.create(T);
}

/// Free an array allocated with `alloc`. To free a single item,
/// see `destroy`.
pub fn free(self: *GC, memory: anytype) void {
    // self.bytes_allocated -= get_array_byte_size(memory);
    self.allocator.free(memory);
}

pub fn destroy(self: *GC, ptr: anytype) void {
    // self.bytes_allocated -= @sizeOf(ptr);
    self.allocator.destroy(ptr);
}

pub fn alloc_obj(self: *GC, comptime ParentType: type) !*ParentType {
    if (Conf.DEBUG_STRESS_GC) {
    }

    comptime validate_obj_pun_type(ParentType);
    const ptr = try self.create(ParentType);

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
    const bytes = comptime blk: {
        const Ptr = @typeInfo(@TypeOf(ptr));
        if (@as(std.builtin.TypeId, Ptr) != std.builtin.TypeId.Pointer) {
            @compileError("expected pointer");
        }
        if (Obj.Type.from_obj_safe(Ptr.Pointer.child) == null) {
            @compileError("expected pointer to a narrow Obj type but got " ++ @typeName(Ptr.Pointer.child));
        }
        break :blk @sizeOf(Ptr.Pointer.child);
    };
    self.bytes_allocated -= bytes;
    self.destroy(ptr);
}

pub fn alloc_string(self: *GC, chars: [*]const u8, len: u32, hash: u32) !*Obj.String {
    var ptr = try self.alloc_obj(Obj.String);
    ptr.len = len;
    ptr.chars = chars;
    ptr.hash = hash;
    _ = try self.interned_strings.insert(self, ptr, Value.nil());
    return ptr;
}

pub fn take_string(self: *GC, chars: [*]const u8, len: u32) !*Obj.String {
    const hash = Table.hash_string(chars, len);
    if (self.interned_strings.find_string(chars, len, hash)) |interned| {
        self.free(chars[0..len]);
        return interned;
    }
    return self.alloc_string(chars, len, hash);
}

pub fn copy_string(self: *GC, chars: [*]const u8, len: u32) !*Obj.String {
    const hash = Table.hash_string(chars, len);
    if (self.interned_strings.find_string(chars, len, hash)) |interned| {
        return interned;
    }
    const alloced_chars = try self.allocator.alloc(u8, len);
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