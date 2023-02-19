const std = @import("std");
const Conf = @import("conf.zig");
const Chunk = @import("chunk.zig").Chunk;
const GC = @import("gc.zig");
const Value = @import("value.zig").Value;
const Table = @import("table.zig");
const mem = std.mem;
const Allocator = mem.Allocator;

const Obj = @This();

pub const Type = enum {
    String,
    Function,
    NativeFunction,
    Closure,
    Upvalue,
    Class,
    Instance,
    BoundMethod,

    pub fn obj_struct(comptime self: Type) type {
        return switch (self) {
            Type.String => String,
            Type.Function => Function,
            Type.NativeFunction => NativeFunction,
            Type.Closure => Closure,
            Type.Upvalue => Upvalue,
            Type.Class => Class,
            Type.Instance => Instance,
            Type.BoundMethod => BoundMethod,
        };
    }

    pub fn from_obj(comptime ObjType: type) Type {
        return Type.from_obj_safe(ObjType) orelse @panic("invalid obj type");
    }

    pub fn from_obj_safe(comptime ObjType: type) ?Type {
        return switch (ObjType) {
            String => Type.String,
            Function => Type.Function,
            NativeFunction => Type.NativeFunction,
            Closure => Type.Closure,
            Upvalue => Type.Upvalue,
            Class => Type.Class,
            Instance => Type.Instance,
            BoundMethod => Type.BoundMethod,
            else => null,
        };
    }
};

type: Type,
is_marked: bool,
next: ?*Obj = null,

pub fn narrow(self: *Obj, comptime ParentType: type) *ParentType {
    if (comptime Conf.SAFE_OBJ_CAST) {
        return self.safe_narrow(ParentType) orelse @panic("invalid cast");
    }
    return @fieldParentPtr(ParentType, "obj", self);
}

pub fn safe_narrow(self: *Obj, comptime ParentType: type) ?*ParentType {
    if (self.type != Type.from_obj(ParentType)) return null;
    return narrow(self, ParentType);
}

pub fn is(self: *Obj, comptime ParentType: type) bool {
    return self.type == Type.from_obj(ParentType);
}

pub fn print(self: *Obj, writer: anytype) void {
    switch (self.type) {
        inline else => |ty| {
            self.narrow(Type.obj_struct(ty)).print(writer);
        },
    }
}

pub const String = struct {
    obj: Obj,
    len: u32,
    hash: u32,
    chars: [*]const u8,

    pub fn as_string(self: *String) []const u8 {
        return self.chars[0..self.len];
    }

    pub fn eq(a: *String, b: *String) bool {
        if (a.len != b.len) return false;
        return mem.eql(u8, a.chars[0..a.len], b.chars[0..b.len]);
    }

    pub inline fn widen(self: *String) *Obj {
        return @ptrCast(*Obj, self);
    }

    pub fn print(self: *String, writer: anytype) void {
        writer.print("\"{s}\"", .{self.chars[0..self.len]});
    }
};

pub const Function = struct {
    obj: Obj,
    arity: u8,
    upvalue_count: u32,
    name: ?*String,
    chunk: Chunk,

    pub fn init(self: *Function, allocator: Allocator) !void {
        self.arity = 0;
        self.name = null;
        self.upvalue_count = 0;
        self.chunk = try Chunk.init(allocator);
    }

    pub inline fn widen(self: *Function) *Obj {
        return @ptrCast(*Obj, self);
    }

    pub fn print(self: *Function, writer: anytype) void {
        _ = self;
        // TODO: unfuck this
        // const name = if (self.name) |name| name.chars[0..name.len] else return writer.print("<script>", .{});
        // writer.print("<fn {s}>", .{name});
        writer.print("<fn >", .{});
    }

    pub fn name_str(self: *Function) []const u8 {
        return if (self.name) |name| name.chars[0..name.len] else "script";
    }
};

pub const NativeFunction = struct {
    obj: Obj,
    function: NativeFn,

    /// A function pointer
    pub const NativeFn = *const fn (u8, []Value) Value;

    pub fn init(self: *NativeFunction, function: NativeFn) void {
        self.function = function;
    }

    pub fn print(self: *NativeFunction, writer: anytype) void {
        _ = self;
        writer.print("<native fn>", .{});
    }

    pub fn widen(self: *NativeFunction) *Obj {
        return @ptrCast(*Obj, self);
    }
};

pub const Closure = struct {
    obj: Obj,
    function: *Function,
    upvalues: [*]*Upvalue,
    upvalues_len: u32,

    pub fn init(gc: *GC, function: *Function) !*Closure {
        const upvalues = try gc.as_allocator().alloc(?*Upvalue, function.upvalue_count);
        for (upvalues) |*upvalue| {
            upvalue.* = std.mem.zeroes(?*Upvalue);
        }

        var ptr = try gc.alloc_obj(Obj.Closure);
        ptr.function = function;
        // I don't like casting this, but I don't like giving it [*]?*Upvalue type either.
        // It would be nice to have upvalue initialization to be done in this function.
        ptr.upvalues = @ptrCast([*]*Upvalue, upvalues);
        ptr.upvalues_len = function.upvalue_count;

        return ptr;
    }

    pub fn print(self: *Closure, writer: anytype) void {
        const name = self.function.name_str();
        writer.print("<closure> {s}\n", .{name});
    }

    pub fn widen(self: *Closure) *Obj {
        return @ptrCast(*Obj, self);
    }
};

pub const Upvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*Upvalue = null,

    pub fn init(self: *Upvalue, value: *Value) void {
        self.location = value;
        self.closed = Value.nil();
        self.next = null;
    }

    pub fn print(self: *Upvalue, writer: anytype) void {
        _ = self;
        writer.print("upvalue", .{});
    }

    pub fn widen(self: *Upvalue) *Obj {
        return @ptrCast(*Obj, self);
    }
};

pub const Class = struct {
    obj: Obj,
    name: *String,
    methods: Table,

    pub fn init(gc: *GC, name: *String) !*Class {
        var ptr = try gc.alloc_obj(Obj.Class);
        ptr.name = name;
        ptr.methods = Table.init();

        return ptr;
    }

    pub fn print(self: *Class, writer: anytype) void {
        writer.print("class {s}", .{self.name.as_string()});
    }

    pub fn widen(self: *Class) *Obj {
        return @ptrCast(*Obj, self);
    }
};

pub const Instance = struct {
    obj: Obj,
    class: *Class,
    fields: Table,

    pub fn init(gc: *GC, class: *Class) !*Instance {
        var ptr = try gc.alloc_obj(Obj.Instance);
        ptr.class = class;
        ptr.fields = Table.init();

        return ptr;
    }

    pub fn print(self: *Instance, writer: anytype) void {
        writer.print("{s} instance", .{self.class.name.as_string()});
    }

    pub fn widen(self: *Instance) *Obj {
        return @ptrCast(*Obj, self);
    }
};

pub const BoundMethod = struct {
    obj: Obj,
    receiver: Value,
    method: *Closure,

    pub fn init(gc: *GC, receiver: Value, method: *Closure) !*BoundMethod {
        var ptr = try gc.alloc_obj(Obj.BoundMethod);
        ptr.receiver = receiver;
        ptr.method = method;

        return ptr;
    }

    pub fn print(self: *BoundMethod, writer: anytype) void {
        self.method.print(writer);
    }

    pub fn widen(self: *BoundMethod) *Obj {
        return @ptrCast(*Obj, self);
    }
};
