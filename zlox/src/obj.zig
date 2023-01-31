const std = @import("std");
const Conf = @import("common.zig");
const mem = std.mem;
const Allocator = mem.Allocator;

const Obj = @This();

pub const Type = enum {
    String,

    pub fn obj_struct(comptime self: Type) type {
        return switch (self) {
            Type.String => String,
        };
    }
};


type: Type,
next: ?*Obj = null,

pub fn narrow(self: *Obj, comptime ParentType: type) *ParentType {
    if (comptime Conf.SAFE_OBJ_CAST) {
        return self.safe_narrow(ParentType) orelse @panic("invalid cast");
    }
    return @fieldParentPtr(ParentType, "obj", self);
}

pub fn safe_narrow(self: *Obj, comptime ParentType: type) ?*ParentType {
    if (self.type.obj_struct() != ParentType) return null;
    return narrow(self, ParentType);
}

pub fn is(self: *Obj, comptime ParentType: type) bool {
    return self.type.obj_struct() == ParentType;
}

pub const String = struct {
    obj: Obj,
    len: u32,
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
};