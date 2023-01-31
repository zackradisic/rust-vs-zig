const std = @import("std");
const Obj = @import("obj.zig");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;

const GC = @This();

allocator: Allocator,
obj_list: ?*Obj,

pub fn init(allocator: Allocator) GC {
    return .{
        .allocator = allocator,
        .obj_list = null,
    };
}

pub fn free_objects(self: *GC) !void {
    var obj = self.obj_list;
    while (obj) |next| {
        self.free_object(next) catch {};
        obj = next.next;
    }
    self.obj_list = null;
}

pub fn free_object(self: *GC, obj: *Obj) !void {
    switch (obj.type) {
        .String => {
            const string = obj.narrow(Obj.String);
            self.allocator.destroy(string.chars);
            self.allocator.destroy(string);
        },
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

pub fn alloc(self: *GC, comptime ParentType: type) !*ParentType {
    comptime validate_obj_pun_type(ParentType);
    const ptr = try self.allocator.create(ParentType);
    ptr.widen().next = self.obj_list;
    self.obj_list = ptr.widen();
    return ptr;
}

pub fn alloc_string(self: *GC, chars: [*]const u8, len: u32) !*Obj.String {
    var ptr = try self.alloc(Obj.String);
    ptr.* = Obj.String{
        .obj = Obj{ .type = Obj.Type.String },
        .len = len,
        .chars = chars,
    };
    return ptr;
}

pub fn take_string(self: *GC, chars: [*]const u8, len: u32) !*Obj.String {
    return self.alloc_string(chars, len);
}

pub fn copy(self: *GC, chars: [*]const u8, len: u32) !*Obj.String {
    const alloced_chars = try self.allocator.alloc(u8, len); 
    mem.copy(u8, alloced_chars, chars[0..len]);
    return self.alloc_string(@ptrCast([*]const u8, alloced_chars), len);
}