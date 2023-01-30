const std = @import("std");
const io = std.io;

const Type = enum { Nil, Bool, Number };

pub const Value = union(Type) {
    Nil: void,
    Bool: bool,
    Number: f64,

    pub fn print(self: *const Value, writer: anytype) void {
        writer.print("{}\n", .{self});
    }

    pub fn eq(a: Value, b: Value) bool {
        if (@as(Type, a) != @as(Type, b)) return false;

        return switch (@as(Type, a)) {
            .Nil => true,
            .Bool => a.Bool == b.Bool,
            .Number => a.Number == b.Number,
        };
    }

    pub inline fn is_falsey(self: Value) bool {
        return self.as_nil() != null or (self.as_boolean().? == false);
    }

    pub inline fn boolean(val: bool) Value {
        return Value{ .Bool = val };
    }

    pub inline fn nil() Value {
        return Value.Nil;
    }

    pub inline fn number(val: f64) Value {
        return Value{ .Number = val };
    }

    pub fn as_boolean(self: Value) ?bool {
        switch (self) {
            .Bool => |val| return val,
            else => return null,
        }
    }

    pub fn as_nil(self: Value) ?void {
        switch (self) {
            .Nil => return,
            else => return null,
        }
    }

    pub fn as_number(self: Value) ?f64 {
        switch (self) {
            .Number => |val| return val,
            else => return null,
        }
    }
};

pub fn main() !void {
    var nil = Value.boolean(true);
    const noob = nil.foo(Type.Nil);
    std.debug.print("NOOB: {}\n", .{noob});

    const ty = comptime blk: {
        const ty_info = @typeInfo(Value);
        break :blk ty_info.Union.fields[0].name;
    };
    std.debug.print("VALUE TYPE: {s}\n", .{ty});
}

// pub fn main() !void {
//     var ArenaAllocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     const arena = ArenaAllocator.allocator();

//     _ = try arena.alloc(u8, 5000);

//     var it = ArenaAllocator.state.buffer_list.first;
//     var index: u32 = 1;
//     while (it) |node| : (it = node.next) {
//         std.debug.print("NODE LEN: {d} {}\n", .{ &node.data[0], node.data.len });
//         index += 1;
//     }

//     _ = try arena.alloc(u8, 5000);
//     it = ArenaAllocator.state.buffer_list.first;
//     index = 1;
//     while (it) |node| : (it = node.next) {
//         std.debug.print("NODE LEN: {d} {}\n", .{ &node.data[0], node.data.len });
//         index += 1;
//     }

//     std.debug.print("END INDEX: {}\nl", .{ArenaAllocator.state.end_index});
// }
