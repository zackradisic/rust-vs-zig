const std = @import("std");
const Value = @import("value.zig").Value;

pub fn clock(arg_count: u8, args: []Value) Value {
    _ = arg_count;
    _ = args;
    return Value.number(@intToFloat(f64, std.time.timestamp()));
}

pub fn __dummy(arg_count: u8, args: []Value) Value {
    _ = arg_count;
    _ = args;
    return Value.number(420);
}