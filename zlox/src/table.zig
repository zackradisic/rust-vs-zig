const std = @import("std");
const Allocator = @import("std/mem").Allocator;
const Obj = @import("obj.zig");
const Value = @import("value.zig").Value;
const GC = @import("gc.zig");

const Table = @This();

count: u32 = 0,
cap: u32 = 0,
entries: ?[*]Entry = null,

const MAX_LOAD = 0.75;

pub const Entry = struct { key: ?*Obj.String, val: Value };

pub fn init() Table {
    return .{};
}

pub fn free(self: *Table, gc: *GC) void {
    if (self.entries) |entries| {
        gc.free(entries[0..self.cap]);
    }
    self.count = 0;
    self.cap = 0;
    self.entries = null;
}

pub fn hash_string(key: [*]const u8, len: u32) u32 {
    var hash: u32 = 2166136261;
    for (key[0..len]) |c| {
        hash ^= c;
        hash *= 16777619;
    }
    return hash;
}

pub fn get(self: *Table, key: *Obj.String) ?Value {
    if (self.count == 0) return null;

    const entry = self.find_entry(key) orelse return null;
    if (entry.key == null) return null;

    return entry.val;
}

/// Returns true if the key is new.
pub fn insert(self: *Table, gc: *GC, key: *Obj.String, val: Value) !bool {
    if (@intToFloat(f64, self.count + 1) > @intToFloat(f64, self.cap) * MAX_LOAD) {
        const new_cap = if (self.cap < 8) 8 else self.cap * 2;
        try self.adjust_capacity(gc, new_cap);
    }

    // Above if branch will always set entries.
    const entry = self.find_entry(key) orelse unreachable;
    const is_new_key = entry.key == null;
    if (is_new_key and entry.val.as_nil() != null) {
        self.count += 1;
    }

    entry.key = key;
    entry.val = val;
    return is_new_key;
}

pub fn add_all(self: *const Table, gc: *GC, to: *Table) !void {
    const entries = self.entries_slice() orelse return;
    for (entries) |entry| {
        if (entry.key == null) continue;
        try to.insert(gc, entry.key, entry.val);
    }
}

pub fn delete(self: *Table, key: *Obj.String) bool {
    if (self.count == 0) return false;
    var entry = self.find_entry(key) orelse return false;
    if (entry.key == null) return false;

    entry.key = null;
    entry.val = Value.boolean(true);
    return true;
}

pub fn find_string(self: *Table, chars: [*]const u8, len: u32, hash: u32) ?*Obj.String {
    if (self.count == 0) return null;
    const entries = self.entries_slice() orelse return null;
    var idx = hash % self.cap;

    var entry: *Entry = undefined;
    while (true) {
        entry = &entries[idx];
        if (entry.key) |key| {
            if (key.len == len and key.hash == hash and std.mem.eql(u8, key.chars[0..key.len], chars[0..len])) return key;
        } else if (entry.val.as_nil() != null) {
            return null;
        }

        idx = (idx + 1) % self.cap;
    }
}

fn find_entry(self: *Table, key: *Obj.String) ?*Entry {
    var entries = self.entries_slice() orelse return null;
    return find_entry_impl(entries, key);
}

fn find_entry_impl(entries: []Entry, key: *Obj.String) *Entry {
    const cap = entries.len;
    var idx = key.hash % cap;
    var tombstone: ?*Entry = null;
    while (true) {
        const entry = &entries[idx];
        if (entry.key == null) {
            if (entry.val.as_nil() != null) {
                if (tombstone) |ts| {
                    return ts;
                }
                return entry;
            }

            if (tombstone == null) {
                tombstone = entry;
            }
        } else if (entry.key == key) {
            return entry;
        }

        idx = (idx + 1) % cap;
    }
}

pub fn adjust_capacity(self: *Table, gc: *GC, new_cap: usize) !void {
    var new_entries = try gc.alloc(Entry, new_cap);
    for (new_entries) |*entry| {
        entry.key = null;
        entry.val = Value.nil();
    }

    var count: u32 = 0;
    if (self.entries_slice()) |entries| {
        for (entries) |entry| {
            const key = entry.key orelse continue;

            var dest = find_entry_impl(new_entries, key);
            dest.key = key;
            dest.val = entry.val;
            count += 1;
        }
        gc.free(entries);
    }

    self.entries = @ptrCast([*]Entry, new_entries);
    self.cap = @intCast(u32, new_cap);
    self.count = count;
}

pub fn entries_slice(self: *Table) ?[]Entry {
    if (self.entries) |entries| {
        return entries[0..self.cap];
    }

    return null;
}
