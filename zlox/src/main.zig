const std = @import("std");
const Allocator = std.mem.Allocator;
const debug = std.debug;
const io = std.io;

const clap = @import("clap");

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const Opcode = _chunk.Opcode;

const VMType = @import("vm.zig");
const InterpretError = VMType.InterpretError;
var VM = VMType.get_vm();
const CompilerType = @import("compile.zig").Compiler;
const Scanner = @import("scanner.zig");
const GC = @import("gc.zig");
const Value = @import("value.zig").Value;

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = general_purpose_allocator.allocator();

const Compiler = CompilerType(std.fs.File.Writer);
const errw = io.getStdErr().writer();

pub fn main() !void {
    debug.print("Token size: {d}\n", .{@sizeOf(Scanner.Token)});
    // First we specify what parameters our program can take.
    // We can use `parseParamsComptime` to parse a string into an array of `Param(Help)`
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\-r, --repl             Start a REPL.
        \\<FILE>...
        \\
    );

    const parsers = comptime .{
        .FILE = clap.parsers.string,
    };

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
    }) catch |err| {
        diag.report(io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    if (res.args.help) {
        debug.print("--help\n", .{});
        return;
    }
    if (res.args.repl) {
        try repl(
            alloc,
        );
        return;
    }

    const file = res.positionals[0];
    try run_file(alloc, file);
}

pub fn repl(
    allocator: Allocator,
) !void {
    var line = [_]u8{0} ** 1024;

    while (true) {
        debug.print("> ", .{});
        // Read from stdin into the `line` buffer
        const slice = try io.getStdIn().reader().readUntilDelimiterOrEof(&line, '\n') orelse return;

        _ = try interpret(allocator, slice);
    }
}

pub fn run_file(allocator: Allocator, path: []const u8) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    _ = try interpret(allocator, source);
    allocator.free(source);
}

pub fn interpret(allocator: Allocator, source: []const u8) !*VMType {
    var gc = GC.init(allocator);
    var chunk = try Chunk.init(allocator);
    defer chunk.free(allocator);

    var parser = Compiler.init_parser();
    var scanner = Scanner.init(source);
    var compiler = try Compiler.init(&gc, errw, source, &chunk, &scanner, &parser);
    const compile_success = try compiler.compile();
    if (!compile_success) {
        return InterpretError.CompileError;
    }
    debug.assert(chunk.code.items.len == chunk.lines.items.len);

    VM.init(gc, &chunk);
    defer {
        _ = VM.free() catch {};
    }

    try VM.run();
    return VM;
}

pub fn interpret_without_teardown(allocator: Allocator, source: []const u8) !*VMType {
    var gc = GC.init(allocator);
    var chunk = try Chunk.init(allocator);
    defer chunk.free(allocator);

    var parser = Compiler.init_parser();
    var scanner = Scanner.init(source);
    var compiler = try Compiler.init(&gc, errw, source, &chunk, &scanner, &parser);
    const compile_success = try compiler.compile();
    if (!compile_success) {
        return InterpretError.CompileError;
    }
    debug.assert(chunk.code.items.len == chunk.lines.items.len);

    VM.init(gc, &chunk);
    // errdefer {
    //     _ = VM.free() catch {};
    // }

    try VM.run();
    return VM;
}

test "if condition" {
    const source =
        \\var result = "fuck";
        \\if (true) { result = "it worked"; } else { result = "it did not work"; }
    ;
    const vm = try interpret_without_teardown(alloc, source);
    defer {
        _ = vm.free() catch {};
    }
    try vm.run();
}

test "for loop" {
    const source =
        \\var result = 1;
        \\for (var i = 0; i < 3; i = i + 1) {
        \\    result = result * 2;
        \\}
    ;
    const vm = try interpret_without_teardown(alloc, source);
    defer {
        _ = vm.free() catch {};
    }
    const result_str = try vm.get_string("result");
    const i_str = try vm.get_string("i");

    const result = vm.gc.globals.get(result_str) orelse @panic("result not found");
    result.print(debug);
    try std.testing.expect(Value.eq(result, Value.number(8)));

    const i_val = vm.gc.globals.get(i_str) orelse Value.nil();
    try std.testing.expect(Value.eq(i_val, Value.nil()));
}

test "while loop" {
    const source =
        \\var i = 0;
        \\var result = 1;
        \\while (i < 3) {
        \\    result = result * 2;
        \\    i = i + 1;
        \\}
    ;
    const vm = try interpret_without_teardown(alloc, source);
    defer {
        _ = vm.free() catch {};
    }
    const result_str = try vm.get_string("result");
    const i_str = try vm.get_string("i");

    const i_val = vm.gc.globals.get(i_str) orelse @panic("i not found");
    debug.print("i: {}\n", .{i_val});
    try std.testing.expect(Value.eq(i_val, Value.number(3)));

    const result = vm.gc.globals.get(result_str) orelse @panic("result not found");
    debug.print("result: {}\n", .{result});
    try std.testing.expect(Value.eq(result, Value.number(8)));
}

test "using variable in its own initializer fails" {
    const source =
        \\var a = "outer";
        \\{
        \\    var a = a;
        \\}
    ;
    const vm = interpret(alloc, source) catch |err| {
        try std.testing.expect(InterpretError.CompileError == err);
        return;
    };
    _ = vm;
}

test "nested scope variables" {
    const source =
        \\{
        \\  var a = "outer";
        \\  {
        \\      var a = "inner";
        \\  }
        \\}
    ;
    const vm = try interpret(alloc, source);
    _ = vm;
}
