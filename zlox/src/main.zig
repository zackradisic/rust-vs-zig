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
    // var scan = Scanner.init(source);
    // scan.scan();
    const vm = interpret(alloc, source) catch |err| {
        try std.testing.expect(InterpretError.CompileError == err);
        return;
    };
    _ = vm;
}
