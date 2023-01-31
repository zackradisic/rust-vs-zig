const std = @import("std");
const Allocator = std.mem.Allocator;
const debug = std.debug;
const io = std.io;

const clap = @import("clap");

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const Opcode = _chunk.Opcode;

var VM = @import("vm.zig").get_vm();
const CompilerType = @import("compile.zig").Compiler;
const Scanner = @import("scanner.zig");
const GC = @import("gc.zig");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = general_purpose_allocator.allocator();

const Compiler = CompilerType(std.fs.File.Writer);
const errw = io.getStdErr().writer();

pub fn main() !void {
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

        try interpret(allocator, slice);
    }
}

pub fn run_file(allocator: Allocator, path: []const u8) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    try interpret(allocator, source);
    allocator.free(source);
}

pub fn interpret(allocator: Allocator, source: []const u8) !void {
    var gc = GC.init(allocator);
    var chunk = try Chunk.init(allocator);
    defer chunk.free(allocator);

    var parser = Compiler.init_parser();
    var compiler = try Compiler.init(&gc, errw, source, &chunk, &parser);
    const compile_success = try compiler.compile();
    if (!compile_success) {
        @panic("Failed to compile.");
    }
    debug.assert(chunk.code.items.len == chunk.lines.items.len);

    VM.init(gc, &chunk);
    defer { 
        _ = VM.free() catch {};
    }

    try VM.run();
}
