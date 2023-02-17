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
const FunctionType = @import("compile.zig").FunctionType;
const CompilerType = @import("compile.zig").Compiler;
const Scanner = @import("scanner.zig");
const GC = @import("gc.zig");
const Value = @import("value.zig").Value;
const Obj = @import("obj.zig");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
    .retain_metadata = true,
}){};
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
    return interpret_impl(allocator, source, true);
}

pub fn interpret_without_teardown(allocator: Allocator, source: []const u8) !*VMType {
    return interpret_impl(allocator, source, false);
}

fn interpret_impl(allocator: Allocator, source: []const u8, comptime do_teardown: bool) !*VMType {
    var gc: *GC = try allocator.create(GC);
    try gc.init(allocator, false);

    var parser = Compiler.init_parser();
    var scanner = Scanner.init(source);
    var compiler = try Compiler.init(gc, errw, null, &scanner, &parser, FunctionType.Script);
    // note that this may get freed by gc depending on how we choose to implement it later
    const function = try compiler.compile() orelse return InterpretError.CompileError;
    const closure = try Obj.Closure.init(gc, function);

    try VM.init(gc, closure);
    gc.patch_allocator();
    defer {
        if (comptime do_teardown) {
            _ = VM.free() catch {};
        }
    }

    try VM.run();
    return VM;
}

test "instance get set" {
    const source =
  \\class Pair {}
  \\
  \\var pair = Pair();
  \\pair.first = 1;
  \\pair.second = 2;
  \\var result = pair.first + pair.second;  
  ;

  const vm = try interpret_without_teardown(alloc, source);
  defer {
    _ = vm.free() catch {};
  }

  const result_str = try vm.get_string("result");
  const value = vm.gc.globals.get(result_str) orelse @panic("result not found");

  value.print(debug);
  try std.testing.expect(Value.eq(value, Value.number(3)));
}

test "upvalue closed" {
    const source = 
  \\fun makeClosure() {
  \\  var a = 1;
  \\  fun f() {
  \\    a = a + 1;
  \\    return a;
  \\  }
  \\  return f;
  \\}
  \\
  \\var closure = makeClosure();
  \\var first = closure();
  \\var anotherClosure = makeClosure();
  \\var second = anotherClosure();
  \\var third = closure();
  ;
    const vm = try interpret_without_teardown(alloc, source);
    defer {
        _ = vm.free() catch {};
    }
    const first_str = try vm.get_string("first");
    const second_str = try vm.get_string("second");
    const third_str = try vm.get_string("third");

    const value1 = vm.gc.globals.get(first_str) orelse @panic("first not found");
    const value2 = vm.gc.globals.get(second_str) orelse @panic("second not found");
    const value3 = vm.gc.globals.get(third_str) orelse @panic("third not found");

    try std.testing.expect(Value.eq(value1, Value.number(2)));
    try std.testing.expect(Value.eq(value2, Value.number(2)));
    try std.testing.expect(Value.eq(value3, Value.number(3)));
}

test "set immediate upvalue" {
    const source = 
      \\fun outer() {
      \\  var x = 420;
      \\  fun inner() {
      \\    x = x + 1;
      \\    return x;
      \\   }
      \\ return inner();
      \\}
      \\var value = outer();
    ;
    const vm = try interpret_without_teardown(alloc, source);
    defer {
        _ = vm.free() catch {};
    }
    const result_str = try vm.get_string("value");
    const result = vm.gc.globals.get(result_str) orelse @panic("value not found");
    try std.testing.expect(Value.eq(result, Value.number(421)));
}

test "immediate upvalue" {
    const source =
      \\var result = "nothing";
      \\fun outer() {
      \\  var x = 420;
      \\  fun inner() {
      \\    result = x;
      \\  }
      \\  inner();
      \\}
      \\outer();
      ;
    const vm = try interpret_without_teardown(alloc, source);
    defer {
        _ = vm.free() catch {};
    }
    const result_str = try vm.get_string("result");
    const result = vm.gc.globals.get(result_str) orelse @panic("result not found");
    try std.testing.expect(Value.eq(result, Value.number(420)));
}

test "call native fn" {
    const source = 
      \\var num = __dummy();
      ;
    const vm = try interpret_without_teardown(alloc, source);
    defer {
        _ = vm.free() catch {};
    }
    const result_str = try vm.get_string("num");
    const result = vm.gc.globals.get(result_str) orelse @panic("num not found");
    try std.testing.expect(Value.eq(result, Value.number(420)));
}

test "call fn" {
    const source = 
      \\fun add420(num) {
      \\    return num + 420;
      \\}
      \\   
      \\fun add69(num) {
      \\    return num + 69;
      \\}
      \\  
      \\var num = add420(1);
      \\num = add69(num);
      \\num = add420(num);
      ;
    const vm = try interpret_without_teardown(alloc, source);
    defer {
        _ = vm.free() catch {};
    }
    const result_str = try vm.get_string("num");
    const result = vm.gc.globals.get(result_str) orelse @panic("num not found");
    try std.testing.expect(Value.eq(result, Value.number(910)));
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
    const expected_str = try vm.get_string("it worked");
    const result_str = try vm.get_string("result");
    const result = vm.gc.globals.get(result_str) orelse @panic("result not found");
    try std.testing.expect(Value.eq(result, Value.obj(expected_str.widen())));
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
