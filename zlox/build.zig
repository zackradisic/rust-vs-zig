const std = @import("std");
const OptionStep = std.build.OptionsStep;

pub const BuildOptions = struct {
    print_code_after_compile: bool = false,
    tracing: bool = false,
    safe_obj_cast: bool = false,
    debug_stress_gc: bool = false,
    debug_log_gc: bool = false,
    gc_heap_grow_factor: usize = 2,

    fn from_builder(b: *std.build.Builder) BuildOptions {
        const print_code_after_compile = b.option(bool, "print_code_after_compile", "Whether to print code after compile") orelse blk: {
            break :blk !b.is_release;
        };
        const tracing = b.option(bool, "tracing", "Enable tracing of stack when running VM") orelse blk: {
            break :blk !b.is_release;
        };
        const safe_obj_cast = b.option(bool, "safe_obj_cast", "Enable safe object casting DONT ENABLE THIS IS BROKEN!") orelse blk: {
            break :blk false;
        };
        const debug_stress_gc = b.option(bool, "debug_stress_gc", "Runs the GC on every allocation for finding GC bugs") orelse blk: {
            break :blk !b.is_release;
        };
        const debug_log_gc = b.option(bool, "debug_log_gc", "Log GC mark/sweep steps") orelse blk: {
            break :blk !b.is_release;
        };
        const gc_heap_grow_factor = b.option(usize, "gc_heap_grow_factor", "How much to grow the GC heap") orelse blk: {
            break :blk 2;
        };

        return .{ .print_code_after_compile = print_code_after_compile, .tracing = tracing, .safe_obj_cast = safe_obj_cast, .debug_stress_gc = debug_stress_gc, .debug_log_gc = debug_log_gc, .gc_heap_grow_factor = gc_heap_grow_factor };
    }
};

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    var mode = b.standardReleaseOptions();

    // const lib = b.addStaticLibrary("zlox", "src/main.zig");
    // lib.setBuildMode(mode);
    // lib.install();

    var opt = b.addOptions();
    const opts = BuildOptions.from_builder(b);
    opt.addOption(@TypeOf(opts.print_code_after_compile), "PRINT_CODE_AFTER_COMPILE", opts.print_code_after_compile);
    opt.addOption(@TypeOf(opts.tracing), "TRACING", opts.tracing);
    opt.addOption(@TypeOf(opts.safe_obj_cast), "SAFE_OBJ_CAST", opts.safe_obj_cast);
    opt.addOption(@TypeOf(opts.debug_stress_gc), "DEBUG_STRESS_GC", opts.debug_stress_gc);
    opt.addOption(@TypeOf(opts.debug_log_gc), "DEBUG_LOG_GC", opts.debug_log_gc);
    opt.addOption(@TypeOf(opts.gc_heap_grow_factor), "GC_HEAP_GROW_FACTOR", opts.gc_heap_grow_factor);

    const bin = b.addExecutable("main", "src/main.zig");
    bin.addPackagePath("clap", "libs/zig-clap/clap.zig");
    bin.addOptions("build_options", opt);
    bin.setBuildMode(mode);
    bin.install();

    const test_filter = b.option([]const u8, "test-filter", "Skip tests that do not match filter");
    const main_tests = b.addTestExe("test", "src/main.zig");
    main_tests.setBuildMode(mode);
    main_tests.addOptions("build_options", opt);
    main_tests.setFilter(test_filter);
    main_tests.install();

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
