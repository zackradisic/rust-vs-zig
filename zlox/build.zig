const std = @import("std");
const OptionStep = std.build.OptionsStep;
// Using commit that is compatible with our version of Zig
// https://github.com/michal-z/zig-gamedev/tree/c8c955ab614610f25b7daf48fa5fee5cc291065d/libs/ztracy
const ztracy = @import("libs/ztracy/libs/ztracy/build.zig");

pub const BuildOptions = struct {
    print_code_after_compile: bool = false,
    tracing: bool = false,
    safe_obj_cast: bool = false,
    debug_stress_gc: bool = false,
    debug_log_gc: bool = false,
    gc_heap_grow_factor: usize = 2,
    ztracy: bool = false,

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
        const use_ztracy = b.option(bool, "ztracy", "Enable ztracy") orelse blk: {
            break :blk false;
        };

        return .{ .print_code_after_compile = print_code_after_compile, .tracing = tracing, .safe_obj_cast = safe_obj_cast, .debug_stress_gc = debug_stress_gc, .debug_log_gc = debug_log_gc, .gc_heap_grow_factor = gc_heap_grow_factor, .ztracy = use_ztracy };
    }
};

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    var mode = b.standardReleaseOptions();

    // const lib = b.addStaticLibrary("zlox", "src/main.zig");
    // lib.setBuildMode(mode);
    // lib.install();

    std.debug.print("Is release? {?}\n", .{b.is_release});

    var opt = b.addOptions();
    const opts = BuildOptions.from_builder(b);
    opt.addOption(@TypeOf(opts.print_code_after_compile), "PRINT_CODE_AFTER_COMPILE", opts.print_code_after_compile);
    opt.addOption(@TypeOf(opts.tracing), "TRACING", opts.tracing);
    opt.addOption(@TypeOf(opts.safe_obj_cast), "SAFE_OBJ_CAST", opts.safe_obj_cast);
    opt.addOption(@TypeOf(opts.debug_stress_gc), "DEBUG_STRESS_GC", opts.debug_stress_gc);
    opt.addOption(@TypeOf(opts.debug_log_gc), "DEBUG_LOG_GC", opts.debug_log_gc);
    opt.addOption(@TypeOf(opts.gc_heap_grow_factor), "GC_HEAP_GROW_FACTOR", opts.gc_heap_grow_factor);
    opt.addOption(@TypeOf(opts.ztracy), "ZTRACY", opts.ztracy);

    const bin = b.addExecutable("zlox", "src/main.zig");
    // Uncomment to enable tracy, it's kind of useless on macos though
    // const ztracy_opts = ztracy.BuildOptionsStep.init(b, .{ .enable_ztracy = opts.ztracy });
    // const ztracy_pkg = ztracy.getPkg(&.{ztracy_opts.getPkg()});
    // bin.addPackage(ztracy_pkg);
    // ztracy.link(bin, ztracy_opts);

    // bin.addPackagePath("ztracy", "libs/ztracy/libs/ztracy/src/ztracy.zig");
    bin.addPackagePath("clap", "libs/zig-clap/clap.zig");
    bin.addOptions("build_options", opt);
    bin.setBuildMode(if (!b.is_release) .Debug else mode);

    // For some reason debug symbols weren't showing up, but then I fucked around and
    // one of these at least allows me to see them in Instruments on macos
    if (!b.is_release) {
        bin.strip = false;
        bin.dll_export_fns = true;
        bin.verbose_link = true;
        bin.export_table = true;
        bin.link_function_sections = true;
        bin.link_emit_relocs = true;
        bin.link_eh_frame_hdr = true;
        bin.link_z_relro = true;
        bin.link_gc_sections = true;
        bin.dead_strip_dylibs = false;
        bin.bundle_compiler_rt = true;
        // bin.use_llvm = false;
    }

    bin.install();

    const run_cmd = bin.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_filter = b.option([]const u8, "test-filter", "Skip tests that do not match filter");
    const main_tests = b.addTestExe("test", "src/main.zig");
    main_tests.setBuildMode(mode);
    main_tests.addOptions("build_options", opt);
    main_tests.setFilter(test_filter);
    main_tests.install();

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
