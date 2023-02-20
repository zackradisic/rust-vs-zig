rust:
	cd loxide && cargo build --release;

zig-debug:
	cd zlox && zig build -Dztracy=false -Dtracing=false -Ddebug_log_gc=false -Ddebug_stress_gc=false -Dprint_code_after_compile=true -Drelease-safe=false --verbose;

zig:
	cd zlox && zig build -Dztracy=false -Dtracing=false -Ddebug_log_gc=false -Ddebug_stress_gc=false -Dprint_code_after_compile=false -Drelease-fast=true;

bench:
	hyperfine --warmup 5 './zlox/zig-out/bin/zlox ./benchmarks/$(BENCH_PROG).lox' './loxide/target/release/loxide ./benchmarks/$(BENCH_PROG).lox'