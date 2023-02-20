rust:
	cd loxide && cargo build --release;
	cp loxide/target/release/loxide out/loxide;

zig-debug:
	cd zlox && zig build -Dztracy=false -Dtracing=false -Ddebug_log_gc=false -Ddebug_stress_gc=false -Dprint_code_after_compile=true -Drelease-safe=false --verbose;
	cp zlox/zig-cache/bin/zlox out/zlox;

zig:
	cd zlox && zig build -Dztracy=false -Dtracing=false -Ddebug_log_gc=false -Ddebug_stress_gc=false -Dprint_code_after_compile=false -Drelease-fast=true;
	cp zlox/zig-out/bin/zlox out/zlox;