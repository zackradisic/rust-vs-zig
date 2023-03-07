# Rust vs. Zig

This is an experiment to evaluate Rust vs. Zig by writing a bytecode interpreter with GC in both languages and comparing them.

The interpreter implementation is from the [Crafting Interpreters](https://craftinginterpreters.com) book.

I wrote a comprehensive summary of this experiment [on my personal blog](https://zackoverflow.dev/writing/unsafe-rust-vs-zig).

## Rust implementation

This is in the [loxide](loxide/) folder.

To build it:

```bash
# in the root project directory
make rust

# inside the rust folder
cargo build --release
```

To run the tests:

```bash
cargo test

# Run with miri to check for undefined behaviour
cargo miri test
```

## Zig implementation

This is in the [zlox](zlox/) folder.

To build it:

```bash
# in the root project directory
make zig

# inside the zlox folder
# run `zig build help` for options
zig build
```

To run the tests:

```bash
zig build test
zig-out/bin/zlox
```

## Benchmarks

The [benchmarks](benchmarks/) folder contains the code ("\*.lox" files) the two interpreters run and the results of the benchmarks. The results are run using hyperfine.
