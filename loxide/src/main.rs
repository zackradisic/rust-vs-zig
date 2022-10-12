pub mod chunk;
pub mod compile;
pub mod value;
pub mod vm;

use std::{io::BufRead, path::Path};

use compile::compile;
use vm::InterpretResult;

use crate::{
    chunk::{Chunk, Opcode},
    value::Value,
    vm::VM,
};

fn main() {
    let mut args = std::env::args();
    let _ = args.next();

    match args.len() {
        0 => {
            repl();
        }
        1 => {
            run_file(args.next().unwrap());
        }
        _ => panic!(),
    }
}

fn repl() {
    let stdin = std::io::stdin();
    let mut lines = stdin.lock().lines();

    while let Some(line) = lines.next() {
        let line = line.unwrap();
        interpret(&line).unwrap();
    }
}

fn run_file<P: AsRef<Path>>(path: P) {
    let string = std::fs::read_to_string(path).unwrap();
    interpret(&string).unwrap();
}

fn interpret(src: &str) -> InterpretResult<()> {
    let tokens = compile(src);
    Ok(())
}

// fn main() {
//     let mut chunk = Chunk::new();

//     let mut constant_idx = chunk.add_constant(Value(0.0));
//     chunk.write(Opcode::CONSTANT, 0);
//     chunk.write(constant_idx, 0);

//     constant_idx = chunk.add_constant(3.4.into());
//     chunk.write(Opcode::CONSTANT, 0);
//     chunk.write(constant_idx, 0);

//     chunk.write(Opcode::ADD, 0);

//     constant_idx = chunk.add_constant(5.6.into());
//     chunk.write(Opcode::CONSTANT, 0);
//     chunk.write(constant_idx, 0);

//     chunk.write(Opcode::DIVIDE, 0);

//     chunk.write(Opcode::NEGATE, 0);
//     chunk.write(Opcode::RETURN, 0);

//     let mut vm = VM::new(chunk);
//     vm.run().unwrap();
// }
