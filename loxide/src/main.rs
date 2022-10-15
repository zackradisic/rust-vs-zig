pub mod chunk;
pub mod compile;
pub mod obj;
pub mod value;
pub mod vm;

use std::{io::BufRead, path::Path};

use compile::Compiler;
use vm::{InterpretError, InterpretResult};

use crate::{
    chunk::{Chunk, Opcode},
    value::Value,
    vm::VM,
};

fn main() {
    // run_file("./test.lox")

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

fn interpret(src: &str) -> InterpretResult<VM> {
    let chunk = Chunk::new();

    let (chunk, obj_list) = {
        let mut compiler = Compiler::new(src, chunk);
        if !compiler.compile() {
            return Err(InterpretError::CompileError);
        }
        (compiler.chunk, compiler.obj_list)
    };

    let mut vm = VM::new(chunk, obj_list);

    vm.run().map(|_| vm)
}

#[cfg(test)]
mod test {
    use std::{alloc::Layout, ptr::NonNull};

    use crate::interpret;

    #[test]
    fn string() {
        let src = r#""hello" + " sir""#;
        let vm = interpret(src).unwrap();

        {
            assert_eq!(vm.stack_top, 1);
            let top = unsafe { vm.stack[0].assume_init() };
            assert_eq!(top.as_str(), Some("hello sir"))
        }
    }
}
