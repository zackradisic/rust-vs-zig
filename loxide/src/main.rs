pub mod chunk;
pub mod compile;
pub mod obj;
pub mod table;
pub mod value;
pub mod vm;

use std::{io::BufRead, path::Path};

use compile::Compiler;
use table::Table;
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

    let (chunk, obj_list, strings) = {
        let mut compiler = Compiler::new(src, chunk, Table::new());
        if !compiler.compile() {
            return Err(InterpretError::CompileError);
        }
        (compiler.chunk, compiler.obj_list, compiler.interned_strings)
    };

    println!("CHUNK: {:?}", chunk);

    let mut vm = VM::new(chunk, obj_list, strings);

    vm.run().map(|_| vm)
}

#[cfg(test)]
mod test {

    use crate::{
        interpret,
        obj::{Obj, ObjString},
        table::Table,
        value::Value,
    };

    #[test]
    fn dafuq() {
        let src = r#"var noob = "nice";
print noob;  "#;
        let vm = interpret(src).unwrap();

        {
            // assert_eq!(vm.stack_top, 1);
            // let top = unsafe { vm.stack[0].assume_init() };
            // assert_eq!(top.as_str(), Some("hello sir sir"))
        }
    }

    #[test]
    fn string() {
        let src = r#""hello" + " sir" + " sir";"#;
        let vm = interpret(src).unwrap();

        {
            assert_eq!(vm.stack_top, 1);
            let top = unsafe { vm.stack[0].assume_init() };
            assert_eq!(top.as_str(), Some("hello sir sir"))
        }
    }

    #[test]
    fn print() {
        let src = r#"print 1 + 2;"#;
        let vm = interpret(src).unwrap();

        {
            assert_eq!(vm.stack_top, 0);
            // let top = unsafe { vm.stack[0].assume_init() };
            // assert_eq!(top.as_str(), Some("3"))
        }
    }

    #[test]
    fn table() {
        let mut table = Table::new();
        let mut interned_strings = Table::new();
        let mut obj_list = Default::default();

        let key = ObjString::copy_string(&mut interned_strings, &mut obj_list, "bagel");
        assert_eq!(table.set(key, Value::Number(420.0)), true);
        assert_eq!(table.set(key, Value::Number(69.0)), false);
        assert_eq!(table.get(key), Some(Value::Number(69.0)));
        assert_eq!(table.delete(key), true);
        assert_eq!(table.delete(key), false);

        for obj in obj_list.iter_mut() {
            Obj::free(*obj)
        }
        Table::free(&mut table);
        Table::free(&mut interned_strings);
    }

    #[test]
    fn ohshit() {
        let bytes = [0, 1, 2, 3];

        println!("NOOB: {:?}", std::mem::size_of::<&[u8]>());
    }
}
