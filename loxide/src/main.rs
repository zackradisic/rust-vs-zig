pub mod chunk;
pub mod compile;
pub mod native_fn;
pub mod obj;
pub mod table;
pub mod value;
pub mod vm;

use std::{io::BufRead, path::Path};

use compile::{Compiler, Parser, Scanner};
use obj::ObjList;
use table::Table;
use vm::{InterpretError, InterpretResult};

use crate::{compile::FunctionKind, vm::VM};

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
    let lines = stdin.lock().lines();

    for line in lines {
        let line = line.unwrap();
        interpret(&line).unwrap();
    }
}

fn run_file<P: AsRef<Path>>(path: P) {
    let string = std::fs::read_to_string(path).unwrap();
    interpret(&string).unwrap();
}

fn interpret(src: &str) -> InterpretResult<VM> {
    let mut obj_list = ObjList::default();
    let mut interned_strings = Table::new();

    let function = {
        let mut scanner = Scanner::new(src);
        let mut parser = Parser::new();
        let mut compiler = Compiler::new(
            FunctionKind::Script,
            &mut interned_strings,
            &mut obj_list,
            &mut scanner,
            &mut parser,
        );
        if !compiler.compile() {
            return Err(InterpretError::CompileError);
        }
        compiler.function
    };

    let mut vm = VM::new(function, obj_list, interned_strings);

    vm.run().map(|_| vm)
}

#[cfg(test)]
mod test {

    use crate::{
        compile::Token,
        interpret,
        native_fn::NativeFn,
        obj::{Obj, ObjString},
        table::Table,
        value::Value,
    };

    #[test]
    fn call_native_fn() {
        let src = r#"
        var num = __dummy();"#;
        let mut vm = interpret(src).unwrap();
        let num_str = vm.get_string("num");
        let value = vm.globals.get(num_str);
        assert_eq!(value, Some(Value::Number(420.0)));
    }

    #[test]
    fn call_fn() {
        let src = r#"
        fun add420(num) {
          return num + 420;
        }

        fun add69(num) {
          return num + 69;
        }

        var num = add420(1);
        num = add69(num);
        num = add420(num);"#;
        let mut vm = interpret(src).unwrap();
        let num_str = vm.get_string("num");
        let value = vm.globals.get(num_str);
        assert_eq!(value, Some(Value::Number(910.0)));
    }

    #[test]
    fn print_fn() {
        let src = r#"
        fun bigNoob() {
          print "OH YEAH";
        }

        print bigNoob;"#;
        let _vm = interpret(src).unwrap();
    }

    #[test]
    fn if_stmt() {
        let src = r#"
        var noob = 420;
        if (420 > 69) { noob = "NICE"; } else { noob = "NOT NICE"; }
"#;
        let mut vm = interpret(src).unwrap();

        let noob = vm.get_string("noob");
        let top = vm.globals.get(noob);
        assert_eq!(top.unwrap().as_str(), Some("NICE"));
    }

    #[test]
    fn if_else_stmt() {
        let src = r#"
        var noob = 420;
        if (69 > 420) { noob = "wtf"; } else { noob = "NICE"; }
"#;
        let mut vm = interpret(src).unwrap();

        let noob = vm.get_string("noob");
        let top = vm.globals.get(noob);
        assert_eq!(top.unwrap().as_str(), Some("NICE"));
    }

    #[test]
    fn while_loop() {
        let src = r#"
        var noob = 0;
        while (noob < 10) {
          noob = noob + 1;
        }
"#;
        let mut vm = interpret(src).unwrap();

        let noob = vm.get_string("noob");
        let top = vm.globals.get(noob);
        assert_eq!(top, Some(Value::Number(10.0)));
    }

    #[test]
    fn for_loop() {
        let src = r#"
        var noob = 420;
        for (var x = 0; x < 10; x = x + 1) {
          noob = x;
        }
"#;
        let _vm = interpret(src).unwrap();

        // let noob = vm.get_string("global");
        // let top = vm.globals.get(noob);
        // assert_eq!(top.unwrap().as_str(), Some("NICE"));
    }

    #[test]
    fn locals() {
        let src = r#"
        var global = 420;
        { var x = "HELLO"; x = "NICE"; global = x; }
"#;
        let mut vm = interpret(src).unwrap();

        let noob = vm.get_string("global");
        let top = vm.globals.get(noob);
        assert_eq!(top.unwrap().as_str(), Some("NICE"));
    }

    #[test]
    fn string() {
        let src = r#"var noob = "hello" + " sir" + " sir";"#;
        let mut vm = interpret(src).unwrap();

        let noob = vm.get_string("noob");
        let top = vm.globals.get(noob);
        assert_eq!(top.unwrap().as_str(), Some("hello sir sir"));
    }

    #[test]
    fn print() {
        let src = r#"print 1 + 2;"#;
        let _vm = interpret(src).unwrap();
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
        // let bytes = [0, 1, 2, 3];

        println!("NOOB: {:?}", std::mem::size_of::<Token>());

        let values = [0, 1, 2, 3, 4, 5];
        println!(
            "NICE: {:?}",
            values.iter().take(3).rev().collect::<Vec<_>>()
        );

        // 0
        // 1
        // 2 ---
        // 3 ---
        // 4
        // 5
        //
        // 6
        // -2 to adjust for the 2 bytes for the jump offset
        let mut chunk = [0, 1, 2, 3, 4, 5];
        let offset = 2;
        let jump = chunk.len() as u32 - offset - 2;

        chunk[offset as usize] = (jump >> 8) as u8 & 0xff;
        chunk[offset as usize + 1] = jump as u8 & 0xff;

        let val = ((chunk[offset as usize] as u16) << 8) | (chunk[offset as usize + 1] as u16);

        println!(
            "{} NOOB: {:?} JUMP: {} {}",
            jump,
            chunk,
            val,
            0u16 << 8 | 2u16
        );
    }

    #[test]
    fn empty_string_hash() {
        println!("{:?}", std::mem::size_of::<NativeFn>());
        println!("{:?}", std::mem::size_of::<Value>());
    }
}

fn _f(_a: i32, _b: i32) -> i32 {
    420
}

fn _noob() {
    let _noob = _f(_f(1, 2), _f(3, 4));
}
