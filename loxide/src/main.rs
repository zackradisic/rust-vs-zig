#![feature(ptr_sub_ptr)]
#![feature(allocator_api)]
#![feature(slice_ptr_get)]
#![feature(let_chains)]

pub mod chunk;
pub mod compile;
pub mod mem;
pub mod native_fn;
pub mod obj;
pub mod obj2;
pub mod obj_id;
pub mod table;
pub mod table2;
pub mod value;
pub mod value2;
pub mod vm;

use std::{io::BufRead, mem::MaybeUninit, path::Path};

use compile::Parser;
use mem::Mem;

use vm::{InterpretError, InterpretResult, ValueStack, STACK_MAX};

use crate::vm::VM;

#[macro_export]
macro_rules! debug_println {
    () => {
        #[cfg(debug_assertions)]
        $std::print!("\n")
    };
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        println!($($arg)*);
    };
}

fn main() {
    // run_file("./test.lox")

    let mut args = std::env::args();
    let _ = args.next();

    match args.len() {
        0 => {
            repl();
        }
        1 => {
            let mut vm = VM::new();
            run_file(&mut vm, args.next().unwrap());
        }
        _ => panic!(),
    }
}

fn repl() {
    let stdin = std::io::stdin();
    let lines = stdin.lock().lines();
    let mut vm = VM::new();

    for line in lines {
        let line = line.unwrap();
        interpret(&mut vm, &line).unwrap();
    }
}

fn run_file<P: AsRef<Path>>(vm: &mut VM, path: P) {
    let string = std::fs::read_to_string(path).unwrap();
    interpret(vm, &string).unwrap();
}

fn interpret(vm: &mut VM, src: &str) -> InterpretResult<()> {
    let function = {
        let mut parser = Parser::new(src, &mut vm.mem);
        if !parser.compile() {
            return Err(InterpretError::CompileError);
        }
        parser.compiler.function
    };
    vm.init(function);

    vm.run()
}

#[cfg(test)]
mod test {

    use std::{cell::UnsafeCell, mem::MaybeUninit};

    use crate::{
        compile::Token,
        interpret,
        mem::Mem,
        table::Table,
        value::Value,
        vm::{InterpretError, ValueStack, STACK_MAX, VM},
    };

    #[test]
    fn fib() {
        let src = r#"
fun fib(x) {
    if (x <= 1) {
        return x;
    }
    return fib(x - 1) + fib(x - 2);
}

var result = fib(2);
"#;

        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
        let result_var_str = vm.get_string("result").as_non_null_ptr();
        let value = vm.mem.globals.get(result_var_str);
        assert_eq!(value.unwrap(), Value::Number(1.0));
    }

    #[test]
    fn superclasses() {
        let src = r#"
class Doughnut {
  cook() {
    print "Dunk in the fryer.";
    this.finish("sprinkles");
  }

  finish(ingredient) {
    return "Finish with " + ingredient;
  }
}

class Cruller < Doughnut {
  finish(ingredient) {
    // No sprinkles, always icing.
    return super.finish("icing");
  }
}

var cruller = Cruller();
var result = cruller.finish("noice");
"#;

        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
        let result_var_str = vm.get_string("result").as_non_null_ptr();
        let value = vm.mem.globals.get(result_var_str);
        assert_eq!(value.unwrap().as_str().unwrap(), "Finish with icing");
    }

    #[test]
    fn invoking_fields() {
        let src = r#"
        class Oops {
            init() {
                fun f() {
                    return 420;
                }

                this.field = f;
            }
        }

        var oops = Oops();
        var result = oops.field();
        "#;

        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
        let result_str = vm.get_string("result").as_non_null_ptr();
        let value = vm.mem.globals.get(result_str);
        assert_eq!(value, Some(Value::Number(420.0)));
    }

    #[test]
    fn misusing_this() {
        let src = r#"
        fun notMethod() {
            print this;
        }
        "#;

        let mut vm = VM::new();
        let err = interpret(&mut vm, src);
        if let Err(InterpretError::CompileError) = err {
        } else {
            panic!()
        }
    }

    #[test]
    fn nested_this() {
        let src = r#"
        class Nested {
            method() {
              fun function() {
                return this.lol;
              }
              return function();
            }
        }
          
        var nested = Nested();
        nested.lol = 420;
        var result = nested.method();"#;

        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
        let result_str = vm.get_string("result").as_non_null_ptr();
        let value = vm.mem.globals.get(result_str);
        assert_eq!(value, Some(Value::Number(420.0)));
    }

    #[test]
    fn this() {
        let src = r#"
        class Nested {
            method() {
              return this.lol;
            }
          }
          
        var nested = Nested();
        nested.lol = 420;
        var result = nested.method();"#;

        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
        let result_str = vm.get_string("result").as_non_null_ptr();
        let value = vm.mem.globals.get(result_str);
        assert_eq!(value, Some(Value::Number(420.0)));
    }

    #[test]
    fn methods() {
        let src = r#"
        class Scone {
            topping(first, second) {
              return "scone with " + first + " and " + second;
            }
          }
          
          var scone = Scone();
          var result = scone.topping("berries", "cream");"#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();

        let result_str = vm.get_string("result").as_non_null_ptr();

        let value = vm.mem.globals.get(result_str);

        let expected_str = vm.get_string("scone with berries and cream");
        println!("VAL: {:?}", value);
        assert_eq!(value, Some(Value::Obj(expected_str.cast())));
    }

    #[test]
    fn instance_get_set() {
        let src = r#"
class Pair {}

var pair = Pair();
pair.first = 1;
pair.second = 2;
var result = pair.first + pair.second;"#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();

        let result_str = vm.get_string("result").as_non_null_ptr();

        let value = vm.mem.globals.get(result_str);

        assert_eq!(value, Some(Value::Number(3.0)));
    }

    #[test]
    fn upvalue_closed() {
        let src = r#"
    fun makeClosure() {
      var a = 1;
      fun f() {
        a = a + 1;
        return a;
      }
      return f;
    }

    var closure = makeClosure();
    var first = closure();
    var anotherClosure = makeClosure();
    var second = anotherClosure();
    var third = closure();"#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();

        let first_str = vm.get_string("first").as_non_null_ptr();
        let second_str = vm.get_string("second").as_non_null_ptr();
        let third_str = vm.get_string("third").as_non_null_ptr();

        let value1 = vm.mem.globals.get(first_str);
        let value2 = vm.mem.globals.get(second_str);
        let value3 = vm.mem.globals.get(third_str);

        assert_eq!(value1, Some(Value::Number(2.0)));
        assert_eq!(value2, Some(Value::Number(2.0)));
        assert_eq!(value3, Some(Value::Number(3.0)));
    }

    #[test]
    fn set_immediate_upvalue() {
        let src = r#"
    fun outer() {
      var x = 420;
      fun inner() {
        x = x + 1;
        return x;
      }
      return inner();
    }
    var value = outer();"#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
        let value_str = vm.get_string("value").as_non_null_ptr();

        let value = vm.mem.globals.get(value_str);

        assert_eq!(value, Some(Value::Number(421.0)));
    }

    #[test]
    fn immediate_upvalue() {
        let src = r#"
var result = "nothing";
fun outer() {
  var x = 420;
  fun inner() {
    result = x;
  }
  inner();
}
outer();"#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
        let result_str = vm.get_string("result").as_non_null_ptr();
        let value = vm.mem.globals.get(result_str);

        assert_eq!(value, Some(Value::Number(420.0)));
    }

    #[test]
    fn call_native_fn() {
        let src = r#"
        var num = __dummy();"#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
        let num_str = vm.get_string("num").as_non_null_ptr();
        let value = vm.mem.globals.get(num_str);
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
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
        let num_str = vm.get_string("num").as_non_null_ptr();
        let value = vm.mem.globals.get(num_str);
        assert_eq!(value, Some(Value::Number(910.0)));
    }

    #[test]
    fn print_fn() {
        let src = r#"
            fun bigNoob() {
              print "OH YEAH";
            }

            print bigNoob;"#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
    }

    #[test]
    fn if_stmt() {
        let src = r#"
            var noob = 420;
            if (420 > 69) { noob = "NICE"; } else { noob = "NOT NICE"; }
    "#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();

        let noob = vm.get_string("noob").as_non_null_ptr();
        let top = vm.mem.globals.get(noob);
        assert_eq!(top.unwrap().as_str(), Some("NICE"));
    }

    #[test]
    fn if_else_stmt() {
        let src = r#"
            var noob = 420;
            if (69 > 420) { noob = "wtf"; } else { noob = "NICE"; }
    "#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();

        let noob = vm.get_string("noob").as_non_null_ptr();
        let top = vm.mem.globals.get(noob);
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
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();

        let noob = vm.get_string("noob").as_non_null_ptr();
        let top = vm.mem.globals.get(noob);
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
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();

        // let noob = vm.get_string("global");
        // let top = vm.mem.globals.get(noob);
        // assert_eq!(top.unwrap().as_str(), Some("NICE"));
    }

    #[test]
    fn locals() {
        let src = r#"
            var global = 420;
            { var x = "HELLO"; x = "NICE"; global = x; }
    "#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();

        let noob = vm.get_string("global").as_non_null_ptr();
        let top = vm.mem.globals.get(noob);
        assert_eq!(top.unwrap().as_str(), Some("NICE"));
    }

    #[test]
    fn string() {
        let src = r#"var noob = "hello" + " sir" + " sir";"#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();

        let noob = vm.get_string("noob").as_non_null_ptr();
        let top = vm.mem.globals.get(noob);
        assert_eq!(top.unwrap().as_str(), Some("hello sir sir"));
    }

    #[test]
    fn print() {
        let src = r#"print 1 + 2;"#;
        let mut vm = VM::new();
        interpret(&mut vm, src).unwrap();
    }

    #[test]
    fn table() {
        let mut mem = Mem::new();
        let mut table = Table::new();

        let key = mem.copy_string("bagel").as_non_null_ptr();
        assert_eq!(table.set(key, Value::Number(420.0)), true);
        assert_eq!(table.set(key, Value::Number(69.0)), false);
        assert_eq!(table.get(key), Some(Value::Number(69.0)));
        assert_eq!(table.delete(key), true);
        assert_eq!(table.delete(key), false);

        Table::free(&mut table);
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

        chunk[offset as usize] = (jump >> 8) as u8;
        chunk[offset as usize + 1] = jump as u8;

        let val = ((chunk[offset as usize] as u16) << 8) | (chunk[offset as usize + 1] as u16);

        println!("{jump} NOOB: {chunk:?} JUMP: {val} {}", 2u16);
    }

    // #[test]
    // fn miri_test() {
    //     let mut obj = Box::into_raw(Box::new(69));
    //     let mut obj2 = unsafe { obj.as_mut().unwrap() };
    //     let foo = unsafe { *obj };
    //     *obj2 = 9999;
    // }
    // #[test]
    // fn miri_test2() {
    //     let mut obj = Box::into_raw(Box::new(69));
    //     let mut obj2 = unsafe { obj.as_mut().unwrap() };
    //     unsafe {
    //         *obj = 420;
    //     };
    //     *obj2 = 9999;
    // }
}

fn _f(_a: i32, _b: i32) -> i32 {
    420
}

fn _noob() {
    let _noob = _f(_f(1, 2), _f(3, 4));
}
