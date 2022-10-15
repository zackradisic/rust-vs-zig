use std::{
    alloc::{self, Layout},
    mem::MaybeUninit,
    ptr::{self, NonNull},
};

use crate::{
    chunk::{Chunk, InstructionDebug, Opcode},
    obj::{Obj, ObjList, ObjString},
    value::Value,
};

pub type InterpretResult<T> = Result<T, InterpretError>;

#[derive(Debug)]
pub enum InterpretError {
    RuntimeError,
    CompileError,
}

const STACK_MAX: usize = 256;

pub struct VM {
    chunk: Chunk,
    obj_list: ObjList,
    // TODO: Make this an instruction pointer?
    instruction_index: usize,
    pub stack: [MaybeUninit<Value>; STACK_MAX],
    pub stack_top: u32,
}

impl VM {
    pub fn new(chunk: Chunk, obj_list: ObjList) -> Self {
        Self {
            chunk,
            obj_list,
            instruction_index: 0,
            stack: [MaybeUninit::uninit(); STACK_MAX],
            stack_top: 0,
        }
    }

    fn push(&mut self, val: Value) {
        self.stack[self.stack_top as usize] = MaybeUninit::new(val);
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        unsafe { MaybeUninit::assume_init(self.stack[self.stack_top as usize]) }
    }

    #[inline]
    fn binary_op<F: FnOnce(Value, Value) -> Value>(&mut self, f: F) -> InterpretResult<()> {
        if !matches!(self.peek(0), Value::Number(_)) || !matches!(self.peek(1), Value::Number(_)) {
            self.runtime_error("Operands must be two numbers or two strings.");
            return Err(InterpretError::RuntimeError);
        }

        let b = self.pop();
        let a = self.pop();
        self.push(f(a, b));

        Ok(())
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    fn runtime_error(&mut self, err: &str) {
        eprintln!("{}", err);

        let instr_idx = self.instruction_index - 1;
        let line = self.chunk.lines[instr_idx];
        println!("[line {}] in script", line);
        self.reset_stack();
    }

    fn peek(&self, distance: u32) -> Value {
        unsafe { self.stack[self.stack_top as usize - 1 - distance as usize].assume_init() }
    }

    fn concatenate(&mut self) {
        let b = self.pop();
        let a = self.pop();

        let b = b.as_obj_str().unwrap();
        let a = a.as_obj_str().unwrap();

        let new_len = a.len + b.len;

        let obj_str = if new_len == 0 {
            ObjString::alloc_str(&mut self.obj_list, NonNull::dangling(), 0) as *mut Obj
        } else {
            let layout = Layout::array::<u8>(new_len as usize).unwrap();
            let chars = unsafe { alloc::alloc(layout) };

            unsafe {
                ptr::copy_nonoverlapping(a.chars.as_ptr(), chars, a.len as usize);
                ptr::copy_nonoverlapping(
                    b.chars.as_ptr(),
                    chars.offset(a.len as isize),
                    b.len as usize,
                );
            }

            ObjString::alloc_str(
                &mut self.obj_list,
                unsafe { NonNull::new_unchecked(chars) },
                new_len,
            ) as *mut Obj
        };

        self.push(Value::Obj(obj_str))
    }

    pub fn run(&mut self) -> InterpretResult<()> {
        loop {
            #[cfg(debug_assertions)]
            {
                // Debug stack
                print!("          ");
                for slot in self.stack.iter().take(self.stack_top as usize) {
                    let value: &Value = unsafe { slot.assume_init_ref() };
                    println!("[ {:?} ]", value);
                }
                print!("\n");

                // Debug instruction
                let mut duplicate_instruction_index = self.instruction_index;
                let line = self.chunk.lines[self.instruction_index];
                let inner = self
                    .chunk
                    .disassemble_instruction(&mut duplicate_instruction_index);
                println!("{:?}", inner.map(|inner| InstructionDebug { line, inner }));
            }

            let byte = self.read_byte();

            match Opcode::from_u8(byte) {
                Some(Opcode::Equal) => {
                    let b = self.pop();
                    let a = self.pop();

                    self.push(Value::Bool(a == b))
                }
                Some(Opcode::Not) => {
                    let top = self.pop();
                    self.push(Value::Bool(top.is_falsey()))
                }
                Some(Opcode::Negate) => {
                    if !matches!(self.peek(0), Value::Bool(_) | Value::Number(_)) {
                        self.runtime_error("Operand must be a number.");
                        return Err(InterpretError::RuntimeError);
                    }

                    let negated = -self.pop();
                    self.push(negated)
                }
                Some(Opcode::Return) => {
                    // println!("return {:?}", self.pop());
                    println!("return {:?}", self.peek(self.stack_top - 1));
                    return Ok(());
                }
                Some(Opcode::Constant) => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                Some(Opcode::Subtract) => self.binary_op(std::ops::Sub::sub)?,
                Some(Opcode::Multiply) => self.binary_op(std::ops::Mul::mul)?,
                Some(Opcode::Divide) => self.binary_op(std::ops::Div::div)?,
                Some(Opcode::Greater) => self.binary_op(Value::gt_owned)?,
                Some(Opcode::Less) => self.binary_op(Value::lt_owned)?,
                Some(Opcode::Add) => {
                    if self.peek(0).is_str() && self.peek(1).is_str() {
                        self.concatenate();
                    } else {
                        self.binary_op(std::ops::Add::add)?
                    }
                }
                otherwise => panic!("Unknown opcode {:?}", otherwise),
            }
        }
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        let ret = self.chunk[self.instruction_index];
        self.instruction_index += 1;
        ret
    }

    #[inline]
    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte();
        self.chunk.constants[idx as usize]
    }

    fn free_objects(&mut self) {
        for obj in self.obj_list.iter_mut() {
            Obj::free(*obj)
        }
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        self.free_objects();
    }
}
