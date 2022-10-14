use std::mem::MaybeUninit;

use crate::{
    chunk::{Chunk, InstructionDebug, Opcode},
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
    // TODO: Make this an instruction pointer?
    instruction_index: usize,
    stack: [MaybeUninit<Value>; STACK_MAX],
    stack_top: u32,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
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
            self.runtime_error("Operands must be numbers.");
            return Err(InterpretError::RuntimeError);
        }

        let b = self.pop();
        let a = self.pop();
        self.push(f(a, b));

        Ok(())
    }

    fn reset_stack(&mut self) {}

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

            match byte {
                Opcode::NOT => {
                    let top = self.pop();
                    self.push(Value::Bool(top.is_falsey()))
                }
                Opcode::NEGATE => {
                    if !matches!(self.peek(0), Value::Bool(_) | Value::Number(_)) {
                        self.runtime_error("Operand must be a number.");
                        return Err(InterpretError::RuntimeError);
                    }

                    let negated = -self.pop();
                    self.push(negated)
                }
                Opcode::RETURN => {
                    println!("return {:?}", self.pop());
                    return Ok(());
                }
                Opcode::CONSTANT => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                Opcode::ADD => self.binary_op(std::ops::Add::add)?,
                Opcode::SUBTRACT => self.binary_op(std::ops::Sub::sub)?,
                Opcode::MULTIPLY => self.binary_op(std::ops::Mul::mul)?,
                Opcode::DIVIDE => self.binary_op(std::ops::Div::div)?,
                otherwise => panic!("Unknown opcode {:?}", otherwise),
            }
        }
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        let ret = self.chunk[self.instruction_index].0;
        self.instruction_index += 1;
        ret
    }

    #[inline]
    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte();
        self.chunk.constants[idx as usize]
    }
}
