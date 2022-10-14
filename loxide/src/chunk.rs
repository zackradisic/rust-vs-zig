use std::ops::Deref;

use crate::value::{Value, ValueArray};

#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct Opcode(pub u8);

impl Opcode {
    pub const RETURN: u8 = 0;
    pub const CONSTANT: u8 = 1;
    pub const NEGATE: u8 = 2;
    pub const ADD: u8 = 3;
    pub const SUBTRACT: u8 = 4;
    pub const MULTIPLY: u8 = 5;
    pub const DIVIDE: u8 = 6;
    pub const NIL: u8 = 7;
    pub const TRUE: u8 = 8;
    pub const FALSE: u8 = 9;
    pub const NOT: u8 = 10;
}

pub struct Chunk {
    pub code: Vec<Opcode>,
    pub constants: ValueArray,
    pub lines: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn iter(&self) -> ChunkIter {
        ChunkIter {
            chunk: self,
            offset: 0,
        }
    }

    pub fn iter_debug(&self) -> ChunkIterDebug {
        ChunkIterDebug(ChunkIter {
            chunk: self,
            offset: 0,
        })
    }

    pub fn write(&mut self, op: u8, line: u32) {
        self.code.push(Opcode(op));
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        let index = self.constants.len();
        self.constants.push(value);
        index.try_into().unwrap()
    }

    /// Dissamble instruction and increment offset to the start of
    /// the next one
    pub fn disassemble_instruction(&self, offset: &mut usize) -> Option<Instruction> {
        let instr = self.code[*offset];
        match instr.0 {
            Opcode::NOT
            | Opcode::TRUE
            | Opcode::FALSE
            | Opcode::NIL
            | Opcode::ADD
            | Opcode::SUBTRACT
            | Opcode::MULTIPLY
            | Opcode::DIVIDE
            | Opcode::NEGATE
            | Opcode::RETURN => {
                *offset += 1;
                Some(Instruction::Simple(instr))
            }
            Opcode::CONSTANT => {
                let constant_idx = self.code[*offset + 1];
                let constant = self.constants[constant_idx.0 as usize];
                *offset += 2;
                Some(Instruction::Constant(constant))
            }
            otherwise => panic!("Invalid opcode {}", otherwise),
        }
    }
}

impl Deref for Chunk {
    type Target = Vec<Opcode>;

    fn deref(&self) -> &Self::Target {
        &self.code
    }
}

impl std::fmt::Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter_debug()).finish()
    }
}

#[derive(Debug)]
pub struct InstructionDebug {
    pub line: u32,
    pub inner: Instruction,
}
pub enum Instruction {
    Simple(Opcode),
    Constant(Value),
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Simple(arg0) => {
                let string = match arg0.0 {
                    Opcode::RETURN => "Return",
                    Opcode::NEGATE => "Negate",
                    Opcode::ADD => "Add",
                    Opcode::SUBTRACT => "Subtract",
                    Opcode::MULTIPLY => "Multiply",
                    Opcode::DIVIDE => "Divide",
                    Opcode::TRUE => "True",
                    Opcode::FALSE => "False",
                    Opcode::NIL => "Nil",
                    Opcode::NOT => "Not",
                    other => panic!("Invalid opcode {:?}", other),
                };
                write!(f, "{:?}", string)
            }
            Instruction::Constant(val) => f.debug_tuple("Constant").field(val).finish(),
        }
    }
}

pub struct ChunkIter<'a> {
    chunk: &'a Chunk,
    offset: usize,
}

impl<'a> ChunkIter<'a> {
    /// Dissamble instruction and increment offset to the start of
    /// the next one
    fn disassemble_instruction(&mut self) -> Option<Instruction> {
        self.chunk.disassemble_instruction(&mut self.offset)
    }
}

impl<'a> Iterator for ChunkIter<'a> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.chunk.len() {
            return None;
        }

        self.disassemble_instruction()
    }
}

pub struct ChunkIterDebug<'a>(ChunkIter<'a>);

impl<'a> Iterator for ChunkIterDebug<'a> {
    type Item = InstructionDebug;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.offset >= self.0.chunk.len() {
            return None;
        }

        let line = self.0.chunk.lines[self.0.offset];
        let inner = self.0.disassemble_instruction();

        inner.map(|inner| InstructionDebug { inner, line })
    }
}

#[cfg(test)]
mod test {
    use crate::chunk::{Chunk, Opcode};
}
