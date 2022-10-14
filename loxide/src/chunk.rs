use std::ops::Deref;

use crate::value::{Value, ValueArray};

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Opcode {
    Return = 0,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,
}

impl Opcode {
    pub fn from_u8(val: u8) -> Option<Self> {
        use Opcode::*;
        match val {
            0 => Some(Return),
            1 => Some(Constant),
            2 => Some(Negate),
            3 => Some(Add),
            4 => Some(Subtract),
            5 => Some(Multiply),
            6 => Some(Divide),
            7 => Some(Nil),
            8 => Some(True),
            9 => Some(False),
            10 => Some(Not),
            11 => Some(Equal),
            12 => Some(Greater),
            13 => Some(Less),
            _ => None,
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
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
        self.code.push(op);
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
        let op = Opcode::from_u8(instr);
        match op {
            Some(
                Opcode::Equal
                | Opcode::Greater
                | Opcode::Less
                | Opcode::Not
                | Opcode::True
                | Opcode::False
                | Opcode::Nil
                | Opcode::Add
                | Opcode::Subtract
                | Opcode::Multiply
                | Opcode::Divide
                | Opcode::Negate
                | Opcode::Return,
            ) => {
                *offset += 1;
                Some(Instruction::Simple(op.unwrap()))
            }
            Some(Opcode::Constant) => {
                let constant_idx = self.code[*offset + 1];
                let constant = self.constants[constant_idx as usize];
                *offset += 2;
                Some(Instruction::Constant(constant))
            }
            otherwise => panic!("Invalid opcode {:?}", otherwise),
        }
    }
}

impl Deref for Chunk {
    type Target = Vec<u8>;

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
            Instruction::Simple(op) => {
                write!(f, "{:?}", op)
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
