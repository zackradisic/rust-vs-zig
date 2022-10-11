pub mod chunk;
pub mod value;
pub mod vm;

use crate::{
    chunk::{Chunk, Opcode},
    value::Value,
    vm::VM,
};

fn main() {
    let mut chunk = Chunk::new();

    let mut constant_idx = chunk.add_constant(Value(0.0));
    chunk.write(Opcode::CONSTANT, 0);
    chunk.write(constant_idx, 0);

    constant_idx = chunk.add_constant(3.4.into());
    chunk.write(Opcode::CONSTANT, 0);
    chunk.write(constant_idx, 0);

    chunk.write(Opcode::ADD, 0);

    constant_idx = chunk.add_constant(5.6.into());
    chunk.write(Opcode::CONSTANT, 0);
    chunk.write(constant_idx, 0);

    chunk.write(Opcode::DIVIDE, 0);

    chunk.write(Opcode::NEGATE, 0);
    chunk.write(Opcode::RETURN, 0);

    let mut vm = VM::new(chunk);
    vm.run().unwrap();
}
