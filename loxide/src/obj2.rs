use std::ptr::NonNull;

use compact_str::CompactString;

use crate::{
    chunk::Chunk,
    native_fn::NativeFnKind,
    obj_id::{
        ObjBoundMethodId, ObjClassId, ObjClosureId, ObjFnId, ObjInstanceId, ObjNativeId,
        ObjStringId, ObjUpvalueId,
    },
    table::{ObjHash, Table},
    value2::Value,
};

#[derive(Copy, Clone)]
pub enum ObjIdsEnum {
    String(ObjStringId),
    Fn(ObjFnId),
    Native(ObjNativeId),
    Closure(ObjClosureId),
    Upvalue(ObjUpvalueId),
    Class(ObjClassId),
    Instance(ObjInstanceId),
    BoundMethod(ObjBoundMethodId),
}

pub enum Obj {
    String(ObjString),
    Fn(ObjFunction),
    Native(ObjNative),
    Closure(ObjClosure),
    Upvalue(ObjUpvalue),
    Class(ObjClass),
    Instance(ObjInstance),
    BoundMethod(ObjBoundMethod),
}

pub struct ObjString {
    pub inner: CompactString,
    pub hash: ObjHash,
}

pub struct ObjFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: ObjStringId,
    pub upvalue_count: u8,
}

pub struct ObjNative {
    pub function: NativeFnKind,
}

pub struct ObjClosure {
    pub function: ObjFnId,
    pub upvalues: Vec<ObjUpvalueId>,
}

pub struct ObjClass {
    pub name: ObjStringId,
    pub methods: Table,
}

pub struct ObjInstance {
    pub class: ObjClassId,
    pub fields: Table,
}

pub struct ObjBoundMethod {
    pub receiver: Value,
    pub method: ObjClosureId,
}

// TODO: This needs to be changed
pub struct ObjUpvalue {
    pub location: NonNull<Value>,
    // open upvalues are in a linkedlist so we can traverse that to reuse upvalues
    pub next: *mut ObjUpvalue,
    pub closed: Value,
}
