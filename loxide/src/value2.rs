use crate::obj_id::{
    ObjBoundMethodId, ObjClassId, ObjClosureId, ObjFnId, ObjInstanceId, ObjNativeId, ObjStringId,
    ObjUpvalueId,
};

#[derive(Copy, Clone)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Nil,
    ObjString(ObjStringId),
    ObjFn(ObjFnId),
    ObjNative(ObjNativeId),
    ObjClosure(ObjClosureId),
    ObjUpvalue(ObjUpvalueId),
    ObjClass(ObjClassId),
    ObjInstance(ObjInstanceId),
    ObjBoundMethod(ObjBoundMethodId),
}

impl Value {
    // pub fn mark(&self, greystack: &mut Gr
}
