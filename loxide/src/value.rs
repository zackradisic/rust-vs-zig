use std::{
    fmt::Debug,
    ops::{Add, Div, Mul, Neg, Sub},
    ptr::NonNull,
};

use crate::{
    mem::{Gc, Greystack},
    obj::{
        Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjKind, ObjNative,
        ObjPtrWrapper, ObjString,
    },
};

pub type ValueArray = Vec<Value>;

#[derive(Copy, Clone)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Nil,
    Obj(Gc<Obj>),
}

impl Value {
    // pub fn from_num(num: f64) -> Self {
    //     let bits = num.to_bits();
    //     Self(bits)
    // }

    // pub fn to_num(self) -> f64 {
    //     f64::from_bits(self.0)
    // }

    // #[cfg(not(feature = "nanboxing"))]
    pub fn mark(&self, greystack: &mut Greystack) {
        match *self {
            Value::Obj(obj) => Obj::mark(obj.as_ptr(), greystack),
            _ => (),
        }
    }

    pub fn is_str(&self) -> bool {
        match *self {
            Value::Obj(obj) => obj.kind == ObjKind::Str,
            _ => false,
        }
    }

    pub fn is_fn(&self) -> bool {
        match self {
            Value::Obj(obj) => obj.kind == ObjKind::Fn,
            _ => false,
        }
    }

    pub fn is_native(&self) -> bool {
        match self {
            Value::Obj(obj) => obj.kind == ObjKind::Native,
            _ => false,
        }
    }

    pub fn as_bound_method(&self) -> Option<Gc<ObjBoundMethod>> {
        match self {
            Value::Obj(obj) if obj.kind == ObjKind::BoundMethod => Some(obj.cast()),
            _ => None,
        }
    }

    pub fn as_instance_fn(&self) -> Option<Gc<ObjInstance>> {
        match *self {
            Value::Obj(obj) if obj.kind == ObjKind::Instance => Some(obj.cast()),
            _ => None,
        }
    }

    pub fn as_class(&self) -> Option<Gc<ObjClass>> {
        match *self {
            Value::Obj(obj) if obj.kind == ObjKind::Class => Some(obj.cast()),
            _ => None,
        }
    }

    pub fn as_fn(&self) -> Option<Gc<ObjFunction>> {
        match *self {
            Value::Obj(obj) if obj.kind == ObjKind::Fn => Some(obj.cast()),
            _ => None,
        }
    }

    pub fn as_obj_native(&self) -> Option<Gc<ObjNative>> {
        match *self {
            Value::Obj(obj) if obj.kind == ObjKind::Native => Some(obj.cast()),
            _ => None,
        }
    }

    pub fn as_obj_closure(&self) -> Option<Gc<ObjClosure>> {
        match self {
            Value::Obj(obj) if obj.kind == ObjKind::Closure => Some(obj.cast()),
            _ => None,
        }
    }

    pub fn as_obj_str(&self) -> Option<Gc<ObjString>> {
        match *self {
            Value::Obj(obj) if obj.kind == ObjKind::Str => Some(obj.cast()),
            _ => None,
        }
    }

    pub fn as_str<'a>(&'a self) -> Option<&'a str> {
        let noob = self.as_obj_str()?;
        Some(unsafe { (*noob.as_ptr()).as_str() })
    }

    #[inline]
    pub fn is_nil(self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn is_falsey(self) -> bool {
        match self {
            Value::Bool(b) => !b,
            Value::Nil => true,
            _ => false,
        }
    }

    pub fn gt_owned(self, other: Self) -> Value {
        self.gt(&other).into()
    }

    pub fn lt_owned(self, other: Self) -> Value {
        self.lt(&other).into()
    }

    fn objs_eq(a: *mut Obj, b: *mut Obj) -> bool {
        a == b
        // unsafe {
        // if (*a).kind != (*b).kind {
        //     return false;
        // }

        // match (*a).kind {
        //     ObjKind::Str => {
        //         let a_str = (&*(a as *mut ObjString)).as_str();
        //         let b_str = (&*(b as *mut ObjString)).as_str();

        //         a_str == b_str
        //     }
        // }
        // }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::Nil => write!(f, "Nil"),
            Self::Obj(arg0) => {
                write!(f, "{:?}", ObjPtrWrapper(arg0.as_ptr()))
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Obj(a), Self::Obj(b)) => Self::objs_eq(a.as_ptr(), b.as_ptr()),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Self::Number(val)
    }
}

impl From<bool> for Value {
    fn from(val: bool) -> Self {
        Self::Bool(val)
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        match self {
            Value::Bool(b) => Value::Bool(!b),
            Value::Number(num) => Value::Number(num),
            _ => unreachable!(),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            _ => unreachable!(),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
            _ => unreachable!(),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
            _ => unreachable!(),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
            _ => unreachable!(),
        }
    }
}
