use std::{fmt::Debug, ptr::NonNull};

use crate::value::Value;

pub type NativeFn = fn(&[Value]) -> Value;

#[derive(Clone, Copy)]
pub enum NativeFnKind {
    Clock,
    Dummy,
    Custom(NativeFn),
}

impl Debug for NativeFnKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Clock => write!(f, "Clock"),
            Self::Dummy => write!(f, "Dummy"),
            Self::Custom(arg0) => {
                let fn_pointer: *const NativeFn = arg0;
                f.debug_tuple("Custom").field(&fn_pointer).finish()
            }
        }
    }
}

impl NativeFnKind {
    pub fn call(&self, values: &[Value]) -> Value {
        match self {
            NativeFnKind::Clock => Self::call_clock(values),
            NativeFnKind::Dummy => Self::call_dummy(values),
            NativeFnKind::Custom(native_fn) => native_fn(values),
        }
    }

    fn call_clock(_values: &[Value]) -> Value {
        Value::Number(420.0)
    }

    fn call_dummy(_values: &[Value]) -> Value {
        Value::Number(420.0)
    }
}
