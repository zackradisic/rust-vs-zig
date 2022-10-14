use std::ops::{Add, Div, Mul, Neg, Sub};

pub type ValueArray = Vec<Value>;

#[derive(Copy, Clone, Debug)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Nil,
}

impl Value {
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
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
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
