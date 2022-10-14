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
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Self::Number(val)
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
