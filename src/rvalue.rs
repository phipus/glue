use std::ptr::null_mut;

use crate::rtype::{ObjectRuntimeType, RuntimeType};

#[derive(Clone, Copy)]
pub union Value {
    pub b: bool,
    pub u: u64,
    pub i: i64,
    pub f: f64,
    pub o: *mut ObjectValue,
    pub v: *mut InterfaceVTValue,
}

impl Value {
    pub fn default(rtype: RuntimeType) -> Self {
        match rtype {
            RuntimeType::Bool => Value { b: false },
            RuntimeType::Uint => Value { u: 0 },
            RuntimeType::Int => Value { i: 0 },
            RuntimeType::Float => Value { f: 0.0 },
            RuntimeType::Object => Value { o: null_mut() },
            RuntimeType::InterfaceVT => Value { v: null_mut() },
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value { b: value }
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Value { u: value }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value { i: value }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value { f: value }
    }
}

pub struct InterfaceVTValue {
    pub alive: bool,
    pub func_indice: Box<[usize]>,
}

pub struct ObjectValue {
    pub alive: bool,
    pub fields: Box<[Value]>,
    pub rtype: *mut ObjectRuntimeType,
}

#[derive(Clone, Copy)]
pub struct TypedValue {
    pub value: Value,
    pub rtype: RuntimeType,
}

impl From<bool> for TypedValue {
    fn from(value: bool) -> Self {
        Self {
            value: value.into(),
            rtype: RuntimeType::Bool,
        }
    }
}

impl From<u64> for TypedValue {
    fn from(value: u64) -> Self {
        Self {
            value: value.into(),
            rtype: RuntimeType::Uint,
        }
    }
}

impl From<i64> for TypedValue {
    fn from(value: i64) -> Self {
        Self {
            value: value.into(),
            rtype: RuntimeType::Int,
        }
    }
}

impl From<f64> for TypedValue {
    fn from(value: f64) -> Self {
        Self {
            value: value.into(),
            rtype: RuntimeType::Int,
        }
    }
}
