use std::ptr::null_mut;

use crate::{
    instr::Instruction,
    rtype::{ObjectRuntimeType, RuntimeType},
};

#[derive(Clone, Copy)]
pub union Value {
    pub b: bool,
    pub u: u64,
    pub i: i64,
    pub f: f64,
    pub o: *mut ObjectValue,
    pub c: *mut CodeValue,
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
            RuntimeType::Code => Value { c: null_mut() },
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

pub struct CodeValue {
    pub alive: bool,
    pub instrs: Box<[Instruction]>,
}

pub struct InterfaceVTValue {
    pub alive: bool,
    pub funcs: Box<[(*mut CodeValue, usize)]>,
}

pub struct ObjectValue {
    pub alive: bool,
    pub fields: Box<[Value]>,
    pub rtype: *mut ObjectRuntimeType,
}

pub fn value_truethy(v: Value, t: RuntimeType) -> bool {
    match t {
        RuntimeType::Bool => unsafe { v.b },
        RuntimeType::Uint => unsafe { v.u != 0 },
        RuntimeType::Int => unsafe { v.i != 0 },
        RuntimeType::Float => unsafe { v.f != 0.0 },
        RuntimeType::Code | RuntimeType::InterfaceVT | RuntimeType::Object => true,
    }
}
