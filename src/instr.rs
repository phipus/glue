use crate::{
    rtype::{ObjectRuntimeType, RuntimeType},
    runtime::Function,
    rvalue::{CodeValue, InterfaceVTValue, Value},
};

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    Bool(bool),
    Uint(u64),
    Int(i64),
    Float(f64),
    Object(*mut ObjectRuntimeType),
    InterfaceVT(*mut InterfaceVTValue),
    Code(*mut CodeValue),
    PushLocal(u32, RuntimeType),
    PopLocal(u32),
    GetField(u32, RuntimeType),
    GetNextField(u32, RuntimeType),
    SetField(u32),
    PopDiscard,
    Call(*mut Function),
    NativeCall(fn(locals: &mut [Value], eval_stack: &mut Vec<(Value, RuntimeType)>)),
    Ret,

    BoolAndBool,
    BoolOrBool,
    UintAddUint,
    UintSubUint,
    UintMulUint,
    UintDivUint,
    IntAddInt,
    IntSubInt,
    IntMulInt,
    IntDivInt,
    FloatAddFloat,
    FloatSubFloat,
    FloatMulFloat,
    FloatDivFloat,

    UintToInt,
    UintToFloat,
    IntToUint,
    IntToFloat,
    FloatToUint,
    FloatToInt,

    Jump(isize),
    JumpTrue(isize),
    JumpFalse(isize),
}
