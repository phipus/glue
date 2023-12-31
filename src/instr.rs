use crate::{
    rtype::{ObjectRuntimeType, RuntimeType},
    runtime::Function,
    rvalue::{InterfaceVTValue, TypedValue, Value},
};

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    Bool(bool),
    Uint(u64),
    Int(i64),
    Float(f64),
    Object(*mut ObjectRuntimeType),
    InterfaceVT(*mut InterfaceVTValue),
    PushLocal(u32, RuntimeType),
    PopLocal(u32),
    GetField(u32, RuntimeType),
    GetNextField(u32, RuntimeType),
    SetField(u32),
    PopDiscard,
    Call(*mut Function),
    NativeCall(fn(locals: &mut [Value], eval_stack: &mut Vec<TypedValue>)),
    Ret,

    BoolAndBool,
    BoolOrBool,
    BoolEqBool,
    BoolNeBool,

    UintAddUint,
    UintSubUint,
    UintMulUint,
    UintDivUint,
    UintGtUint,
    UintGeUint,
    UintEqUint,
    UintNeUint,
    UintLeUint,
    UintLtUint,

    IntAddInt,
    IntSubInt,
    IntMulInt,
    IntDivInt,
    IntGtInt,
    IntGeInt,
    IntEqInt,
    IntNeInt,
    IntLeInt,
    IntLtInt,

    FloatAddFloat,
    FloatSubFloat,
    FloatMulFloat,
    FloatDivFloat,
    FloatGtFloat,
    FloatGeFloat,
    FloatEqFloat,
    FloatNeFloat,
    FloatLeFloat,
    FloatLtFloat,

    UintToInt,
    UintToFloat,
    IntToUint,
    IntToFloat,
    FloatToUint,
    FloatToInt,

    NegInt,
    NegFloat,
    NegBool,

    Jump(isize),
    JumpTrue(isize),
    JumpFalse(isize),
}
