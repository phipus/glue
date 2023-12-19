use std::sync::{Arc, Mutex};

use glue::{
    gc::Collector,
    instr::Instruction,
    rtype::RuntimeType,
    runtime::{FrameType, Function, Thread},
    rvalue::{CodeValue, TypedValue, Value},
};

fn main() {
    let mut print_float_code = CodeValue {
        alive: true,
        instrs: Box::from([Instruction::NativeCall(print_float), Instruction::Ret]),
    };

    let mut print_float_ftype = FrameType {
        alive: true,
        field_types: Box::from([RuntimeType::Float]),
    };

    let mut print_float_func = Function {
        alive: true,
        code: &mut print_float_code as *mut CodeValue,
        ftype: &mut print_float_ftype as *mut FrameType,
        code_offset: 0,
        argc: 1,
        retc: 0,
    };

    let mut main_code = CodeValue {
        alive: true,
        instrs: Box::from([
            Instruction::NativeCall(hello_world),
            Instruction::Float(2.0),
            Instruction::Float(2.0),
            Instruction::Float(4.0),
            Instruction::FloatDivFloat,
            Instruction::FloatAddFloat,
            Instruction::Call(&mut print_float_func as *mut Function),
            Instruction::Ret,
        ]),
    };

    let mut t = Thread::new(Arc::new(Mutex::new(Collector::new())));

    let mut main_ftype = FrameType {
        alive: true,
        field_types: Box::from([]),
    };

    unsafe {
        t.push_frame(
            &mut main_ftype as *mut FrameType,
            &mut main_code as *mut CodeValue,
            0,
        );
        t.eval().unwrap();
    }
}

fn hello_world(_locals: &mut [Value], _eval_stack: &mut Vec<TypedValue>) {
    println!("Hello World");
}

fn print_float(locals: &mut [Value], _eval_stack: &mut Vec<TypedValue>) {
    println!("{}", unsafe { locals[0].f })
}
