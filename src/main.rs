use std::sync::{Arc, Mutex};

use glue::{
    gc::Collector,
    instr::Instruction,
    rtype::RuntimeType,
    runtime::{FrameType, Function, Thread},
    rvalue::{TypedValue, Value},
};

fn main() {
    let print_float_code = Box::from([Instruction::NativeCall(print_float), Instruction::Ret]);

    let print_float_ftype = FrameType {
        field_types: Box::from([RuntimeType::Float]),
    };

    let mut print_float_func = Function {
        alive: true,
        code: print_float_code,
        ftype: print_float_ftype,
        code_offset: 0,
        argc: 1,
        retc: 0,
    };

    let main_code = Box::from([
        Instruction::NativeCall(hello_world),
        Instruction::Float(2.0),
        Instruction::Float(2.0),
        Instruction::Float(4.0),
        Instruction::FloatDivFloat,
        Instruction::FloatAddFloat,
        Instruction::Call(&mut print_float_func as *mut Function),
        Instruction::Ret,
    ]);

    let mut t = Thread::new(Arc::new(Mutex::new(Collector::new())));

    let main_ftype = FrameType {
        field_types: Box::from([]),
    };

    let mut main_func = Function {
        alive: true,
        code: main_code,
        ftype: main_ftype,
        code_offset: 0,
        argc: 0,
        retc: 0,
    };

    unsafe {
        t.push_function(&mut main_func as *mut Function);
        t.eval().unwrap();
    }
}

fn hello_world(_locals: &mut [Value], _eval_stack: &mut Vec<TypedValue>) {
    println!("Hello World");
}

fn print_float(locals: &mut [Value], _eval_stack: &mut Vec<TypedValue>) {
    println!("{}", unsafe { locals[0].f })
}
