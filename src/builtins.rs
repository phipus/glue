use crate::{
    gc::Collector,
    instr::Instruction,
    rtype::RuntimeType,
    runtime::{FrameType, Function},
    rvalue::{TypedValue, Value},
};

pub struct Builtins {
    pub print_float: *mut Function,
}

impl Builtins {
    pub fn new(gc: &mut Collector) -> Self {
        Self {
            print_float: create_native_func(gc, Box::new([RuntimeType::Float]), 0, print_float),
        }
    }
}

fn create_native_func(
    gc: &mut Collector,
    args: Box<[RuntimeType]>,
    retc: u32,
    f: fn(&mut [Value], &mut Vec<TypedValue>),
) -> *mut Function {
    create_func(
        gc,
        args.len() as u32,
        retc,
        args,
        Box::new([Instruction::NativeCall(f), Instruction::Ret]),
    )
}

fn create_func(
    gc: &mut Collector,
    argc: u32,
    retc: u32,
    frame_type: Box<[RuntimeType]>,
    code: Box<[Instruction]>,
) -> *mut Function {
    return unsafe {
        gc.new_function(
            code,
            FrameType {
                field_types: frame_type,
            },
            0,
            argc,
            retc,
        )
    };
}

fn print_float(locals: &mut [Value], _eval_stack: &mut Vec<TypedValue>) {
    println!("{}", unsafe { locals[0].f })
}
