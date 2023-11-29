use crate::{
    gc::Collector, instr::Instruction, rtype::RuntimeType, runtime::Function, rvalue::Value,
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
    f: fn(&mut [Value], &mut Vec<(Value, RuntimeType)>),
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
    let code = gc.new_code_obj(code);
    let ftype = gc.new_frame_type(frame_type);
    return unsafe { gc.new_function(code, ftype, 0, argc, retc) };
}

fn print_float(locals: &mut [Value], _eval_stack: &mut Vec<(Value, RuntimeType)>) {
    println!("{}", unsafe { locals[0].f })
}
