use std::{sync::Mutex, task::Context};

use crate::{gc::Collector, runtime::Function, instr::Instruction};

use self::{parse::{Parse, BlockKind}, error::CompileError, compile::CompileContext, typing::CompileType, builtins::declare_builtins};

pub mod ast;
pub mod builtins;
pub mod compile;
pub mod error;
pub mod parse;
pub mod scan;
pub mod typing;

pub fn compile_file<'a>(gc: &Mutex<Collector>, input: &'a str, filename: &'a str) -> Result<*mut Function, CompileError> {
    let mut parse = Parse::new(input, filename);
    let mut block = parse.parse_block(BlockKind::WholeFile)?;

    let mut builtin_ctx = CompileContext::new("", gc);
    declare_builtins(&mut builtin_ctx);

    let mut ctx = CompileContext::new(input, gc);
    ctx.scope.parent = Some(&mut builtin_ctx.scope);
    
    let mut code = Vec::<Instruction>::new();

    block.check_type(&mut ctx, Some(CompileType::Unit))?;
    let frame_fields = ctx.scope.generate_frame(&ctx.trepo);
    block.compile(&mut ctx, &mut code)?;

    let func = {
        let mut gc = gc.lock().unwrap();
        let code = gc.new_code_obj(code.into_boxed_slice());
        let ftype = gc.new_frame_type(frame_fields.into_boxed_slice());
        // SAFETY: we created code and ftype in the same gc, so new_function is safe
        unsafe {gc.new_function(code, ftype, 0, 0, 0)}
    };

    Ok(func)
}
