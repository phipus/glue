use std::sync::Mutex;

use crate::{
    gc::Collector,
    instr::Instruction,
    runtime::{FrameType, Function},
};

use self::{
    builtins::declare_builtins,
    compile::CompileContext,
    error::CompileError,
    parse::{BlockKind, Parse},
    typing::CompileType,
};

pub mod ast;
pub mod builtins;
pub mod compile;
pub mod error;
pub mod parse;
pub mod scan;
pub mod scope;
#[cfg(test)]
mod tests;
pub mod typing;

pub fn compile_file<'a>(
    gc: &Mutex<Collector>,
    input: &'a str,
    filename: &'a str,
) -> Result<*mut Function, CompileError> {
    let mut parse = Parse::new(input, filename);
    let mut block = parse.parse_block(BlockKind::WholeFile)?;

    let mut ctx = CompileContext::new(input, gc);
    declare_builtins(&mut ctx);

    ctx.scope.push_new_scope();

    let mut code = Vec::<Instruction>::new();

    block.check_type(&mut ctx, Some(CompileType::Unit))?;
    let frame_fields = ctx.scope.generate_frame(&ctx.trepo);
    block.compile(&mut ctx, &mut code)?;

    code.push(Instruction::Ret);

    let func = {
        let mut gc = gc.lock().unwrap();
        // SAFETY: we created code and ftype in the same gc, so new_function is safe
        unsafe {
            gc.new_function(
                code.into_boxed_slice(),
                FrameType {
                    field_types: frame_fields.into_boxed_slice(),
                },
                0,
                0,
                0,
            )
        }
    };

    Ok(func)
}
