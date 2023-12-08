use crate::builtins;

use super::{compile::CompileContext, typing::CompileType};

pub fn declare_builtins(ctx: &mut CompileContext) {
    let builtins = {
        let mut gc = ctx.gc.lock().unwrap();
        builtins::Builtins::new(&mut *gc)
    };

    let print_float_type = ctx
        .trepo
        .new_func(CompileType::Unit, vec![CompileType::Float]);
    ctx.scope.declare_function(
        "print_float",
        CompileType::Func(print_float_type),
        builtins.print_float,
    );
}
