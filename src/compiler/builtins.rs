use crate::builtins;

use super::{
    compile::CompileContext,
    typing::{CompileType, FuncTypeArg},
};

pub fn declare_builtins(ctx: &mut CompileContext) {
    let builtins = {
        let mut gc = ctx.gc.lock().unwrap();
        builtins::Builtins::new(&mut *gc)
    };

    let print_float_type = ctx.trepo.new_func(
        CompileType::Unit,
        vec![FuncTypeArg {
            name: Some(Box::from("f")),
            ctype: CompileType::Float,
            default: None,
        }],
    );
    ctx.scope.declare_function(
        "print_float",
        CompileType::Func(print_float_type),
        builtins.print_float,
    );
    ctx.scope.declare_type("unit", CompileType::Unit);
    ctx.scope.declare_type("bool", CompileType::Bool);
    ctx.scope.declare_type("uint", CompileType::Uint);
    ctx.scope.declare_type("int", CompileType::Int);
    ctx.scope.declare_type("float", CompileType::Float);
}
