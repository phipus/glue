use std::sync::Mutex;

use crate::{gc::Collector, instr::Instruction, rtype::RuntimeType, runtime::Function};

use super::{
    ast::{
        AssignmentNode, BinaryOpNode, BinaryOperand, BlockNode, BoolNode, CallNode,
        DeclarationNode, FieldNode, FloatNode, FuncStmtNode, IfElseNode, IntNode, VariableNode,
    },
    error::CompileError,
    scan::{token, Token},
    scope::{FuncScope, SymbolLocation},
    typing::{CompileType, FuncTypeArg, TypeRepo},
};

type Result<T> = std::result::Result<T, CompileError>;

pub trait NodeValueold {
    fn start_token(&self) -> Token;
    fn end_token(&self) -> Token;
    fn check_type(
        &mut self,
        ctx: &mut CompileContext,
        type_hint: Option<CompileType>,
    ) -> Result<CompileType>;

    fn compile(&self, ctx: &mut CompileContext, code: &mut Vec<Instruction>) -> Result<()>;
}

#[derive(Clone)]
pub enum Node {
    Bool(BoolNode),
    Int(IntNode),
    Float(FloatNode),
    BinaryOp(Box<BinaryOpNode>),
    Declaration(Box<DeclarationNode>),
    Variable(VariableNode),
    Assignment(Box<AssignmentNode>),
    Block(BlockNode),
    Call(Box<CallNode>),
    IfElse(Box<IfElseNode>),
    Field(Box<FieldNode>),
    FuncStmt(Box<FuncStmtNode>),
}

impl Node {
    pub fn start_token(&self) -> Token {
        match self {
            Self::Bool(n) => n.token,
            Self::Int(n) => n.token,
            Self::Float(n) => n.token,
            Self::BinaryOp(n) => n.initial.start_token(),
            Self::Declaration(n) => n.start,
            Self::Variable(n) => n.token,
            Self::Assignment(n) => n.left.start_token(),
            Self::Block(n) => match n.exprs.first() {
                Some(first) => first.start_token(),
                None => Token {
                    kind: token::EOF,
                    start: 0,
                    end: 0,
                    line_start: 0,
                    line_end: 0,
                    col_start: 0,
                    col_end: 0,
                },
            },
            Self::Call(n) => n.value.end_token(),
            Self::IfElse(n) => n.start,
            Self::Field(n) => n.expr.start_token(),
            Self::FuncStmt(n) => n.start,
        }
    }

    pub fn end_token(&self) -> Token {
        match self {
            Self::Bool(n) => n.token,
            Self::Int(n) => n.token,
            Self::Float(n) => n.token,
            Self::BinaryOp(n) => match n.ops.last() {
                None => n.initial.end_token(),
                Some((_, n)) => n.end_token(),
            },
            Self::Declaration(n) => n.end,
            Self::Variable(n) => n.token,
            Self::Assignment(n) => n.right.end_token(),
            Self::Block(n) => match n.exprs.last() {
                Some(last) => last.end_token(),
                None => Token {
                    kind: token::EOF,
                    start: 0,
                    end: 0,
                    line_start: 0,
                    line_end: 0,
                    col_start: 0,
                    col_end: 0,
                },
            },
            Self::Call(n) => n.end,
            Self::IfElse(n) => match &n.alt {
                Some(alt) => alt.end_token(),
                None => n.exprs.last().unwrap().1.end_token(),
            },
            Self::Field(n) => n.field,
            Self::FuncStmt(n) => n.body.end_token(),
        }
    }

    pub fn check_type(
        &mut self,
        ctx: &mut CompileContext,
        type_hint: Option<CompileType>,
    ) -> Result<CompileType> {
        match self {
            Self::Bool(_) => {
                if let Some(type_hint) = type_hint {
                    if type_hint != CompileType::Bool {
                        return Err(CompileError::TypeError(format!(
                            "can not convert bool to {:?}",
                            type_hint
                        )));
                    }
                }

                Ok(CompileType::Bool)
            }
            Self::Int(n) => match type_hint {
                Some(type_hint) => match type_hint {
                    CompileType::Uint => {
                        n.as_uint = true;
                        Ok(CompileType::Uint)
                    }
                    CompileType::Int => Ok(CompileType::Int),
                    CompileType::Float => {
                        n.as_float = true;
                        Ok(CompileType::Float)
                    }
                    _ => Err(CompileError::TypeError(format!(
                        "can not convert untyped int to {:?}",
                        type_hint
                    ))),
                },

                None => Ok(CompileType::Int),
            },
            Self::Float(_) => match type_hint {
                Some(type_hint) => match type_hint {
                    CompileType::Float => Ok(CompileType::Float),
                    _ => Err(CompileError::TypeError(format!(
                        "can not convert float to {:?}",
                        type_hint
                    ))),
                },
                None => Ok(CompileType::Float),
            },
            Self::BinaryOp(n) => {
                let expr_type = n.initial.check_type(ctx, type_hint)?;
                n.expr_type = expr_type;

                for (_, node) in n.ops.iter_mut() {
                    node.check_type(ctx, Some(expr_type))?;
                }

                Ok(expr_type)
            }
            Self::Declaration(n) => {
                if let Some(hint) = type_hint {
                    match hint {
                        CompileType::Unit => (),
                        _ => {
                            return Err(CompileError::TypeError(format!(
                                "can not assign use declaration as {:?}",
                                hint
                            )))
                        }
                    }
                }

                if let Some(assignment) = &mut n.assignment {
                    let ctype = assignment.check_type(ctx, n.ctype)?;
                    match &n.ctype {
                        None => n.ctype = Some(ctype),
                        Some(_) => (), // ctype must be same
                    }
                }
                if n.ctype.is_none() {
                    return Err(CompileError::TypeError(format!("can not infer type")));
                }

                let symbol = ctx.scope.declare_variable(&n.name, n.ctype.unwrap());
                n.symbol = Some(symbol);

                Ok(CompileType::Unit)
            }
            Self::Variable(n) => {
                let name = &ctx.input[n.token.start..n.token.end];

                let sym_id = match ctx.scope.lookup(name) {
                    None => {
                        return Err(CompileError::NameError(format!(
                            "variable {} not defined",
                            name
                        )))
                    }
                    Some((_, sym)) => sym,
                };

                n.symbol = Some(sym_id);
                let sym = ctx.scope.get_symbol(sym_id);

                match type_hint {
                    None => Ok(sym.ctype),
                    Some(tye_hint) => {
                        if tye_hint != sym.ctype {
                            return Err(CompileError::TypeError(format!(
                                "exected type {:?} but variable {} is {:?}",
                                tye_hint, name, sym.ctype
                            )));
                        }
                        Ok(sym.ctype)
                    }
                }
            }
            Self::Assignment(n) => match &n.left {
                Node::Variable(left) => {
                    let sym_id = left.symbol.unwrap();
                    let ctype = ctx.scope.get_symbol(sym_id).ctype;
                    let actual_type = n.right.check_type(ctx, Some(ctype))?;
                    if actual_type != ctype {
                        // this should not happen, type_hint should be respected in any case
                        return Err(CompileError::TypeError(format!(
                            "assignment type missmatch"
                        )));
                    }

                    Ok(ctype)
                }

                _ => Err(CompileError::TypeError(format!(
                    "{:?} is not assignable",
                    n.left.node_kind_str()
                ))),
            },
            Self::Block(n) => {
                if let Some(type_hint) = type_hint {
                    if type_hint != CompileType::Unit {
                        return Err(CompileError::TypeError(format!(
                            "can not convert block to {:?}",
                            type_hint
                        )));
                    }
                }

                for child in &mut n.exprs {
                    let ctype = child.check_type(ctx, None)?;
                    n.node_types.push(ctype);
                }

                Ok(CompileType::Unit)
            }
            Self::Call(n) => {
                let typ = n.value.check_type(ctx, None)?;
                let funcid = match typ {
                    CompileType::Func(id) => id,
                    _ => return Err(CompileError::TypeError(format!("can not call {:?}", typ))),
                };

                let ftype = ctx.trepo.get_func(funcid).clone();
                if let Some(type_hint) = type_hint {
                    if type_hint != ftype.returns {
                        return Err(CompileError::TypeError(format!(
                            "can not convert {:?} to {:?}",
                            ftype.returns, type_hint
                        )));
                    }
                }

                if n.args.len() != ftype.args.len() {
                    return Err(CompileError::TypeError(format!(
                        "expected {} arguments but found {}",
                        ftype.args.len(),
                        n.args.len()
                    )));
                }

                for (i, arg) in n.args.iter_mut().enumerate() {
                    let argtype = arg.check_type(ctx, Some(ftype.args[i].ctype))?;
                    if argtype != ftype.args[i].ctype {
                        return Err(CompileError::TypeError(format!("unexpected argument type")));
                    }
                }

                Ok(ftype.returns)
            }
            Self::IfElse(n) => {
                if let Some(type_hint) = type_hint {
                    if type_hint != CompileType::Unit {
                        return Err(CompileError::TypeError(format!(
                            "can not convert unit to {:?}",
                            type_hint
                        )));
                    }
                }

                for (expr, body) in &mut n.exprs {
                    expr.check_type(ctx, Some(CompileType::Bool))?;
                    body.check_type(ctx, Some(CompileType::Unit))?;
                }

                if let Some(alt) = &mut n.alt {
                    alt.check_type(ctx, Some(CompileType::Unit))?;
                }

                Ok(CompileType::Unit)
            }
            Self::Field(n) => {
                let ctype = n.expr.check_type(ctx, None)?;
                match ctype {
                    CompileType::Object(obj_id) => {
                        let otype = ctx.trepo.get_object(obj_id);
                        let field_name = &ctx.input[n.field.start..n.field.end];
                        match otype.get_field_by_name(field_name) {
                            None => {
                                return Err(CompileError::TypeError(format!(
                                    "object has no field {}",
                                    field_name
                                )))
                            }
                            Some(field) => {
                                if let Some(type_hint) = type_hint {
                                    if type_hint != field.ctype {
                                        return Err(CompileError::TypeError(format!(
                                            "can not convert field {} ({:?}) to {:?}",
                                            field_name, field.ctype, type_hint
                                        )));
                                    }
                                }
                                n.info = Some(field);

                                Ok(field.ctype)
                            }
                        }
                    }
                    _ => {
                        return Err(CompileError::TypeError(format!(
                            "{:?} has not properties",
                            ctype
                        )))
                    }
                }
            }
            Self::FuncStmt(n) => {
                if let Some(type_hint) = type_hint {
                    if type_hint != CompileType::Unit {
                        return Err(CompileError::TypeError(format!(
                            "can not use function as {:?}",
                            type_hint
                        )));
                    }
                }

                // ensure all argument types have been specified
                let mut args = Vec::new();
                for (argname, argtype) in &n.args {
                    let argname_str = &ctx.input[argname.start..argname.end];
                    args.push(FuncTypeArg {
                        name: Some(Box::from(argname_str)),
                        ctype: match argtype {
                            Some(argtype) => argtype.eval(ctx)?,
                            None => {
                                return Err(CompileError::TypeError(format!(
                                    "can not infer type of argument {}",
                                    argname_str
                                )))
                            }
                        },
                        default: None,
                    })
                }

                let rtype = match &n.returns {
                    Some(rtype) => rtype.eval(ctx)?,
                    None => panic!("return type inference not implemented"),
                };

                ctx.scope.push_new_scope();
                n.body.check_type(ctx, Some(CompileType::Unit))?;
                let mut scope = ctx.scope.pop_scope().unwrap();

                let argc = args
                    .iter()
                    .map(|arg| arg.ctype.get_size(&ctx.trepo))
                    .reduce(|a, b| a + b)
                    .unwrap_or(0);
                let retc = rtype.get_size(&ctx.trepo);

                let ftype = scope.generate_frame(&ctx.trepo);

                let (code, func) = unsafe {
                    let mut gc = ctx.gc.lock().unwrap();
                    let code = gc.new_code_obj(Box::new([]));
                    let ftype = gc.new_frame_type(ftype.into_boxed_slice());
                    let func = gc.new_function(code, ftype, 0, argc, retc);
                    (code, func)
                };

                let ctype = CompileType::Func(ctx.trepo.new_func(rtype, args));
                ctx.scope
                    .declare_function(&ctx.input[n.name.start..n.name.end], ctype, func);

                n.code = Some(code);
                n.scope = Some(scope);
                Ok(ctype)
            }
        }
    }

    pub fn compile(&mut self, ctx: &mut CompileContext, code: &mut Vec<Instruction>) -> Result<()> {
        match self {
            Self::Bool(n) => {
                let b = match n.token.kind {
                    token::TRUE => true,
                    token::FALSE => false,
                    _ => panic!("BoolNode with invalid token"),
                };
                code.push(Instruction::Bool(b));
                Ok(())
            }
            Self::Int(n) => {
                let token_value = &ctx.input[n.token.start..n.token.end];
                if n.as_uint {
                    return match token_value.parse::<u64>() {
                        Ok(value) => {
                            code.push(Instruction::Uint(value));
                            Ok(())
                        }
                        Err(_) => Err(CompileError::TypeError(format!(
                            "can not use {} ase uint",
                            token_value
                        ))),
                    };
                }
                if n.as_float {
                    return match token_value.parse::<f64>() {
                        Ok(value) => {
                            code.push(Instruction::Float(value));
                            Ok(())
                        }
                        Err(_) => Err(CompileError::TypeError(format!(
                            "can not use {} as float",
                            token_value
                        ))),
                    };
                }

                match token_value.parse::<i64>() {
                    Ok(value) => {
                        code.push(Instruction::Int(value));
                        Ok(())
                    }
                    Err(_) => Err(CompileError::TypeError(format!(
                        "can not use {} as int",
                        token_value
                    ))),
                }
            }
            Self::Float(n) => {
                let token_value = &ctx.input[n.token.start..n.token.end];
                match token_value.parse::<f64>() {
                    Ok(value) => {
                        code.push(Instruction::Float(value));
                        Ok(())
                    }
                    Err(_) => Err(CompileError::TypeError(format!(
                        "can not use {} as float",
                        token_value
                    ))),
                }
            }
            Self::BinaryOp(n) => {
                n.initial.compile(ctx, code)?;
                for (op, node) in &mut n.ops {
                    node.compile(ctx, code)?;
                    code.push(binary_operand_to_instr(&n.expr_type, &op)?);
                }

                Ok(())
            }
            Self::Declaration(n) => {
                if let Some(assignment) = &mut n.assignment {
                    assignment.compile(ctx, code)?;
                    match ctx.scope.get_location(n.symbol.unwrap()) {
                        SymbolLocation::Local { offset } => {
                            let type_size = ctx
                                .scope
                                .get_symbol(n.symbol.unwrap())
                                .ctype
                                .get_size(&ctx.trepo);
                            for i in 0..type_size {
                                code.push(Instruction::PopLocal(*offset + i))
                            }
                        }

                        SymbolLocation::Function { ptr: _ } => {
                            return Err(CompileError::TypeError(format!(
                                "can not declare function as variable"
                            )))
                        }

                        SymbolLocation::Type => {
                            return Err(CompileError::TypeError(format!(
                                "can not declare type as variable"
                            )))
                        }
                    }
                }

                Ok(())
            }
            Self::Variable(n) => {
                let sym = ctx.scope.get_symbol(n.symbol.unwrap());
                let mut fields = Vec::<RuntimeType>::new();
                sym.ctype.fields(&mut fields, &ctx.trepo);

                match ctx.scope.get_location(n.symbol.unwrap()) {
                    SymbolLocation::Local { offset } => {
                        for (i, field) in fields.into_iter().enumerate() {
                            code.push(Instruction::PushLocal(*offset + i as u32, field));
                        }
                    }
                    SymbolLocation::Function { ptr: _ } => {
                        return Err(CompileError::TypeError(format!(
                            "can not evaluate function (call required)"
                        )))
                    }
                    SymbolLocation::Type => {
                        return Err(CompileError::TypeError(format!("can not evaluate type")))
                    }
                }
                Ok(())
            }
            Self::Assignment(n) => match &n.left {
                Node::Variable(left) => {
                    let type_size = ctx
                        .scope
                        .get_symbol(left.symbol.unwrap())
                        .ctype
                        .get_size(&ctx.trepo);
                    n.right.compile(ctx, code)?;

                    match ctx.scope.get_location(left.symbol.unwrap()) {
                        SymbolLocation::Local { offset } => {
                            for i in 0..type_size {
                                code.push(Instruction::PopLocal(offset + i))
                            }
                        }
                        SymbolLocation::Function { ptr: _ } => {
                            return Err(CompileError::TypeError(format!(
                                "can not assign to function"
                            )))
                        }
                        SymbolLocation::Type => {
                            return Err(CompileError::TypeError(format!("can not assign type")))
                        }
                    }

                    Ok(())
                }

                _ => panic!("can not assign {}", n.left.node_kind_str()),
            },
            Self::Block(n) => {
                for (i, child) in n.exprs.iter_mut().enumerate() {
                    child.compile(ctx, code)?;
                    let type_size = n.node_types[i].get_size(&ctx.trepo);
                    for _ in 0..type_size {
                        code.push(Instruction::PopDiscard);
                    }
                }

                Ok(())
            }
            Self::Call(n) => match &n.value {
                Node::Variable(variable) => {
                    let sym_id = variable.symbol.unwrap();

                    match ctx.scope.get_location(sym_id) {
                        SymbolLocation::Function { ptr } => {
                            let ptr = *ptr;
                            for arg in &mut n.args {
                                arg.compile(ctx, code)?;
                            }
                            code.push(Instruction::Call(ptr));

                            Ok(())
                        }
                        SymbolLocation::Local { offset: _ } => {
                            panic!("function pointers not implemented")
                        }
                        SymbolLocation::Type => {
                            Err(CompileError::TypeError(format!("type is not callable")))
                        }
                    }
                }
                _ => panic!("function pointers not implemented"),
            },
            Self::IfElse(n) => {
                struct Jump {
                    pos: usize,
                    to: usize,
                }

                let mut jumps = Vec::new();
                let mut jump_locactions = vec![0usize];

                for (expr, body) in &mut n.exprs {
                    expr.compile(ctx, code)?;
                    jumps.push(Jump {
                        pos: code.len(),
                        to: jump_locactions.len(),
                    });
                    code.push(Instruction::JumpFalse(0));
                    body.compile(ctx, code)?;
                    jumps.push(Jump {
                        pos: code.len(),
                        to: 0,
                    });
                    code.push(Instruction::Jump(0));
                    jump_locactions.push(code.len());
                }

                if let Some(alt) = &mut n.alt {
                    alt.compile(ctx, code)?;
                    jump_locactions.push(code.len());
                }

                jump_locactions[0] = code.len();

                // replace labels
                for jmp in jumps {
                    let offset = jump_locactions[jmp.to] as isize - jmp.pos as isize - 1;
                    code[jmp.pos] = match code[jmp.pos] {
                        Instruction::Jump(_) => Instruction::Jump(offset),
                        Instruction::JumpTrue(_) => Instruction::JumpTrue(offset),
                        Instruction::JumpFalse(_) => Instruction::JumpFalse(offset),
                        instr => instr,
                    };
                }

                Ok(())
            }
            Self::Field(n) => {
                let mut fields = Vec::new();
                let field = n.info.unwrap();
                field.ctype.fields(&mut fields, &ctx.trepo);

                if fields.len() > 0 {
                    code.push(Instruction::GetField(field.offset, fields[0]));
                }
                for i in 1..fields.len() {
                    code.push(Instruction::GetNextField(
                        field.offset + i as u32,
                        fields[i],
                    ));
                }

                Ok(())
            }

            Self::FuncStmt(n) => {
                ctx.scope.push_scope(n.scope.take().unwrap());
                let mut code = Vec::new();
                n.body.compile(ctx, &mut code)?;
                n.scope = ctx.scope.pop_scope();

                unsafe {
                    (*n.code.unwrap()).instrs = code.into_boxed_slice();
                }

                Ok(())
            }
        }
    }

    pub fn node_kind_str(&self) -> &'static str {
        match self {
            Self::Bool(_) => "bool",
            Self::Int(_) => "int",
            Self::Float(_) => "float",
            Self::BinaryOp(_) => "binary op",
            Self::Declaration(_) => "declaration",
            Self::Variable(_) => "variable",
            Self::Assignment(_) => "assignment",
            Self::Block(_) => "block",
            Self::Call(_) => "call",
            Self::IfElse(_) => "if branch",
            Self::Field(_) => "field access",
            Self::FuncStmt(_) => "function stmt",
        }
    }
}

fn binary_operand_to_instr(t: &CompileType, op: &BinaryOperand) -> Result<Instruction> {
    match op {
        BinaryOperand::Add => match t {
            CompileType::Uint => return Ok(Instruction::UintAddUint),
            CompileType::Int => return Ok(Instruction::IntAddInt),
            CompileType::Float => return Ok(Instruction::FloatAddFloat),
            _ => (),
        },
        BinaryOperand::Sub => match t {
            CompileType::Uint => return Ok(Instruction::UintSubUint),
            CompileType::Int => return Ok(Instruction::IntSubInt),
            CompileType::Float => return Ok(Instruction::FloatSubFloat),
            _ => (),
        },
        BinaryOperand::Mul => match t {
            CompileType::Uint => return Ok(Instruction::UintMulUint),
            CompileType::Int => return Ok(Instruction::IntMulInt),
            CompileType::Float => return Ok(Instruction::FloatMulFloat),
            _ => (),
        },
        BinaryOperand::Div => match t {
            CompileType::Uint => return Ok(Instruction::UintDivUint),
            CompileType::Int => return Ok(Instruction::IntDivInt),
            CompileType::Float => return Ok(Instruction::FloatDivFloat),
            _ => (),
        },
    }

    Err(CompileError::TypeError(format!(
        "{:?} is not implemented for {:?}",
        t, op
    )))
}

pub struct CompileContext<'a> {
    pub input: &'a str,
    pub scope: FuncScope,
    pub functions: Vec<*mut Function>,
    pub gc: &'a Mutex<Collector>,
    pub trepo: TypeRepo,
}

impl<'a> CompileContext<'a> {
    pub fn new(input: &'a str, gc: &'a Mutex<Collector>) -> Self {
        Self {
            input,
            scope: FuncScope::new(),
            functions: Vec::new(),
            gc,
            trepo: TypeRepo::new(),
        }
    }
}
