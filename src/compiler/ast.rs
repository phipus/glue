use crate::{runtime::Function, rvalue::CodeValue};

use super::{
    compile::{CompileContext, Node},
    error::CompileError,
    scan::Token,
    scope::{FuncScope, SymbolID, SymbolKind},
    typing::{CompileType, FieldInfo, FuncTypeArg},
};

#[derive(Clone)]
pub struct BoolNode {
    pub token: Token,
}

impl BoolNode {
    pub const fn new(token: Token) -> Self {
        Self { token }
    }
}

#[derive(Clone)]
pub struct IntNode {
    pub token: Token,
    pub as_uint: bool,
    pub as_float: bool,
}

impl IntNode {
    pub const fn new(token: Token) -> Self {
        Self {
            token,
            as_uint: false,
            as_float: false,
        }
    }
}

#[derive(Clone)]
pub struct FloatNode {
    pub token: Token,
}

impl FloatNode {
    pub const fn new(token: Token) -> Self {
        Self { token }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOperand {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone)]
pub struct BinaryOpNode {
    pub initial: Node,
    pub expr_type: CompileType,
    pub ops: Vec<(BinaryOperand, Node)>,
}

#[derive(Clone)]
pub struct DeclarationNode {
    pub start: Token,
    pub end: Token,
    pub name: Box<str>,
    pub assignment: Option<Node>,
    pub ctype_name: Option<TypeNode>,
    pub ctype: Option<CompileType>,
    pub symbol: Option<SymbolID>,
}

#[derive(Clone)]
pub struct AssignmentNode {
    pub left: Node,
    pub right: Node,
}

impl AssignmentNode {
    pub const fn new(left: Node, right: Node) -> Self {
        Self { left, right }
    }
}

#[derive(Clone)]
pub struct VariableNode {
    pub token: Token,
    pub symbol: Option<SymbolID>,
}

impl VariableNode {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            symbol: None,
        }
    }
}

#[derive(Clone)]
pub struct BlockNode {
    pub exprs: Vec<Node>,
    pub node_types: Vec<CompileType>,
}

impl BlockNode {
    pub fn new(exprs: Vec<Node>) -> Self {
        Self {
            exprs,
            node_types: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct CallNode {
    pub value: Node,
    pub args: Vec<Node>,
    pub end: Token,
}

#[derive(Clone)]
pub struct IfElseNode {
    pub start: Token,
    pub exprs: Vec<(Node, Node)>,
    pub alt: Option<Node>,
}

#[derive(Clone)]
pub struct FieldNode {
    pub expr: Node,
    pub field: Token,
    pub info: Option<FieldInfo>,
}

#[derive(Clone)]
pub enum TypeNode {
    Ident(IdentTypeNode),
    Fn(Box<FuncTypeNode>),
}

impl TypeNode {
    pub fn eval(&self, ctx: &mut CompileContext) -> Result<CompileType, CompileError> {
        match self {
            Self::Ident(n) => {
                let name = &ctx.input[n.start.start..n.start.end];
                let mut symbol_id = match ctx.scope.lookup(name) {
                    None => {
                        return Err(CompileError::NameError(format!("{} is not defined", name)))
                    }
                    Some((_, symid)) => symid,
                };

                for _tok in &n.items {
                    panic!("modules not implemented")
                }

                let symbol = ctx.scope.get_symbol(symbol_id);
                match &symbol.kind {
                    SymbolKind::Type => Ok(symbol.ctype),
                    _ => Err(CompileError::TypeError(format!(
                        "can not use {:?} as type",
                        symbol.ctype
                    ))),
                }
            }
            Self::Fn(n) => {
                let mut args = Vec::new();
                for (argname, argtype) in &n.args {
                    let ctype = argtype.eval(ctx)?;
                    args.push(FuncTypeArg {
                        name: argname.clone(),
                        ctype,
                        default: None,
                    });
                }
                let returns = n.returns.eval(ctx)?;

                Ok(CompileType::Func(ctx.trepo.new_func(returns, args)))
            }
        }
    }
}

#[derive(Clone)]
pub struct IdentTypeNode {
    pub start: Token,
    pub items: Vec<Token>,
}

#[derive(Clone)]
pub struct FuncTypeNode {
    pub returns: TypeNode,
    pub args: Vec<(Option<Box<str>>, TypeNode)>,
}

#[derive(Clone)]
pub struct FuncStmtNode {
    pub start: Token,
    pub name: Token,
    pub args: Vec<(Token, Option<TypeNode>)>,
    pub returns: Option<TypeNode>,
    pub body: Node,
    pub code: Option<*mut CodeValue>,
    pub scope: Option<Box<FuncScope>>,
}
