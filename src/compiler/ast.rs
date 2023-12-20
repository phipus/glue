use crate::runtime::Function;

use super::{
    compile::{CompileContext, Node},
    error::CompileError,
    scan::Token,
    scope::{FuncScope, SymbolID, SymbolKind},
    typing::{CompileType, FieldInfo, FuncTypeArg},
};

#[derive(Clone, Debug)]
pub struct BoolNode {
    pub token: Token,
}

impl BoolNode {
    pub const fn new(token: Token) -> Self {
        Self { token }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
    Gt,
    Ge,
    Eq,
    Ne,
    Le,
    Lt,
}

#[derive(Clone, Debug)]
pub struct BinaryOpNode {
    pub initial: Node,
    pub expr_types: Vec<CompileType>,
    pub ops: Vec<(BinaryOperand, Node)>,
}

#[derive(Clone, Debug)]
pub struct DeclarationNode {
    pub start: Token,
    pub end: Token,
    pub name: Box<str>,
    pub assignment: Option<Node>,
    pub ctype_name: Option<TypeNode>,
    pub ctype: Option<CompileType>,
    pub symbol: Option<SymbolID>,
}

#[derive(Clone, Debug)]
pub struct AssignmentNode {
    pub left: Node,
    pub right: Node,
}

impl AssignmentNode {
    pub const fn new(left: Node, right: Node) -> Self {
        Self { left, right }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct CallNode {
    pub value: Node,
    pub args: Vec<Node>,
    pub end: Token,
    pub call: Option<CallKind>,
}

#[derive(Clone, Copy, Debug)]
pub enum CallKind {
    Function { func: *mut Function },
    Method { func: *mut Function },
}

#[derive(Clone, Debug)]
pub struct IfElseNode {
    pub start: Token,
    pub exprs: Vec<(Node, Node)>,
    pub alt: Option<Node>,
}

#[derive(Clone, Debug)]
pub struct FieldNode {
    pub expr: Node,
    pub field: Token,
    pub info: Option<FieldInfo>,
}

#[derive(Clone, Debug)]
pub enum TypeNode {
    Ident(IdentTypeNode),
    Fn(Box<FuncTypeNode>),
}

impl TypeNode {
    pub fn eval(&self, ctx: &mut CompileContext) -> Result<CompileType, CompileError> {
        match self {
            Self::Ident(n) => {
                let name = &ctx.input[n.start.start..n.start.end];
                let symbol_id = match ctx.scope.lookup(name) {
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

#[derive(Clone, Debug)]
pub struct IdentTypeNode {
    pub start: Token,
    pub items: Vec<Token>,
}

#[derive(Clone, Debug)]
pub struct FuncTypeNode {
    pub returns: TypeNode,
    pub args: Vec<(Option<Box<str>>, TypeNode)>,
}

#[derive(Clone, Debug)]
pub struct FuncStmtNode {
    pub start: Token,
    pub name: Token,
    pub args: Vec<(Token, Option<TypeNode>)>,
    pub returns: Option<TypeNode>,
    pub body: Node,
    pub function: Option<*mut Function>,
    pub scope: Option<Box<FuncScope>>,
    pub implicit_return: bool,
}

#[derive(Clone, Debug)]
pub struct ReturnNode {
    pub start: Token,
    pub value: Option<Node>,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOP {
    Minus,
    Plus,
    Not,
}

#[derive(Clone, Debug)]
pub struct UnaryNode {
    pub start: Token,
    pub atom: Node,
    pub ops: Vec<UnaryOP>,
    pub ctype: Option<CompileType>,
}
