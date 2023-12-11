use super::{
    compile::{Node, SymbolID},
    scan::Token,
    typing::CompileType,
};

pub struct IntNode {
    pub token: Token,
    pub as_uint: bool,
    pub as_float: bool,
}

impl IntNode {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            as_uint: false,
            as_float: false,
        }
    }
}

pub struct FloatNode {
    pub token: Token,
}

impl FloatNode {
    pub fn new(token: Token) -> Self {
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

pub struct BinaryOpNode {
    pub initial: Node,
    pub expr_type: CompileType,
    pub ops: Vec<(BinaryOperand, Node)>,
}

pub struct DeclarationNode {
    pub start: Token,
    pub end: Token,
    pub name: Box<str>,
    pub assignment: Option<Node>,
    pub ctype_name: Option<Box<str>>,
    pub ctype: Option<CompileType>,
    pub symbol: Option<SymbolID>,
}

pub struct AssignmentNode {
    pub left: Node,
    pub right: Node,
}

impl AssignmentNode {
    pub fn new(left: Node, right: Node) -> Self {
        Self { left, right }
    }
}

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

pub struct CallNode {
    pub value: Node,
    pub args: Vec<Node>,
    pub end: Token,
}