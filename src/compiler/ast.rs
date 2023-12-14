use super::{
    compile::Node,
    scan::Token,
    scope::SymbolID,
    typing::{CompileType, FieldInfo},
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
    pub ctype_name: Option<Box<str>>,
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
