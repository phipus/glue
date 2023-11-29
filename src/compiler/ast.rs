use super::{compile::Node, scan::Token, typing::CompileType};

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
