use super::{
    ast::{
        AssignmentNode, BinaryOpNode, BinaryOperand, BlockNode, CallNode, DeclarationNode,
        FloatNode, IfElseNode, IntNode, VariableNode, BoolNode,
    },
    compile::Node,
    error::CompileError,
    scan::{token, Scan, Token},
    typing::CompileType,
};

pub struct Parse<'a> {
    scan: Scan<'a>,
    l0: Token,
}

type Result<T> = std::result::Result<T, CompileError>;

impl<'a> Parse<'a> {
    pub fn new(input: &'a str, filename: &'a str) -> Self {
        let mut scan = Scan::new(input, filename);
        let l0 = scan.next_token();
        Self { scan, l0 }
    }

    pub fn consume_token(&mut self, kind: i32) -> Result<Token> {
        let t = self.l0;
        if t.kind != kind {
            return Err(self.err_unexpected_token(&t));
        }
        self.l0 = self.scan.next_token();
        return Ok(t);
    }

    fn err_unexpected_token(&self, t: &Token) -> CompileError {
        CompileError::UnexpectedToken(format!("{}", &self.scan.input[t.start..t.end]))
    }

    pub fn consume(&mut self) -> Token {
        let t = self.l0;
        self.l0 = self.scan.next_token();
        return t;
    }

    pub fn parse_atom(&mut self) -> Result<Node> {
        let node: Node = match self.l0.kind {
            token::INT => Node::Int(IntNode::new(self.consume())),
            token::FLOAT => Node::Float(FloatNode::new(self.consume())),
            token::IDENT => Node::Variable(VariableNode::new(self.consume())),
            token::TRUE | token::FALSE => Node::Bool(BoolNode::new(self.consume())),

            _ => return Err(self.err_unexpected_token(&self.l0)),
        };

        Ok(node)
    }

    pub fn parse_postfix(&mut self) -> Result<Node> {
        let atom = self.parse_atom()?;

        if self.l0.kind == '(' as i32 {
            self.consume();
            let mut args = Vec::<Node>::new();
            while self.l0.kind != ')' as i32 {
                let n = self.parse_expr()?;
                args.push(n);

                if self.l0.kind == ',' as i32 {
                    self.consume();
                } else {
                    break;
                }
            }
            let end = self.consume_token(')' as i32)?; // consume the )

            Ok(Node::Call(Box::new(CallNode {
                value: atom,
                args,
                end,
            })))
        } else {
            Ok(atom)
        }
    }

    fn parse_factor(&mut self) -> Result<Node> {
        self.parse_binaryop(
            |p| p.parse_postfix(),
            &[
                ('*' as i32, BinaryOperand::Mul),
                ('/' as i32, BinaryOperand::Div),
            ],
        )
    }

    fn parse_term(&mut self) -> Result<Node> {
        self.parse_binaryop(
            |p| p.parse_factor(),
            &[
                ('+' as i32, BinaryOperand::Add),
                ('-' as i32, BinaryOperand::Sub),
            ],
        )
    }

    pub fn parse_expr(&mut self) -> Result<Node> {
        self.parse_term()
    }

    pub fn parse_stmt(&mut self, with_semicolon: bool) -> Result<Node> {
        let (node, requires_semicolon) = match self.l0.kind {
            token::LET => (self.parse_let_stmt()?, true),
            token::IF => (self.parse_if_stmt()?, false),
            _ => {
                let left = self.parse_expr()?;
                if self.l0.kind == '=' as i32 {
                    self.consume();
                    let right = self.parse_expr()?;
                    (
                        Node::Assignment(Box::new(AssignmentNode::new(left, right))),
                        true,
                    )
                } else {
                    (left, true)
                }
            }
        };

        if requires_semicolon && with_semicolon {
            self.consume_token(';' as i32)?;
        }

        Ok(node)
    }

    pub fn parse_binaryop<F: FnMut(&mut Parse) -> Result<Node>>(
        &mut self,
        mut next: F,
        ops: &[(i32, BinaryOperand)],
    ) -> Result<Node> {
        let initial = next(self)?;
        let mut node_ops = Vec::<(BinaryOperand, Node)>::new();

        'main: loop {
            for (kind, op) in ops.iter() {
                if self.l0.kind == *kind {
                    self.consume();
                    let node = next(self)?;
                    node_ops.push((*op, node));
                    continue 'main;
                }
            }
            if node_ops.len() == 0 {
                return Ok(initial);
            }
            return Ok(Node::BinaryOp(Box::new(BinaryOpNode {
                initial,
                expr_type: CompileType::Bool,
                ops: node_ops,
            })));
        }
    }

    fn get_token_value(&self, start: Token, end: Token) -> &str {
        &self.scan.input[start.start..end.end]
    }

    fn parse_let_stmt(&mut self) -> Result<Node> {
        let start = self.consume_token(token::LET)?;
        let ident = self.consume_token(token::IDENT)?;
        let mut ctype_name = None;
        if self.l0.kind == ':' as i32 {
            self.consume();
            ctype_name = Some(self.parse_type()?);
        }

        let mut end = ident;
        let mut assignment = None;
        if self.l0.kind == '=' as i32 {
            self.consume();
            let expr = self.parse_expr()?;
            end = expr.end_token();
            assignment = Some(expr);
        }

        Ok(Node::Declaration(Box::new(DeclarationNode {
            start,
            end,
            name: Box::from(self.get_token_value(ident, ident)),
            assignment,
            ctype_name,
            ctype: None,
            symbol: None,
        })))
    }

    fn parse_if_stmt(&mut self) -> Result<Node> {
        let start = self.consume_token(token::IF)?;
        let expr = self.parse_expr()?;
        let body = self.parse_block(BlockKind::Curly)?;

        let mut exprs = vec![(expr, body)];
        let mut alt = None;

        loop {
            if self.l0.kind != token::ELSE {
                break;
            }
            self.consume();

            if self.l0.kind == token::IF {
                self.consume();
                let expr = self.parse_expr()?;
                let body = self.parse_block(BlockKind::Curly)?;
                exprs.push((expr, body));
            } else {
                alt = Some(self.parse_block(BlockKind::Curly)?);
                break;
            }
        }

        Ok(Node::IfElse(Box::new(IfElseNode { start, exprs, alt })))
    }

    fn parse_type(&mut self) -> Result<Box<str>> {
        panic!("not implemented")
    }

    pub fn parse_block(&mut self, kind: BlockKind) -> Result<Node> {
        match kind {
            BlockKind::WholeFile => {
                let mut exprs = Vec::<Node>::new();

                while self.l0.kind != token::EOF {
                    let n = self.parse_stmt(true)?;
                    exprs.push(n);
                }

                Ok(Node::Block(BlockNode::new(exprs)))
            }
            BlockKind::Curly => {
                let mut exprs = Vec::<Node>::new();

                self.consume_token('{' as i32)?;
                while self.l0.kind != '}' as i32 {
                    let n = self.parse_stmt(true)?;
                    exprs.push(n);
                }
                self.consume(); // consume the '}'

                Ok(Node::Block(BlockNode::new(exprs)))
            }
            BlockKind::Expr => self.parse_expr(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BlockKind {
    WholeFile,
    Expr,
    Curly,
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use crate::{
        builtins::Builtins,
        compiler::{compile::CompileContext, typing::CompileType},
        gc::Collector,
        instr::Instruction,
        runtime::Thread,
    };

    use super::Parse;

    #[test]
    fn test_parse_expr() {
        let mut p = Parse::new("2 + 2 / 4", "<inline>");
        let mut expr = p.parse_expr().unwrap();
        let gc = Arc::new(Mutex::new(Collector::new()));

        let mut code = Vec::<Instruction>::new();

        let mut ctx = &mut CompileContext::new(p.scan.input, &*gc);

        expr.check_type(ctx, Some(CompileType::Float)).unwrap();
        let field_types = ctx.scope.generate_frame(&ctx.trepo);
        expr.compile(&mut ctx, &mut code).unwrap();

        let builtins;
        let code_obj;
        let ftype;
        {
            let mut gc = gc.lock().unwrap();
            builtins = Builtins::new(&mut gc);

            code.push(Instruction::Call(builtins.print_float));

            code_obj = gc.new_code_obj(code.into_boxed_slice());
            ftype = gc.new_frame_type(field_types.into_boxed_slice());
        };

        let mut rt = Thread::new(gc);
        unsafe {
            rt.push_frame(ftype, code_obj, 0);
            rt.eval().unwrap();
        }
    }
}
