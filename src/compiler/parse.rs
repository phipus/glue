use super::{
    ast::{BinaryOpNode, BinaryOperand, FloatNode, IntNode},
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
            token::INT => Box::new(IntNode::new(self.consume())),
            token::FLOAT => Box::new(FloatNode::new(self.consume())),

            _ => return Err(self.err_unexpected_token(&self.l0)),
        };

        Ok(node)
    }

    fn parse_factor(&mut self) -> Result<Node> {
        self.parse_binaryop(
            |p| p.parse_atom(),
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
            return Ok(Box::new(BinaryOpNode {
                initial,
                expr_type: CompileType::Bool,
                ops: node_ops,
            }));
        }
    }
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

        let mut code = Vec::<Instruction>::new();

        let mut ctx = &mut CompileContext {
            input: p.scan.input,
        };

        expr.check_type(ctx, Some(CompileType::Float)).unwrap();

        expr.compile(&mut ctx, &mut code).unwrap();

        let mut gc = Collector::new();
        let builtins = Builtins::new(&mut gc);

        code.push(Instruction::Call(builtins.print_float));

        let code_obj = gc.new_code_obj(code.into_boxed_slice());
        let ftype = gc.new_frame_type(Box::new([]));

        let mut rt = Thread::new(Arc::new(Mutex::new(gc)));
        unsafe {
            rt.push_frame(ftype, code_obj, 0);
            rt.eval().unwrap();
        }
    }
}
