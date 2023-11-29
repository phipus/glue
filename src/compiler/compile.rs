use crate::instr::Instruction;

use super::{
    ast::{BinaryOpNode, BinaryOperand, FloatNode, IntNode},
    error::CompileError,
    scan::Token,
    typing::CompileType,
};

type Result<T> = std::result::Result<T, CompileError>;

pub trait NodeValue {
    fn start_token(&self) -> Token;
    fn end_token(&self) -> Token;
    fn check_type(
        &mut self,
        ctx: &mut CompileContext,
        type_hint: Option<CompileType>,
    ) -> Result<CompileType>;

    fn compile(&self, ctx: &mut CompileContext, code: &mut Vec<Instruction>) -> Result<()>;
}

pub type Node = Box<dyn NodeValue>;

impl NodeValue for IntNode {
    fn start_token(&self) -> Token {
        self.token
    }

    fn end_token(&self) -> Token {
        self.token
    }

    fn check_type(
        &mut self,
        _ctx: &mut CompileContext,
        type_hint: Option<CompileType>,
    ) -> Result<CompileType> {
        match type_hint {
            Some(type_hint) => match type_hint {
                CompileType::Uint => {
                    self.as_uint = true;
                    Ok(CompileType::Uint)
                }
                CompileType::Int => Ok(CompileType::Int),
                CompileType::Float => {
                    self.as_float = true;
                    Ok(CompileType::Float)
                }
                _ => Err(CompileError::TypeError(format!(
                    "can not convert untyped int to {:?}",
                    type_hint
                ))),
            },

            None => Ok(CompileType::Int),
        }
    }

    fn compile(&self, ctx: &mut CompileContext, code: &mut Vec<Instruction>) -> Result<()> {
        let token_value = &ctx.input[self.token.start..self.token.end];
        if self.as_uint {
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
        if self.as_float {
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
}

impl NodeValue for FloatNode {
    fn start_token(&self) -> Token {
        self.token
    }

    fn end_token(&self) -> Token {
        self.token
    }

    fn check_type(
        &mut self,
        _ctx: &mut CompileContext,
        type_hint: Option<CompileType>,
    ) -> Result<CompileType> {
        match type_hint {
            Some(type_hint) => match type_hint {
                CompileType::Float => Ok(CompileType::Float),
                _ => Err(CompileError::TypeError(format!(
                    "can not convert float to {:?}",
                    type_hint
                ))),
            },
            None => Ok(CompileType::Float),
        }
    }

    fn compile(&self, ctx: &mut CompileContext, code: &mut Vec<Instruction>) -> Result<()> {
        let token_value = &ctx.input[self.token.start..self.token.end];
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
}

impl NodeValue for BinaryOpNode {
    fn start_token(&self) -> Token {
        self.initial.start_token()
    }

    fn end_token(&self) -> Token {
        match self.ops.last() {
            Some((_, last)) => last.end_token(),
            None => self.initial.end_token(),
        }
    }

    fn check_type(
        &mut self,
        ctx: &mut CompileContext,
        type_hint: Option<CompileType>,
    ) -> Result<CompileType> {
        let expr_type = self.initial.check_type(ctx, type_hint)?;
        self.expr_type = expr_type;

        for (_, node) in self.ops.iter_mut() {
            node.check_type(ctx, Some(expr_type))?;
        }

        Ok(expr_type)
    }

    fn compile(&self, ctx: &mut CompileContext, code: &mut Vec<Instruction>) -> Result<()> {
        self.initial.compile(ctx, code)?;
        for (op, node) in self.ops.iter() {
            node.compile(ctx, code)?;
            code.push(binary_operand_to_instr(&self.expr_type, &op)?);
        }

        Ok(())
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
}
