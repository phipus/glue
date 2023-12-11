use std::{collections::HashMap, fmt::Write, sync::Mutex};

use crate::{gc::Collector, instr::Instruction, rtype::RuntimeType, runtime::Function};

use super::{
    ast::{
        AssignmentNode, BinaryOpNode, BinaryOperand, BlockNode, CallNode, DeclarationNode,
        FloatNode, IntNode, VariableNode,
    },
    error::CompileError,
    scan::{token, Token},
    typing::{CompileType, TypeRepo},
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

pub enum Node {
    Int(IntNode),
    Float(FloatNode),
    BinaryOp(Box<BinaryOpNode>),
    Declaration(Box<DeclarationNode>),
    Variable(VariableNode),
    Assignment(Box<AssignmentNode>),
    Block(BlockNode),
    Call(Box<CallNode>),
}

impl Node {
    pub fn start_token(&self) -> Token {
        match self {
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
        }
    }

    pub fn end_token(&self) -> Token {
        match self {
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
        }
    }

    pub fn check_type(
        &mut self,
        ctx: &mut CompileContext,
        type_hint: Option<CompileType>,
    ) -> Result<CompileType> {
        match self {
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
                    let argtype = arg.check_type(ctx, Some(ftype.args[i]))?;
                    if argtype != ftype.args[i] {
                        return Err(CompileError::TypeError(format!("unexpected argument type")));
                    }
                }

                Ok(ftype.returns)
            }
        }
    }

    pub fn compile(&self, ctx: &mut CompileContext, code: &mut Vec<Instruction>) -> Result<()> {
        match self {
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
                for (op, node) in n.ops.iter() {
                    node.compile(ctx, code)?;
                    code.push(binary_operand_to_instr(&n.expr_type, &op)?);
                }

                Ok(())
            }
            Self::Declaration(n) => {
                if let Some(assignment) = &n.assignment {
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

                        SymbolLocation::Type { ctype: _ } => {
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
                            code.push(Instruction::PushLocal((*offset + i as u32, field)));
                        }
                    }
                    SymbolLocation::Function { ptr: _ } => {
                        return Err(CompileError::TypeError(format!(
                            "can not evaluate function (call required)"
                        )))
                    }
                    SymbolLocation::Type { ctype: _ } => {
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
                        SymbolLocation::Type { ctype: _ } => {
                            return Err(CompileError::TypeError(format!("can not assign type")))
                        }
                    }

                    Ok(())
                }

                _ => panic!("can not assign {}", n.left.node_kind_str()),
            },
            Self::Block(n) => {
                for (i, child) in n.exprs.iter().enumerate() {
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
                            for arg in &n.args {
                                arg.compile(ctx, code)?;
                            }
                            code.push(Instruction::Call(ptr));

                            Ok(())
                        }
                        SymbolLocation::Local { offset: _ } => {
                            panic!("function pointers not implemented")
                        }
                        SymbolLocation::Type { ctype: _ } => {
                            Err(CompileError::TypeError(format!("type is not callable")))
                        }
                    }
                }
                _ => panic!("function pointers not implemented"),
            },
        }
    }

    pub fn node_kind_str(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Float(_) => "float",
            Self::BinaryOp(_) => "binary op",
            Self::Declaration(_) => "declaration",
            Self::Variable(_) => "variable",
            Self::Assignment(_) => "assignment",
            Self::Block(_) => "block",
            Self::Call(_) => "call",
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SymbolID {
    index: usize,
}

pub struct FuncScope {
    pub parent: Option<Box<FuncScope>>,
    pub names: HashMap<Box<str>, SymbolID>,
    pub symbols: Vec<Symbol>,
    pub active_scopes: Vec<u32>,
    pub scopeid: u32,
    name_buf: String,
    current_scopeid: u32,
    locations: Vec<SymbolLocation>,
}

impl FuncScope {
    pub fn new() -> Self {
        Self {
            parent: None,
            names: HashMap::new(),
            symbols: Vec::new(),
            active_scopes: Vec::new(),
            scopeid: 1,
            name_buf: String::new(),
            current_scopeid: 1,
            locations: Vec::new(),
        }
    }

    pub fn push_scope(&mut self) {
        let mut parent = Box::new(Self::new());
        std::mem::swap(self, &mut *parent);
        self.parent = Some(parent);
    }

    pub fn pop_scope(&mut self) -> Option<Box<Self>> {
        match self.parent.take() {
            None => None,
            Some(mut parent) => {
                std::mem::swap(self, &mut *parent);
                Some(parent)
            }
        }
    }

    fn declare_symbol(&mut self, name: &str, symbol: Symbol) -> SymbolID {
        self.declare_symbol_scoped(name, symbol, self.scopeid)
    }

    fn declare_symbol_scoped(&mut self, name: &str, symbol: Symbol, scopeid: u32) -> SymbolID {
        let symbol_id = SymbolID {
            index: self.symbols.len(),
        };
        self.symbols.push(symbol);

        Self::generate_scoped_name(&mut self.name_buf, scopeid, name);
        self.names
            .insert(Box::from(self.name_buf.as_str()), symbol_id);
        return symbol_id;
    }

    pub fn declare_variable(&mut self, name: &str, ctype: CompileType) -> SymbolID {
        self.declare_symbol(
            name,
            Symbol {
                ctype,
                kind: SymbolKind::Variable {
                    scopeid: self.scopeid,
                    captured: false,
                    assigned: false,
                },
            },
        )
    }

    pub fn declare_function(
        &mut self,
        name: &str,
        ctype: CompileType,
        func: *mut Function,
    ) -> SymbolID {
        self.declare_symbol(
            name,
            Symbol {
                ctype,
                kind: SymbolKind::Function { ptr: func },
            },
        )
    }

    fn generate_scoped_name(buf: &mut String, scopeid: u32, name: &str) {
        buf.clear();
        write!(buf, "{}-{}", scopeid, name).unwrap()
    }

    pub fn lookup(&mut self, name: &str) -> Option<(u32, SymbolID)> {
        Self::generate_scoped_name(&mut self.name_buf, self.scopeid, name);

        match self.names.get(self.name_buf.as_str()) {
            Some(sym) => Some((self.scopeid, *sym)),
            None => {
                for scopeid in self.active_scopes.iter().rev() {
                    Self::generate_scoped_name(&mut self.name_buf, *scopeid, name);
                    if let Some(sym) = self.names.get(self.name_buf.as_str()) {
                        return Some((*scopeid, *sym));
                    }
                }

                // symbol not resolved in this context, check the parent contex if available
                if let Some(parent) = &mut self.parent {
                    match parent.lookup(name) {
                        None => None,
                        Some((_, parent_sym_id)) => {
                            let parent_symbol = parent.get_symbol_mut(parent_sym_id);
                            match &parent_symbol.kind {
                                SymbolKind::Variable {
                                    scopeid: _,
                                    captured: _,
                                    assigned: _,
                                } => panic!("can not capture variables"),
                                SymbolKind::Function { ptr } => {
                                    let symbol = Symbol {
                                        ctype: parent_symbol.ctype,
                                        kind: SymbolKind::Function { ptr: *ptr },
                                    };

                                    Some((0, self.declare_symbol_scoped(name, symbol, 0)))
                                }
                                SymbolKind::Type { ctype } => {
                                    let symbol = Symbol {
                                        ctype: *ctype,
                                        kind: SymbolKind::Type { ctype: *ctype },
                                    };
                                    Some((0, self.declare_symbol_scoped(name, symbol, 0)))
                                }
                            }
                        }
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn begin_scope(&mut self) {
        self.active_scopes.push(self.scopeid);
        self.current_scopeid += 1;
        self.scopeid = self.current_scopeid;
    }

    pub fn end_scope(&mut self) {
        self.scopeid = self.active_scopes.pop().unwrap();
    }

    pub fn get_symbol(&self, id: SymbolID) -> &Symbol {
        &self.symbols[id.index]
    }

    pub fn get_symbol_mut(&mut self, id: SymbolID) -> &mut Symbol {
        &mut self.symbols[id.index]
    }

    pub fn get_location(&self, id: SymbolID) -> &SymbolLocation {
        &self.locations[id.index]
    }

    pub fn generate_frame(&mut self, trepo: &TypeRepo) -> Vec<RuntimeType> {
        let mut fields = Vec::<RuntimeType>::new();
        for sym in &self.symbols {
            let loc = match sym.kind {
                SymbolKind::Function { ptr } => SymbolLocation::Function { ptr },
                SymbolKind::Variable {
                    scopeid: _,
                    captured: _,
                    assigned: _,
                } => {
                    sym.ctype.fields(&mut fields, trepo);
                    SymbolLocation::Local {
                        offset: (fields.len() - 1) as u32,
                    }
                }
                SymbolKind::Type { ctype } => SymbolLocation::Type { ctype },
            };

            self.locations.push(loc);
        }

        fields
    }
}

pub enum SymbolKind {
    Variable {
        scopeid: u32,
        captured: bool,
        assigned: bool,
    },
    Function {
        ptr: *mut Function,
    },
    Type {
        ctype: CompileType,
    },
}

pub struct Symbol {
    pub ctype: CompileType,
    pub kind: SymbolKind,
}

pub enum SymbolLocation {
    Local { offset: u32 },
    Function { ptr: *mut Function },
    Type { ctype: CompileType },
}
