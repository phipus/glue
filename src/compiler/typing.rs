use crate::rtype::RuntimeType;

use super::compile::Node;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompileType {
    Unit,
    Bool,
    Uint,
    Int,
    Float,
    Func(FuncID),
}

impl CompileType {
    pub fn get_size(&self, _repo: &TypeRepo) -> u32 {
        match self {
            Self::Unit => 0,
            Self::Bool | Self::Uint | Self::Int | Self::Float => 1,
            Self::Func(_) => panic!("can not get size of a function"),
        }
    }

    pub fn fields(&self, rtypes: &mut Vec<RuntimeType>, _repo: &TypeRepo) -> u32 {
        let len = rtypes.len();
        match self {
            Self::Unit => (),
            Self::Bool => rtypes.push(RuntimeType::Bool),
            Self::Uint => rtypes.push(RuntimeType::Uint),
            Self::Int => rtypes.push(RuntimeType::Int),
            Self::Float => rtypes.push(RuntimeType::Float),
            Self::Func(_) => panic!("can not get fields of a function"),
        }

        (rtypes.len() - len) as u32
    }
}

#[derive(Clone)]
pub struct FuncArg {
    pub name: Box<str>,
    pub ctype: CompileType,
    pub default: Option<Box<Node>>,
}

#[derive(Clone)]
pub struct FuncType {
    pub returns: CompileType,
    pub args: Vec<FuncArg>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FuncID {
    index: usize,
}

pub struct TypeRepo {
    pub func_types: Vec<FuncType>,
}

impl TypeRepo {
    pub fn new() -> Self {
        Self {
            func_types: Vec::new(),
        }
    }

    pub fn get_func(&self, id: FuncID) -> &FuncType {
        &self.func_types[id.index]
    }

    pub fn get_func_mut(&mut self, id: FuncID) -> &mut FuncType {
        &mut self.func_types[id.index]
    }

    pub fn new_func(&mut self, rtype: CompileType, args: Vec<FuncArg>) -> FuncID {
        let id = FuncID {
            index: self.func_types.len(),
        };
        self.func_types.push(FuncType {
            returns: rtype,
            args,
        });
        return id;
    }
}
