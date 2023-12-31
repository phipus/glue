use std::collections::HashMap;

use crate::{rtype::RuntimeType, runtime::Function};

use super::compile::Node;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompileType {
    Never,
    Unit,
    Bool,
    Uint,
    Int,
    Float,
    Func(FuncID),
    Object(ObjectID),
}

impl CompileType {
    pub fn get_size(&self, _repo: &TypeRepo) -> u32 {
        match self {
            Self::Never => 0,
            Self::Unit => 0,
            Self::Bool | Self::Uint | Self::Int | Self::Float => 1,
            Self::Func(_) => panic!("can not get size of a function"),
            Self::Object(_) => 1,
        }
    }

    pub fn fields(&self, rtypes: &mut Vec<RuntimeType>, _repo: &TypeRepo) -> u32 {
        let len = rtypes.len();
        match self {
            Self::Never => (),
            Self::Unit => (),
            Self::Bool => rtypes.push(RuntimeType::Bool),
            Self::Uint => rtypes.push(RuntimeType::Uint),
            Self::Int => rtypes.push(RuntimeType::Int),
            Self::Float => rtypes.push(RuntimeType::Float),
            Self::Func(_) => panic!("can not get fields of a function"),
            Self::Object(_) => rtypes.push(RuntimeType::Object),
        }

        (rtypes.len() - len) as u32
    }

    pub fn is_assignable_to(&self, t: CompileType, _repo: &TypeRepo) -> bool {
        if self == &t {
            return true;
        }

        match self {
            Self::Never => t == CompileType::Unit,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct FuncTypeArg {
    pub name: Option<Box<str>>,
    pub ctype: CompileType,
    pub default: Option<Box<Node>>,
}

#[derive(Clone)]
pub struct FuncType {
    pub returns: CompileType,
    pub args: Vec<FuncTypeArg>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FuncID {
    index: usize,
}

#[derive(Clone)]
pub struct ObjectType {
    field_names: HashMap<Box<str>, usize>,
    method_names: HashMap<Box<str>, usize>,
    fields: Vec<FieldInfo>,
    methods: Vec<(*mut Function, FuncID)>,
}

impl ObjectType {
    pub fn get_field_by_name(&self, name: &str) -> Option<FieldInfo> {
        match self.field_names.get(name) {
            Some(index) => Some(self.fields[*index]),
            None => None,
        }
    }

    pub fn get_field_by_index(&self, index: usize) -> FieldInfo {
        self.fields[index]
    }

    pub fn get_field_index(&self, name: &str) -> Option<usize> {
        self.field_names.get(name).map(|idx| *idx)
    }

    pub fn get_method_by_name(&self, name: &str) -> Option<(*mut Function, FuncID)> {
        match self.method_names.get(name) {
            Some(index) => Some(self.methods[*index]),
            None => None,
        }
    }

    pub fn get_method_by_index(&self, index: usize) -> (*mut Function, FuncID) {
        self.methods[index]
    }

    pub fn get_method_index(&self, name: &str) -> Option<usize> {
        self.method_names.get(name).map(|idx| *idx)
    }
}

#[derive(Clone, Copy)]
pub struct Field {
    pub ctype: CompileType,
    pub is_variable: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct FieldInfo {
    pub ctype: CompileType,
    pub offset: u32,
    pub is_variable: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ObjectID {
    index: usize,
}

pub struct TypeRepo {
    pub func_types: Vec<FuncType>,
    pub object_types: Vec<ObjectType>,
}

impl TypeRepo {
    pub fn new() -> Self {
        Self {
            func_types: Vec::new(),
            object_types: Vec::new(),
        }
    }

    pub fn get_func(&self, id: FuncID) -> &FuncType {
        &self.func_types[id.index]
    }

    pub fn get_func_mut(&mut self, id: FuncID) -> &mut FuncType {
        &mut self.func_types[id.index]
    }

    pub fn new_func(&mut self, rtype: CompileType, args: Vec<FuncTypeArg>) -> FuncID {
        let id = FuncID {
            index: self.func_types.len(),
        };
        self.func_types.push(FuncType {
            returns: rtype,
            args,
        });
        return id;
    }

    pub fn get_object(&self, id: ObjectID) -> &ObjectType {
        &self.object_types[id.index]
    }

    pub fn get_object_mut(&mut self, id: ObjectID) -> &mut ObjectType {
        &mut self.object_types[id.index]
    }

    pub fn new_object(&mut self, given_fields: Vec<(Box<str>, Field)>) -> ObjectID {
        let mut field_names = HashMap::new();
        let mut fields = Vec::new();
        let mut offset = 0u32;

        for (name, field) in given_fields {
            field_names.insert(name, fields.len());
            fields.push(FieldInfo {
                ctype: field.ctype,
                is_variable: field.is_variable,
                offset,
            });
            offset += field.ctype.get_size(self)
        }

        let id = ObjectID {
            index: self.object_types.len(),
        };
        self.object_types.push(ObjectType {
            field_names,
            fields,
            method_names: HashMap::new(),
            methods: Vec::new(),
        });
        return id;
    }
}

