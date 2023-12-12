use std::collections::HashMap;
use std::fmt::Write;

use crate::{rtype::RuntimeType, runtime::Function};

use super::typing::{CompileType, TypeRepo};

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

    pub fn declare_type(&mut self, name: &str, ctype: CompileType) -> SymbolID {
        self.declare_symbol(
            name,
            Symbol {
                ctype,
                kind: SymbolKind::Type,
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
                                SymbolKind::Type => {
                                    let symbol = Symbol {
                                        ctype: parent_symbol.ctype,
                                        kind: SymbolKind::Type,
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
                SymbolKind::Type => SymbolLocation::Type,
            };

            self.locations.push(loc);
        }

        fields
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SymbolID {
    index: usize,
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
    Type,
}

pub struct Symbol {
    pub ctype: CompileType,
    pub kind: SymbolKind,
}

pub enum SymbolLocation {
    Local { offset: u32 },
    Function { ptr: *mut Function },
    Type,
}

pub struct Module {
    pub name: Box<str>,
    pub symbols: HashMap<Box<str>, Symbol>,
}
