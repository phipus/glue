#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RuntimeType {
    Bool,
    Uint,
    Int,
    Float,
    Object,
    InterfaceVT,
    Code,
}

pub struct ObjectRuntimeType {
    pub alive: bool,
    pub field_types: Box<[RuntimeType]>,
}