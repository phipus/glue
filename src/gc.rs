use crate::{
    instr::Instruction,
    rtype::{ObjectRuntimeType, RuntimeType},
    runtime::{FrameType, Function},
    rvalue::{CodeValue, InterfaceVTValue, ObjectValue, Value},
};

pub struct Collector {
    pub objects: Vec<Box<ObjectValue>>,
    pub object_types: Vec<Box<ObjectRuntimeType>>,
    pub code_objs: Vec<Box<CodeValue>>,
    pub frame_types: Vec<Box<FrameType>>,
    pub functions: Vec<Box<Function>>,
}

impl Collector {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            object_types: Vec::new(),
            code_objs: Vec::new(),
            frame_types: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub unsafe fn new_object(&mut self, otype_ptr: *mut ObjectRuntimeType) -> *mut ObjectValue {
        let otype = &mut *otype_ptr;
        let mut fields = Vec::with_capacity(otype.field_types.len());
        for t in otype.field_types.iter() {
            fields.push(Value::default(*t));
        }

        let mut obj = Box::new(ObjectValue {
            alive: true,
            fields: fields.into_boxed_slice(),
            rtype: otype_ptr,
        });
        let ptr = &mut *obj as *mut ObjectValue;
        self.objects.push(obj);
        return ptr;
    }

    pub unsafe fn new_object_type(
        &mut self,
        field_types: &[RuntimeType],
    ) -> *mut ObjectRuntimeType {
        let mut typ = Box::new(ObjectRuntimeType {
            alive: true,
            field_types: Box::from(field_types),
        });
        let ptr = &mut *typ as *mut ObjectRuntimeType;
        self.object_types.push(typ);
        return ptr;
    }

    pub fn new_code_obj(&mut self, code: Box<[Instruction]>) -> *mut CodeValue {
        push_gcobject(
            &mut self.code_objs,
            CodeValue {
                alive: true,
                instrs: code,
            },
        )
    }

    pub fn new_frame_type(&mut self, field_types: Box<[RuntimeType]>) -> *mut FrameType {
        push_gcobject(
            &mut self.frame_types,
            FrameType {
                alive: true,
                field_types,
            },
        )
    }

    pub unsafe fn new_function(
        &mut self,
        code: *mut CodeValue,
        ftype: *mut FrameType,
        code_offset: usize,
        argc: u32,
        retc: u32,
    ) -> *mut Function {
        push_gcobject(
            &mut self.functions,
            Function {
                alive: true,
                code,
                ftype,
                code_offset,
                argc,
                retc,
            },
        )
    }
}

pub unsafe fn mark_value(v: Value, rtype: RuntimeType) {
    match rtype {
        RuntimeType::Bool | RuntimeType::Int | RuntimeType::Uint | RuntimeType::Float => (),
        RuntimeType::Object => mark_object_value(v.o),
        RuntimeType::Code => mark_code_value(v.c),
        RuntimeType::InterfaceVT => mark_interface_vt(v.v),
    }
}

pub unsafe fn mark_object_value(ptr: *mut ObjectValue) {
    if ptr.is_null() {
        return;
    }
    let v = &mut *ptr;
    if v.alive {
        return;
    }
    v.alive = true;
    let rtype = &mut *v.rtype;
    rtype.alive = true;

    for (i, t) in rtype.field_types.iter().enumerate() {
        mark_value(v.fields[i], *t);
    }
}

pub unsafe fn mark_code_value(ptr: *mut CodeValue) {
    if ptr.is_null() {
        return;
    }

    let v = &mut *ptr;
    if v.alive {
        return;
    }
    v.alive = true;

    mark_instructions(&v.instrs);
}

pub unsafe fn mark_interface_vt(ptr: *mut InterfaceVTValue) {
    if ptr.is_null() {
        return;
    }

    let v = &mut *ptr;
    if v.alive {
        return;
    }
    v.alive = true;

    for (f, _) in v.funcs.iter() {
        mark_code_value(*f);
    }
}

pub unsafe fn mark_frame_type(ptr: *mut FrameType) {
    if ptr.is_null() {
        return;
    }
    let v = &mut *ptr;
    v.alive = true;
}

pub unsafe fn mark_instructions(code: &[Instruction]) {
    for instr in code {
        match instr {
            Instruction::Bool(_) => (),
            Instruction::Uint(_) => (),
            Instruction::Int(_) => (),
            Instruction::Float(_) => (),
            Instruction::Object(rtype) => (**rtype).alive = true,
            Instruction::InterfaceVT(vt) => mark_interface_vt(*vt),
            Instruction::Code(code) => mark_code_value(*code),
            Instruction::PushLocal(_, _) => (),
            Instruction::PopLocal(_) => (),
            Instruction::GetField(_, _) => (),
            Instruction::GetNextField(_, _) => (),
            Instruction::SetField(_) => (),
            Instruction::PopDiscard => (),
            Instruction::Call(func) => mark_function(*func),
            Instruction::NativeCall(_) => (),
            Instruction::Ret => (),

            Instruction::BoolAndBool => (),
            Instruction::BoolOrBool => (),
            Instruction::BoolEqBool => (),
            Instruction::BoolNeBool => (),

            Instruction::UintAddUint => (),
            Instruction::UintSubUint => (),
            Instruction::UintMulUint => (),
            Instruction::UintDivUint => (),
            Instruction::UintGtUint => (),
            Instruction::UintGeUint => (),
            Instruction::UintEqUint => (),
            Instruction::UintNeUint => (),
            Instruction::UintLeUint => (),
            Instruction::UintLtUint => (),

            Instruction::IntAddInt => (),
            Instruction::IntSubInt => (),
            Instruction::IntMulInt => (),
            Instruction::IntDivInt => (),
            Instruction::IntGtInt => (),
            Instruction::IntGeInt => (),
            Instruction::IntEqInt => (),
            Instruction::IntNeInt => (),
            Instruction::IntLeInt => (),
            Instruction::IntLtInt => (),

            Instruction::FloatAddFloat => (),
            Instruction::FloatSubFloat => (),
            Instruction::FloatMulFloat => (),
            Instruction::FloatDivFloat => (),
            Instruction::FloatGtFloat => (),
            Instruction::FloatGeFloat => (),
            Instruction::FloatEqFloat => (),
            Instruction::FloatNeFloat => (),
            Instruction::FloatLeFloat => (),
            Instruction::FloatLtFloat => (),

            Instruction::UintToInt => (),
            Instruction::UintToFloat => (),
            Instruction::IntToUint => (),
            Instruction::IntToFloat => (),
            Instruction::FloatToUint => (),
            Instruction::FloatToInt => (),

            Instruction::NegInt => (),
            Instruction::NegFloat => (),
            Instruction::NegBool => (),

            Instruction::Jump(_) => (),
            Instruction::JumpTrue(_) => (),
            Instruction::JumpFalse(_) => (),
        }
    }
}

pub unsafe fn mark_function(ptr: *mut Function) {
    if ptr.is_null() {
        return;
    }

    let func = &mut *ptr;
    if func.alive {
        return;
    }
    func.alive = true;
}

fn push_gcobject<T>(objs: &mut Vec<Box<T>>, obj: T) -> *mut T {
    let mut b = Box::new(obj);
    let ptr = &mut *b as *mut T;
    objs.push(b);
    return ptr;
}
