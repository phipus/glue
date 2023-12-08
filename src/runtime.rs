use std::{
    error::Error,
    sync::{Arc, Mutex},
};

use crate::{
    gc::Collector,
    instr::Instruction,
    rtype::RuntimeType,
    rvalue::{CodeValue, Value},
};

pub struct Runtime {}

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

pub struct Thread {
    pub locals: Vec<Value>,
    pub call_stack: Vec<StackFrame>,
    pub eval_stack: Vec<(Value, RuntimeType)>,
    pub gc: Arc<Mutex<Collector>>,
    pub exc: Option<Box<dyn Error>>,
}

impl Thread {
    pub fn new(gc: Arc<Mutex<Collector>>) -> Self {
        Self {
            locals: Vec::new(),
            call_stack: Vec::new(),
            eval_stack: Vec::new(),
            gc,
            exc: None,
        }
    }

    pub unsafe fn push_frame(
        &mut self,
        ftype: *mut FrameType,
        code: *mut CodeValue,
        code_offset: usize,
    ) {
        self.call_stack.push(StackFrame {
            code_value: code,
            code: &(*code).instrs[code_offset..],
            fp: self.locals.len(),
            ftype,
        });
        for rtype in (*ftype).field_types.iter() {
            self.locals.push(Value::default(*rtype))
        }
    }

    fn get_code_and_fp(&self) -> (&'static [Instruction], usize) {
        let len = self.call_stack.len();
        let frame = &self.call_stack[len - 1];

        (frame.code, frame.fp)
    }

    fn backup_code_and_fp(&mut self, code: &'static [Instruction], fp: usize) {
        match self.call_stack.last_mut() {
            None => (),
            Some(frame) => {
                frame.code = code;
                frame.fp = fp;
            }
        }
    }

    pub unsafe fn eval(&mut self) -> Result<()> {
        let mut code;
        let mut fp;
        (code, fp) = self.get_code_and_fp();

        loop {
            let instr = {
                let len = code.len();
                if len <= 0 {
                    return Ok(());
                }
                code[0]
            };
            code = &code[1..];

            match instr {
                Instruction::Bool(b) => self.eval_stack.push((Value { b }, RuntimeType::Bool)),
                Instruction::Uint(u) => self.eval_stack.push((Value { u }, RuntimeType::Uint)),
                Instruction::Int(i) => self.eval_stack.push((Value { i }, RuntimeType::Int)),
                Instruction::Float(f) => self.eval_stack.push((Value { f }, RuntimeType::Float)),
                Instruction::Object(rtype) => {
                    let obj = {
                        let mut gc = self.gc.lock().unwrap();
                        gc.new_object(rtype)
                    };
                    self.eval_stack
                        .push((Value { o: obj }, RuntimeType::Object));
                }
                Instruction::InterfaceVT(_) => todo!(),
                Instruction::Code(_) => todo!(),
                Instruction::PushLocal((offset, rtype)) => {
                    self.eval_stack
                        .push((self.locals[fp + offset as usize], rtype));
                }
                Instruction::PopLocal(offset) => match self.eval_stack.pop() {
                    None => (),
                    Some((v, _)) => self.locals[fp + offset as usize] = v,
                },
                Instruction::GetField((offset, rtype)) => {
                    match self.eval_stack.pop() {
                        Some((v, _)) => {
                            // assert_eq!(t, RuntimeType::Object);
                            self.eval_stack
                                .push(((*v.o).fields[offset as usize], rtype));
                        }
                        None => (),
                    }
                }
                Instruction::SetField(offset) => match self.eval_stack.pop() {
                    None => (),
                    Some((v, _)) => match self.eval_stack.last() {
                        None => (),
                        Some((o, _)) => (*o.o).fields[offset as usize] = v,
                    },
                },
                Instruction::PopDiscard => {
                    self.eval_stack.pop();
                }
                Instruction::Call(func_ptr) => {
                    let func = &*func_ptr;
                    self.backup_code_and_fp(code, fp);
                    self.push_frame(func.ftype, func.code, func.code_offset);
                    (code, fp) = self.get_code_and_fp();

                    let len = self.eval_stack.len();
                    for i in 1..=func.argc as usize {
                        let (arg, _) = self.eval_stack.pop().expect("glue: not enough arguments");
                        self.locals[fp + len - i] = arg;
                    }
                }
                Instruction::NativeCall(func) => func(&mut self.locals[fp..], &mut self.eval_stack),
                Instruction::Ret => match self.call_stack.pop() {
                    None => return Ok(()),
                    Some(oldframe) => {
                        self.locals.truncate(oldframe.fp);
                        if self.call_stack.len() == 0 {
                            return Ok(());
                        }
                        (code, fp) = self.get_code_and_fp();
                    }
                },

                Instruction::BoolAndBool => self.binaryop(|l, r| Ok(l.b && r.b))?,
                Instruction::BoolOrBool => self.binaryop(|l, r| Ok(l.b || r.b))?,
                Instruction::UintAddUint => self.binaryop(|l, r| Ok(l.u + r.u))?,
                Instruction::UintSubUint => self.binaryop(|l, r| Ok(l.u - r.u))?,
                Instruction::UintMulUint => self.binaryop(|l, r| Ok(l.u * r.u))?,
                Instruction::UintDivUint => self.binaryop(|l, r| {
                    if r.u == 0 {
                        Err(Box::new(ErrDiv0))
                    } else {
                        Ok(l.u / r.u)
                    }
                })?,
                Instruction::IntAddInt => self.binaryop(|l, r| Ok(l.i + r.i))?,
                Instruction::IntSubInt => self.binaryop(|l, r| Ok(l.i - r.i))?,
                Instruction::IntMulInt => self.binaryop(|l, r| Ok(l.i * r.i))?,
                Instruction::IntDivInt => self.binaryop(|l, r| {
                    if r.i == 0 {
                        Err(Box::new(ErrDiv0))
                    } else {
                        Ok(l.i / r.i)
                    }
                })?,
                Instruction::FloatAddFloat => self.binaryop(|l, r| Ok(l.f + r.f))?,
                Instruction::FloatSubFloat => self.binaryop(|l, r| Ok(l.f - r.f))?,
                Instruction::FloatMulFloat => self.binaryop(|l, r| Ok(l.f * r.f))?,
                Instruction::FloatDivFloat => self.binaryop(|l, r| Ok(l.f / r.f))?,

                Instruction::UintToInt => self.convertop(|v| (v.u as i64, RuntimeType::Int)),
                Instruction::UintToFloat => self.convertop(|v| (v.u as f64, RuntimeType::Float)),
                Instruction::IntToUint => self.convertop(|v| (v.i as u64, RuntimeType::Uint)),
                Instruction::IntToFloat => self.convertop(|v| (v.i as f64, RuntimeType::Float)),
                Instruction::FloatToUint => self.convertop(|v| (v.f as u64, RuntimeType::Uint)),
                Instruction::FloatToInt => self.convertop(|v| (v.f as i64, RuntimeType::Int)),
            }
        }
    }

    fn binaryop<T: Into<Value>, F: FnOnce(Value, Value) -> Result<T>>(
        &mut self,
        f: F,
    ) -> Result<()> {
        match self.eval_stack.pop() {
            None => Ok(()),
            Some((v, _)) => match self.eval_stack.last_mut() {
                None => Ok(()),
                Some((t, _)) => {
                    *t = f(*t, v)?.into();
                    Ok(())
                }
            },
        }
    }

    fn convertop<T: Into<Value>, F: FnOnce(Value) -> (T, RuntimeType)>(&mut self, f: F) {
        match self.eval_stack.last_mut() {
            None => (),
            Some((tv, tt)) => {
                let (i, t) = f(*tv);
                (*tv, *tt) = (i.into(), t)
            }
        }
    }
}

pub struct StackFrame {
    pub code_value: *mut CodeValue,
    pub code: &'static [Instruction],
    pub fp: usize,
    pub ftype: *mut FrameType,
}

pub struct FrameType {
    pub alive: bool,
    pub field_types: Box<[RuntimeType]>,
}

pub struct Function {
    pub alive: bool,
    pub code: *mut CodeValue,
    pub ftype: *mut FrameType,
    pub code_offset: usize,
    pub argc: u32,
    pub retc: u32,
}

#[derive(Debug)]
pub struct ErrDiv0;

impl std::fmt::Display for ErrDiv0 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "division by 0")
    }
}

impl std::error::Error for ErrDiv0 {}
