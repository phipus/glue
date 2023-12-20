use std::{
    error::Error,
    sync::{Arc, Mutex},
};

use crate::{
    gc::Collector,
    instr::Instruction,
    rtype::RuntimeType,
    rvalue::{TypedValue, Value},
};

pub struct Runtime {}

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

pub struct Thread {
    pub locals: Vec<Value>,
    pub call_stack: Vec<StackFrame>,
    pub eval_stack: Vec<TypedValue>,
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

    pub unsafe fn push_function(&mut self, func: *mut Function) {
        self.call_stack.push(StackFrame {
            function: func,
            code_offset: (*func).code_offset,
            fp: self.locals.len(),
        });
        for rtype in (*func).ftype.field_types.iter() {
            self.locals.push(Value::default(*rtype))
        }
    }

    pub unsafe fn eval(&mut self) -> Result<()> {
        let mut frame = self
            .call_stack
            .pop()
            .expect("eval called with no frame on the stack");

        let mut code = &*(*frame.function).code;
        let mut last_field = None;

        loop {
            let instr = code[frame.code_offset];
            frame.code_offset += 1;

            match instr {
                Instruction::Bool(b) => self.eval_stack.push(b.into()),
                Instruction::Uint(u) => self.eval_stack.push(u.into()),
                Instruction::Int(i) => self.eval_stack.push(i.into()),
                Instruction::Float(f) => self.eval_stack.push(f.into()),
                Instruction::Object(rtype) => {
                    let obj = {
                        let mut gc = self.gc.lock().unwrap();
                        gc.new_object(rtype)
                    };
                    self.eval_stack.push(TypedValue {
                        value: Value { o: obj },
                        rtype: RuntimeType::Object,
                    });
                }
                Instruction::InterfaceVT(_) => todo!(),
                Instruction::PushLocal(offset, rtype) => {
                    self.eval_stack.push(TypedValue {
                        value: self.locals[frame.fp + offset as usize],
                        rtype,
                    });
                }
                Instruction::PopLocal(offset) => match self.eval_stack.pop() {
                    None => (),
                    Some(v) => self.locals[frame.fp + offset as usize] = v.value,
                },
                Instruction::GetField(offset, rtype) => {
                    match self.eval_stack.pop() {
                        Some(v) => {
                            // assert_eq!(t, RuntimeType::Object);
                            last_field = Some(v);
                            self.eval_stack.push(TypedValue {
                                value: (*v.value.o).fields[offset as usize],
                                rtype,
                            });
                        }
                        None => (),
                    }
                }
                Instruction::GetNextField(offset, rtype) => match last_field {
                    None => (),
                    Some(v) => self.eval_stack.push(TypedValue {
                        value: (*v.value.o).fields[offset as usize],
                        rtype,
                    }),
                },
                Instruction::SetField(offset) => match self.eval_stack.pop() {
                    None => (),
                    Some(v) => match self.eval_stack.last() {
                        None => (),
                        Some(o) => (*o.value.o).fields[offset as usize] = v.value,
                    },
                },
                Instruction::PopDiscard => {
                    self.eval_stack.pop();
                }
                Instruction::Call(func) => {
                    self.call_stack.push(frame);
                    self.push_function(func);

                    frame = self.call_stack.pop().unwrap();
                    code = &*(*frame.function).code;

                    let len = self.eval_stack.len();
                    for i in 1..=(*frame.function).argc as usize {
                        let arg = self.eval_stack.pop().expect("glue: not enough arguments");
                        self.locals[frame.fp + len - i] = arg.value;
                    }
                }
                Instruction::NativeCall(func) => func(&mut self.locals[frame.fp..], &mut self.eval_stack),
                Instruction::Ret => match self.call_stack.pop() {
                    None => return Ok(()),
                    Some(prev_frame) => {
                        self.locals.truncate(frame.fp);
                        frame = prev_frame;
                        code = &*(*frame.function).code;
                    }
                },

                Instruction::BoolAndBool => self.binaryop(|l, r| l.b && r.b),
                Instruction::BoolOrBool => self.binaryop(|l, r| l.b || r.b),
                Instruction::BoolEqBool => self.binaryop(|l, r| l.b == r.b),
                Instruction::BoolNeBool => self.binaryop(|l, r| l.b != r.b),

                Instruction::UintAddUint => self.binaryop(|l, r| l.u + r.u),
                Instruction::UintSubUint => self.binaryop(|l, r| l.u - r.u),
                Instruction::UintMulUint => self.binaryop(|l, r| l.u * r.u),
                Instruction::UintDivUint => self.binaryop_result(|l, r| {
                    if r.u == 0 {
                        Err(Box::new(ErrDiv0))
                    } else {
                        Ok(l.u / r.u)
                    }
                })?,
                Instruction::UintGtUint => self.binaryop(|l, r| l.u > r.u),
                Instruction::UintGeUint => self.binaryop(|l, r| l.u >= r.u),
                Instruction::UintEqUint => self.binaryop(|l, r| l.u == r.u),
                Instruction::UintNeUint => self.binaryop(|l, r| l.u != r.u),
                Instruction::UintLeUint => self.binaryop(|l, r| l.u <= r.u),
                Instruction::UintLtUint => self.binaryop(|l, r| l.u < r.u),

                Instruction::IntAddInt => self.binaryop(|l, r| l.i + r.i),
                Instruction::IntSubInt => self.binaryop(|l, r| l.i - r.i),
                Instruction::IntMulInt => self.binaryop(|l, r| l.i * r.i),
                Instruction::IntDivInt => self.binaryop_result(|l, r| {
                    if r.i == 0 {
                        Err(Box::new(ErrDiv0))
                    } else {
                        Ok(l.i / r.i)
                    }
                })?,
                Instruction::IntGtInt => self.binaryop(|l, r| l.i > r.i),
                Instruction::IntGeInt => self.binaryop(|l, r| l.i >= r.i),
                Instruction::IntEqInt => self.binaryop(|l, r| l.i == r.i),
                Instruction::IntNeInt => self.binaryop(|l, r| l.i != r.i),
                Instruction::IntLeInt => self.binaryop(|l, r| l.i <= r.i),
                Instruction::IntLtInt => self.binaryop(|l, r| l.i < r.i),

                Instruction::FloatAddFloat => self.binaryop(|l, r| l.f + r.f),
                Instruction::FloatSubFloat => self.binaryop(|l, r| l.f - r.f),
                Instruction::FloatMulFloat => self.binaryop(|l, r| l.f * r.f),
                Instruction::FloatDivFloat => self.binaryop(|l, r| l.f / r.f),
                Instruction::FloatGtFloat => self.binaryop(|l, r| l.f > r.f),
                Instruction::FloatGeFloat => self.binaryop(|l, r| l.f >= r.f),
                Instruction::FloatEqFloat => self.binaryop(|l, r| l.f == r.f),
                Instruction::FloatNeFloat => self.binaryop(|l, r| l.f != r.f),
                Instruction::FloatLeFloat => self.binaryop(|l, r| l.f <= r.f),
                Instruction::FloatLtFloat => self.binaryop(|l, r| l.f < r.f),

                Instruction::UintToInt => self.convertop(|v| v.u as i64),
                Instruction::UintToFloat => self.convertop(|v| v.u as f64),
                Instruction::IntToUint => self.convertop(|v| v.i as u64),
                Instruction::IntToFloat => self.convertop(|v| v.i as f64),
                Instruction::FloatToUint => self.convertop(|v| v.f as u64),
                Instruction::FloatToInt => self.convertop(|v| v.f as i64),

                Instruction::NegInt => self.negop(|v| -v.i),
                Instruction::NegFloat => self.negop(|v| -v.f),
                Instruction::NegBool => self.negop(|v| !v.b),

                Instruction::Jump(offset) => frame.code_offset = (frame.code_offset as isize + offset) as usize,
                Instruction::JumpTrue(offset) => match self.eval_stack.pop() {
                    Some(value) => {
                        if value.value.b {
                            frame.code_offset = (frame.code_offset as isize + offset) as usize
                        }
                    }
                    None => (),
                },
                Instruction::JumpFalse(offset) => match self.eval_stack.pop() {
                    Some(value) => {
                        if !value.value.b {
                            frame.code_offset = (frame.code_offset as isize + offset) as usize
                        }
                    }
                    None => (),
                },
            }
        }
    }

    fn binaryop_result<T: Into<TypedValue>, F: FnOnce(Value, Value) -> Result<T>>(
        &mut self,
        f: F,
    ) -> Result<()> {
        match self.eval_stack.pop() {
            None => Ok(()),
            Some(v) => match self.eval_stack.last_mut() {
                None => Ok(()),
                Some(r) => {
                    *r = f(r.value, v.value)?.into();
                    Ok(())
                }
            },
        }
    }

    fn binaryop<T: Into<TypedValue>, F: FnOnce(Value, Value) -> T>(&mut self, f: F) {
        match self.eval_stack.pop() {
            None => (),
            Some(v) => match self.eval_stack.last_mut() {
                None => (),
                Some(r) => {
                    *r = f(r.value, v.value).into();
                }
            },
        }
    }

    fn convertop<T: Into<TypedValue>, F: FnOnce(Value) -> T>(&mut self, f: F) {
        match self.eval_stack.last_mut() {
            None => (),
            Some(t) => {
                *t = f(t.value).into();
            }
        }
    }

    fn negop<T: Into<Value>, F: FnOnce(Value) -> T>(&mut self, f: F) {
        match self.eval_stack.last_mut() {
            None => (),
            Some(v) => v.value = f(v.value).into(),
        }
    }
}

pub struct StackFrame {
    pub function: *mut Function,
    pub code_offset: usize,
    pub fp: usize,
}

pub struct FrameType {
    pub field_types: Box<[RuntimeType]>,
}

pub struct Function {
    pub alive: bool,
    pub code: Box<[Instruction]>,
    pub ftype: FrameType,
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
