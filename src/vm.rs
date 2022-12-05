use crate::ast;
use crate::compile::*;
use crate::gc::*;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

/// Number of instructions between GC cycles

const GC_THRESHOLD: usize = 10000;

/// Runtime result

pub type VRes = Result<VRef, VErr>;

/// Runtime values

pub type VRef = GcPtr<Val>;

pub enum Val {
  Nil,
  Bool(bool),
  Int(BigInt),
  Str(String),
  Func(GcPtr<Env>, Vec<String>, usize),
  Ctor(GcPtr<Env>, Vec<String>, Vec<(String, Vec<String>, usize)>),
  Obj(GcPtr<Env>),
  Builtin(BuiltinFn, usize)
}

impl PartialEq for Val {
  fn eq(&self, rhs: &Val) -> bool {
    match (self, rhs) {
      (Val::Nil, Val::Nil) => true,
      (Val::Bool(a), Val::Bool(b)) => a == b,
      (Val::Int(a), Val::Int(b)) => a == b,
      (Val::Str(a), Val::Str(b)) => a == b,
      _ => false
    }
  }
}

impl Eq for Val {}

impl core::hash::Hash for Val {
  fn hash<H: core::hash::Hasher>(&self, h: &mut H) {
    match self {
      Val::Nil      => { 0.hash(h); }
      Val::Bool(a)  => { 1.hash(h); a.hash(h); }
      Val::Int(a)   => { 2.hash(h); a.hash(h); }
      Val::Str(a)   => { 3.hash(h); a.hash(h); }
      _ => todo!()
    }
  }
}

type BuiltinFn = fn(&mut Vm, Vec<VRef>) -> VRes;

impl GcObj for Val {
  fn visit_children(&self, f: fn(GcPtr<dyn GcObj>)) {
    match self {
      Val::Nil            => (),
      Val::Bool(..)       => (),
      Val::Int(..)        => (),
      Val::Str(..)        => (),
      Val::Func(env, ..)  => f(*env),
      Val::Ctor(env, ..)  => f(*env),
      Val::Obj(env, ..)   => f(*env),
      Val::Builtin(..)    => ()
    }
  }
}

/// Runtime error

#[derive(Debug)]
pub enum VErr {
  UnknownId(String),
  RedefinedId(String),
  WrongType,
  DivideByZero,
  WrongArgs,
  WrongField(String)
}

impl std::fmt::Display for VErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      VErr::UnknownId(id) => write!(f, "Unknown identifier {}", id),
      VErr::RedefinedId(id) => write!(f, "Re-definition of {}", id),
      VErr::WrongType => write!(f, "Type error"),
      VErr::DivideByZero => write!(f, "Division by zero"),
      VErr::WrongArgs => write!(f, "Wrong number of argumenbts"),
      VErr::WrongField(id) => write!(f, "Unknown field {}", id),
    }
  }
}

impl std::error::Error for VErr {}

/// Name resolution environment

pub struct Env {
  parent: Option<GcPtr<Env>>,
  names: HashMap<String, VRef>
}

impl Env {
  fn root(names: HashMap<String, VRef>) -> Env {
    Self {
      parent: None, names
    }
  }

  fn child(parent: GcPtr<Env>) -> Env {
    Self {
      parent: Some(parent),
      names: HashMap::new()
    }
  }

  fn insert(&mut self, id: &str, val: VRef) -> Result<(), VErr> {
    match self.names.insert(str::to_owned(id), val) {
      Some(..) => Err(VErr::RedefinedId(str::to_owned(id))),
      None => Ok(())
    }
  }

  fn get(&self, id: &str) -> VRes {
    if let Some(val) = self.names.get(id) {
      Ok(val.clone())
    } else if let Some(parent) = &self.parent {
      parent.get(id)
    } else {
      Err(VErr::UnknownId(str::to_owned(id)))
    }
  }

  fn set(&mut self, id: &str, val: VRef) -> Result<(), VErr> {
    if let Some(dest) = self.names.get_mut(id) {
      *dest = val;
      Ok(())
    } else if let Some(parent) = &mut self.parent {
      parent.set(id, val)
    } else {
      Err(VErr::UnknownId(str::to_owned(id)))
    }
  }
}

impl GcObj for Env {
  fn visit_children(&self, f: fn(GcPtr<dyn GcObj>)) {
    if let Some(parent) = self.parent {
      f(parent)
    }
    for (_, val) in self.names.iter() {
      f(*val)
    }
  }
}

/// Builtin implementations

fn builtin_print(vm: &mut Vm, args: Vec<VRef>) -> VRes {
  // Convert to string
  match &*args[0] {
    Val::Nil      => println!("nil"),
    Val::Bool(b)  => println!("{}", b),
    Val::Int(i)   => println!("{}", i),
    Val::Str(s)   => println!("{}", s),
                _ => todo!()
  }
  // Print returns nothing
  Ok(vm.alloc(Val::Nil))
}

fn builtin_time(vm: &mut Vm, _: Vec<VRef>) -> VRes {
  // Get current time as seconds
  let now = BigInt::from(SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .unwrap().as_secs());
  // Return as GC value
  Ok(vm.alloc(Val::Int(now)))
}

/// AST interpreter

pub struct Vm {
  // Garbage collector
  gc: GcHeap,
  // Constant pool
  const_pool: HashMap<GcPtr<Val>, ()>,
  // Instruction table
  instructions: Vec<Insn>,
  // Value stack
  val_stack: Vec<GcPtr<Val>>,
  // Environment stack
  env_stack: Vec<GcPtr<Env>>,
  // Function call stack
  call_stack: Vec<usize>,
  // Instruction counter
  insn_cnt: usize
}

impl Vm {
  pub fn new() -> Self {
    Self {
      gc: GcHeap::new(),
      const_pool: HashMap::new(),
      instructions: Vec::new(),
      val_stack: Vec::new(),
      env_stack: Vec::new(),
      call_stack: Vec::new(),
      insn_cnt: 0
    }
  }

  pub fn execute(&mut self) -> Result<(), VErr> {
    // Root environment
    {
      let builtins = HashMap::from([
        (str::to_owned("print"), self.alloc(Val::Builtin(builtin_print, 1))),
        (str::to_owned("time"), self.alloc(Val::Builtin(builtin_time, 0)))
      ]);
      let root_env = self.alloc(Env::root(builtins));
      self.push_env(root_env);
    }

    // Main interpreter loop
    let mut cur_ip = 0;

    loop {
      // Perform garbage collection if needed
      self.maybe_gc();

      // Interpret next instruction
      let insn = unsafe { &*(self.instructions.get_unchecked(cur_ip) as *const _) };
      cur_ip += 1;

      match insn {
        Insn::Const(val) => {
          self.push_val(*val);
        }
        Insn::Func(params, func_ip) => {
          let val = self.alloc(Val::Func(self.peek_env(), params.clone(), func_ip.clone()));
          self.push_val(val);
        }
        Insn::Ctor(fields, methods) => {
          let val = self.alloc(Val::Ctor(self.peek_env(), fields.clone(), methods.clone()));
          self.push_val(val);
        }
        Insn::Load(id) => {
          self.push_val(self.peek_env().get(id)?);
        }
        Insn::Store(id) => {
          let val = self.pop_val();
          self.peek_env().set(id, val)?;
        }
        Insn::Declare(id) => {
          let val = self.pop_val();
          self.peek_env().insert(id, val)?;
        }
        Insn::LoadField(id) => {
          match &*self.pop_val() {
            Val::Obj(env) => match env.names.get(id) {
              Some(val) => { self.push_val(*val); }
              None => return Err(VErr::WrongField(id.clone()))
            }
            _ => return Err(VErr::WrongType)
          }
        }
        Insn::StoreField(id) => {
          match &*self.pop_val() {
            Val::Obj(mut env) => match env.names.get_mut(id) {
              Some(dest) => { *dest = self.pop_val(); }
              None => return Err(VErr::WrongField(id.clone()))
            }
            _ => return Err(VErr::WrongType)
          }
        }
        Insn::Call(arg_cnt) => {
          let func = self.pop_val();
          match &*func {
            Val::Func(env, params, func_ip) => {
              if *arg_cnt != params.len() {
                return Err(VErr::WrongArgs)
              }
              // Create environment
              let mut env = self.alloc(Env::child(*env));
              for id in params.iter().rev() {
                env.insert(id, self.pop_val())?;
              }
              self.push_env(env);
              // Push return address and jump to function
              self.call_stack.push(cur_ip);
              cur_ip = *func_ip;
            }
            Val::Ctor(env, fields, methods) => {
              if *arg_cnt != fields.len() {
                return Err(VErr::WrongArgs)
              }
              // Create environment
              let mut env = self.alloc(Env::child(*env));
              for id in fields.iter().rev() {
                env.insert(id, self.pop_val())?;
              }
              for (id, params, method_ip) in methods.iter() {
                let val = self.alloc(Val::Func(env, params.clone(), *method_ip));
                env.insert(id, val)?;
              }
              // Push type object
              let tmp = self.alloc(Val::Obj(env));
              self.push_val(tmp);
            }
            Val::Builtin(native_fn, param_cnt) => {
              if arg_cnt != param_cnt {
                return Err(VErr::WrongArgs)
              }
              let args = self.val_stack.split_off(self.val_stack.len() - arg_cnt);
              let tmp = native_fn(self, args)?;
              self.push_val(tmp);
            }
            _ => return Err(VErr::WrongType)
          }
        }
        Insn::Un(op) => {
          let arg = self.pop_val();
          let tmp = self.eval_un(op, arg)?;
          self.push_val(tmp);
        }
        Insn::Bin(op) => {
          let rhs = self.pop_val();
          let lhs = self.pop_val();
          let tmp = self.eval_bin(op, lhs, rhs)?;
          self.push_val(tmp);
        }
        Insn::Jump(new_ip) => {
          cur_ip = *new_ip;
        }
        Insn::JumpTrue(new_ip) => {
          match &*self.pop_val() {
            Val::Bool(true)   => { cur_ip = *new_ip; }
            Val::Bool(false)  => {}
                            _ => return Err(VErr::WrongType)
          }
        }
        Insn::JumpFalse(new_ip) => {
          match &*self.pop_val() {
            Val::Bool(true)   => {}
            Val::Bool(false)  => { cur_ip = *new_ip; }
                            _ => return Err(VErr::WrongType)
          }
        }
        Insn::Enter => {
          let env = self.alloc(Env::child(self.peek_env()));
          self.push_env(env);
        }
        Insn::Exit => {
          self.pop_env();
        }
        Insn::Dup => {
          self.push_val(self.peek_val());
        }
        Insn::Discard => {
          self.pop_val();
        }
        Insn::Return => {
          self.pop_env();
          cur_ip = self.call_stack.pop().unwrap();
        }
        Insn::Terminate => {
          break;
        }
      }
    }

    Ok(())
  }

  #[inline(always)]
  fn push_val(&mut self, val: GcPtr<Val>) {
    self.val_stack.push(val);
  }

  #[inline(always)]
  fn peek_val(&self) -> GcPtr<Val> {
    *self.val_stack.last().unwrap()
  }

  #[inline(always)]
  fn pop_val(&mut self) -> GcPtr<Val> {
    self.val_stack.pop().unwrap()
  }

  #[inline(always)]
  fn push_env(&mut self, env: GcPtr<Env>) {
    self.env_stack.push(env);
  }

  #[inline(always)]
  fn peek_env(&self) -> GcPtr<Env> {
    *self.env_stack.last().unwrap()
  }

  #[inline(always)]
  fn pop_env(&mut self) -> GcPtr<Env> {
    self.env_stack.pop().unwrap()
  }

  #[inline(always)]
  fn eval_un(&mut self, op: &ast::UnOp, arg: VRef) -> VRes {
    match (op, &*arg) {
      (ast::UnOp::Neg, Val::Int(i))   => Ok(self.alloc(Val::Int(-i))),
      (ast::UnOp::Not, Val::Bool(b))  => Ok(self.alloc(Val::Bool(!b))),

                                    _ => Err(VErr::WrongType)
    }
  }

  #[inline(always)]
  fn eval_bin(&mut self, op: &ast::BinOp, lhs: VRef, rhs: VRef) -> VRes {
    use ast::BinOp::*;
    match (op, &*lhs, &*rhs) {
      // Nil
      (Eq, Val::Nil, Val::Nil)          => Ok(self.alloc(Val::Bool(true))),
      (Ne, Val::Nil, Val::Nil)          => Ok(self.alloc(Val::Bool(false))),

      // Bool
      (Eq, Val::Bool(a), Val::Bool(b))  => Ok(self.alloc(Val::Bool(a == b))),
      (Ne, Val::Bool(a), Val::Bool(b))  => Ok(self.alloc(Val::Bool(a != b))),

      // Int
      (Add, Val::Int(a), Val::Int(b))   => Ok(self.alloc(Val::Int(a + b))),
      (Sub, Val::Int(a), Val::Int(b))   => Ok(self.alloc(Val::Int(a - b))),
      (Mul, Val::Int(a), Val::Int(b))   => Ok(self.alloc(Val::Int(a * b))),
      (Div|Mod, Val::Int(_), Val::Int(b)) if b == &BigInt::from(0i32) => {
        Err(VErr::DivideByZero)
      },
      (Div, Val::Int(a), Val::Int(b))   => Ok(self.alloc(Val::Int(a / b))),
      (Mod, Val::Int(a), Val::Int(b))   => Ok(self.alloc(Val::Int(a % b))),
      (Eq, Val::Int(a), Val::Int(b))    => Ok(self.alloc(Val::Bool(a == b))),
      (Ne, Val::Int(a), Val::Int(b))    => Ok(self.alloc(Val::Bool(a != b))),
      (Lt, Val::Int(a), Val::Int(b))    => Ok(self.alloc(Val::Bool(a < b))),
      (Gt, Val::Int(a), Val::Int(b))    => Ok(self.alloc(Val::Bool(a > b))),
      (Le, Val::Int(a), Val::Int(b))    => Ok(self.alloc(Val::Bool(a <= b))),
      (Ge, Val::Int(a), Val::Int(b))    => Ok(self.alloc(Val::Bool(a >= b))),

      // Str
      (Add, Val::Str(a), Val::Str(b))   => Ok(self.alloc(Val::Str(format!("{}{}", a, b)))),
      (Eq, Val::Str(a), Val::Str(b))    => Ok(self.alloc(Val::Bool(a == b))),
      (Ne, Val::Str(a), Val::Str(b))    => Ok(self.alloc(Val::Bool(a != b))),
      (Lt, Val::Str(a), Val::Str(b))    => Ok(self.alloc(Val::Bool(a < b))),
      (Gt, Val::Str(a), Val::Str(b))    => Ok(self.alloc(Val::Bool(a > b))),
      (Le, Val::Str(a), Val::Str(b))    => Ok(self.alloc(Val::Bool(a <= b))),
      (Ge, Val::Str(a), Val::Str(b))    => Ok(self.alloc(Val::Bool(a >= b))),

      // Generic object comparison
      (Eq, _, _)                        => Ok(self.alloc(Val::Bool(lhs.ptr_eq(rhs)))),
      (Ne, _, _)                        => Ok(self.alloc(Val::Bool(!lhs.ptr_eq(rhs)))),

                                      _ => Err(VErr::WrongType)
    }
  }

  #[inline(always)]
  fn maybe_gc(&mut self) {
    self.insn_cnt += 1;
    if self.insn_cnt > GC_THRESHOLD {
      self.insn_cnt = 0;
      for (cval, _) in self.const_pool.iter() {
        self.gc.mark(*cval);
      }
      for val in self.val_stack.iter() {
        self.gc.mark(*val);
      }
      for env in self.env_stack.iter() {
        self.gc.mark(*env);
      }
      self.gc.sweep();
    }
  }

  fn alloc<T: 'static + GcObj>(&mut self, val: T) -> GcPtr<T> {
    self.gc.alloc(val)
  }

  pub fn emit_const(&mut self, val: Val) {
    // Safety: this splits the mutably borrowed self into two non-overlapping
    // references to the garbage collector and to the constant pool
    let gc: &mut GcHeap = unsafe { &mut *(&mut self.gc as *mut _) };
    // Now using the raw entry API we can avoid a GC allocation when
    // a previous constant with the same value was found
    let (ptr, _) = self.const_pool
                        .raw_entry_mut()
                        .from_key(&val)
                        .or_insert_with(|| (gc.alloc(val), ()));
    // And finally we can emit an instruction that pushes the constant to the stack
    self.instructions.push(Insn::Const(*ptr));
  }

  pub fn emit(&mut self, insn: Insn) -> usize {
    let index = self.instructions.len();
    self.instructions.push(insn);
    index
  }

  pub fn insn_cnt(&self) -> usize {
    self.instructions.len()
  }

  pub fn patch(&mut self, at: usize, insn: Insn) {
    self.instructions[at] = insn;
  }
}
