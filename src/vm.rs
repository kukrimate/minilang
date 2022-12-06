use crate::ast;
use crate::compile::*;
use crate::gc::*;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

/// Number of instructions between GC cycles

const GC_THRESHOLD: usize = 10000;

/// Runtime result

pub type VRes = Result<Val, VErr>;

/// Runtime values

#[derive(Clone)]
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

type BuiltinFn = fn(&mut Vm, Vec<Val>) -> VRes;

impl GcObj for Val {
  fn visit_children(&self, gc: &mut GcHeap) {
    match self {
      Val::Nil            => (),
      Val::Bool(..)       => (),
      Val::Int(..)        => (),
      Val::Str(..)        => (),
      Val::Func(env, ..)  => gc.mark(*env),
      Val::Ctor(env, ..)  => gc.mark(*env),
      Val::Obj(env, ..)   => gc.mark(*env),
      Val::Builtin(..)    => ()
    }
  }
}

impl PartialEq for Val {
  fn eq(&self, rhs: &Val) -> bool {
    match (self, rhs) {
      (Val::Nil, Val::Nil)          => { true }
      (Val::Bool(a), Val::Bool(b))  => { a == b }
      (Val::Int(a), Val::Int(b))    => { a == b }
      (Val::Str(a), Val::Str(b))    => { a == b }
      (Val::Func(env1, params1, ip1), Val::Func(env2, params2, ip2)) => {
        env1.ptr_eq(*env2) && params1 == params2 && ip1 == ip2
      }
      (Val::Ctor(env1, fields1, methods1), Val::Ctor(env2, fields2, methods2)) => {
        env1.ptr_eq(*env2) && fields1 == fields2 && methods1 == methods2
      }
      (Val::Obj(env1), Val::Obj(env2)) => {
        env1.ptr_eq(*env2)
      }
      (Val::Builtin(fn1, cnt1), Val::Builtin(fn2, cnt2)) => {
        *fn1 as usize == *fn2 as usize && cnt1 == cnt2
      }
      _ => false
    }
  }
}

impl Eq for Val {}


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
  names: HashMap<String, Val>
}

impl Env {
  fn root(names: HashMap<String, Val>) -> Env {
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

  fn insert(&mut self, id: &str, val: Val) -> Result<(), VErr> {
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

  fn set(&mut self, id: &str, val: Val) -> Result<(), VErr> {
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
  fn visit_children(&self, gc: &mut GcHeap) {
    if let Some(parent) = self.parent {
      gc.mark(parent)
    }
    for (_, val) in self.names.iter() {
      val.visit_children(gc)
    }
  }
}

/// Builtin implementations

fn builtin_print(_: &mut Vm, args: Vec<Val>) -> VRes {
  // Convert to string
  match &args[0] {
    Val::Nil      => println!("nil"),
    Val::Bool(b)  => println!("{}", b),
    Val::Int(i)   => println!("{}", i),
    Val::Str(s)   => println!("{}", s),
                _ => todo!()
  }
  // Print returns nothing
  Ok(Val::Nil)
}

fn builtin_time(_: &mut Vm, _: Vec<Val>) -> VRes {
  // Get current time as seconds
  let now = BigInt::from(SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .unwrap().as_secs());
  // Return as GC value
  Ok(Val::Int(now))
}

/// AST interpreter

pub struct Vm {
  // Garbage collector
  gc: GcHeap,
  // Instruction table
  instructions: Vec<Insn>,
  // Value stack
  val_stack: Vec<Val>,
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
        (str::to_owned("print"), Val::Builtin(builtin_print, 1)),
        (str::to_owned("time"), Val::Builtin(builtin_time, 0))
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
          self.push_val(val.clone());
        }
        Insn::Func(params, func_ip) => {
          self.push_val(Val::Func(self.peek_env(), params.clone(), func_ip.clone()));
        }
        Insn::Ctor(fields, methods) => {
          self.push_val(Val::Ctor(self.peek_env(), fields.clone(), methods.clone()));
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
          match self.pop_val() {
            Val::Obj(env) => match env.names.get(id) {
              Some(val) => { self.push_val(val.clone()); }
              None => return Err(VErr::WrongField(id.clone()))
            }
            _ => return Err(VErr::WrongType)
          }
        }
        Insn::StoreField(id) => {
          match self.pop_val() {
            Val::Obj(mut env) => match env.names.get_mut(id) {
              Some(dest) => { *dest = self.pop_val(); }
              None => return Err(VErr::WrongField(id.clone()))
            }
            _ => return Err(VErr::WrongType)
          }
        }
        Insn::Call(arg_cnt) => {
          match self.pop_val() {
            Val::Func(env, params, func_ip) => {
              if *arg_cnt != params.len() {
                return Err(VErr::WrongArgs)
              }
              // Create environment
              let mut env = self.alloc(Env::child(env));
              for id in params.iter().rev() {
                env.insert(id, self.pop_val())?;
              }
              self.push_env(env);
              // Push return address and jump to function
              self.call_stack.push(cur_ip);
              cur_ip = func_ip;
            }
            Val::Ctor(env, fields, methods) => {
              if *arg_cnt != fields.len() {
                return Err(VErr::WrongArgs)
              }
              // Create environment
              let mut env = self.alloc(Env::child(env));
              for id in fields.iter().rev() {
                env.insert(id, self.pop_val())?;
              }
              for (id, params, method_ip) in methods.iter() {
                let val = Val::Func(env, params.clone(), *method_ip);
                env.insert(id, val)?;
              }
              // Push type object
              let tmp = Val::Obj(env);
              self.push_val(tmp);
            }
            Val::Builtin(native_fn, param_cnt) => {
              if *arg_cnt != param_cnt {
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
          match self.pop_val() {
            Val::Bool(true)   => { cur_ip = *new_ip; }
            Val::Bool(false)  => {}
                            _ => return Err(VErr::WrongType)
          }
        }
        Insn::JumpFalse(new_ip) => {
          match self.pop_val() {
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
  fn push_val(&mut self, val: Val) {
    self.val_stack.push(val);
  }

  #[inline(always)]
  fn peek_val(&self) -> Val {
    self.val_stack.last().unwrap().clone()
  }

  #[inline(always)]
  fn pop_val(&mut self) -> Val {
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
  fn eval_un(&mut self, op: &ast::UnOp, arg: Val) -> VRes {
    match (op, arg) {
      (ast::UnOp::Neg, Val::Int(i))   => Ok(Val::Int(-i)),
      (ast::UnOp::Not, Val::Bool(b))  => Ok(Val::Bool(!b)),

                                    _ => Err(VErr::WrongType)
    }
  }

  #[inline(always)]
  fn eval_bin(&mut self, op: &ast::BinOp, lhs: Val, rhs: Val) -> VRes {
    use ast::BinOp::*;
    match (op, lhs, rhs) {
      // Int
      (Add, Val::Int(a), Val::Int(b))   => Ok(Val::Int(a + b)),
      (Sub, Val::Int(a), Val::Int(b))   => Ok(Val::Int(a - b)),
      (Mul, Val::Int(a), Val::Int(b))   => Ok(Val::Int(a * b)),
      (Div|Mod, Val::Int(_), Val::Int(b)) if b == BigInt::from(0i32) => {
        Err(VErr::DivideByZero)
      },
      (Div, Val::Int(a), Val::Int(b))   => Ok(Val::Int(a / b)),
      (Mod, Val::Int(a), Val::Int(b))   => Ok(Val::Int(a % b)),
      (Lt, Val::Int(a), Val::Int(b))    => Ok(Val::Bool(a < b)),
      (Gt, Val::Int(a), Val::Int(b))    => Ok(Val::Bool(a > b)),
      (Le, Val::Int(a), Val::Int(b))    => Ok(Val::Bool(a <= b)),
      (Ge, Val::Int(a), Val::Int(b))    => Ok(Val::Bool(a >= b)),

      // Str
      (Add, Val::Str(a), Val::Str(b))   => Ok(Val::Str(format!("{}{}", a, b))),
      (Lt, Val::Str(a), Val::Str(b))    => Ok(Val::Bool(a < b)),
      (Gt, Val::Str(a), Val::Str(b))    => Ok(Val::Bool(a > b)),
      (Le, Val::Str(a), Val::Str(b))    => Ok(Val::Bool(a <= b)),
      (Ge, Val::Str(a), Val::Str(b))    => Ok(Val::Bool(a >= b)),

      // Generic object comparison
      (Eq, v1, v2)                      => Ok(Val::Bool(v1 == v2)),
      (Ne, v1, v2)                      => Ok(Val::Bool(v1 != v2)),

                                      _ => Err(VErr::WrongType)
    }
  }

  #[inline(always)]
  fn maybe_gc(&mut self) {
    self.insn_cnt += 1;
    if self.insn_cnt > GC_THRESHOLD {
      self.insn_cnt = 0;
      for val in self.val_stack.iter() {
        val.visit_children(&mut self.gc);
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
    self.instructions.push(Insn::Const(val));
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
