use crate::ast;
use crate::gc::*;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};


/// Runtime result

pub type VRes = Result<VRef, VErr>;

/// Runtime values

pub type VRef = GcPtr<Val>;

pub enum Val {
  Nil,
  Bool(bool),
  Int(BigInt),
  Str(String),
  Func(GcPtr<Env>, *const Vec<String>, *const ast::Expr),
  TypeCtor(GcPtr<Env>, *const Vec<String>, *const Vec<ast::FuncDef>),
  TypeObj(GcPtr<Env>),
  Builtin(BuiltinFn, usize)
}

type BuiltinFn = fn(&mut Interpreter, Vec<VRef>) -> VRes;

impl GcObj for Val {
  fn visit_children(&self, f: fn(GcPtr<dyn GcObj>)) {
    match self {
      Val::Nil                => (),
      Val::Bool(..)           => (),
      Val::Int(..)            => (),
      Val::Str(..)            => (),
      Val::Func(env, ..)      => f(*env),
      Val::TypeCtor(env, ..)  => f(*env),
      Val::TypeObj(env, ..)   => f(*env),
      Val::Builtin(..)        => ()
    }
  }
}

/// Runtime error

pub enum VErr {
  UnknownId(String),
  RedefinedId(String),
  WrongType,
  DivideByZero,
  WrongAssign,
  WrongArgs,
  WrongField(String),
  WrongContinue,
  WrongBreak,
  WrongReturn(VRef)
}

impl fmt::Debug for VErr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      VErr::UnknownId(id) => write!(f, "Unknown identifier {}", id),
      VErr::RedefinedId(id) => write!(f, "Re-definition of {}", id),
      VErr::WrongType => write!(f, "Type error"),
      VErr::DivideByZero => write!(f, "Division by zero"),
      VErr::WrongAssign => write!(f, "Invalid assignment"),
      VErr::WrongArgs => write!(f, "Wrong number of argumenbts"),
      VErr::WrongField(id) => write!(f, "Unknown field {}", id),
      VErr::WrongContinue => write!(f, "Continue outside loop"),
      VErr::WrongBreak => write!(f, "Break outside loop"),
      VErr::WrongReturn(..) => write!(f, "Return outside function")
    }
  }
}

/// Name resolution environment

pub struct Env {
  parent: Option<GcPtr<Env>>,
  names: HashMap<String, VRef>
}

impl Env {
  fn root(interp: &mut Interpreter) -> GcPtr<Env> {
    let names = HashMap::from([
      (str::to_owned("print"), interp.alloc(Val::Builtin(builtin_print, 1))),
      (str::to_owned("time"), interp.alloc(Val::Builtin(builtin_time, 0)))
    ]);
    interp.alloc(Self {
      parent: None,
      names: names
    })
  }

  fn child(interp: &mut Interpreter, parent: GcPtr<Env>) -> GcPtr<Env> {
    interp.alloc(Self {
      parent: Some(parent),
      names: HashMap::new()
    })
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

fn builtin_print(interp: &mut Interpreter, args: Vec<VRef>) -> VRes {
  // Convert to string
  match &*args[0] {
    Val::Nil    => println!("nil"),
    Val::Int(i) => println!("{}", i),
    Val::Str(s) => println!("{}", s),
              _ => todo!()
  }
  // Print returns nothing
  Ok(interp.alloc(Val::Nil))
}

fn builtin_time(interp: &mut Interpreter, _: Vec<VRef>) -> VRes {
  // Get current time as seconds
  let now = BigInt::from(SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .unwrap().as_secs());
  // Return as GC value
  Ok(interp.alloc(Val::Int(now)))
}

/// AST interpreter

pub struct Interpreter { gc: GcHeap, eval_cnt: usize }

impl Interpreter {
  pub fn new() -> Self {
    Self { gc: GcHeap::new(), eval_cnt: 0 }
  }

  pub fn execute(&mut self, ast::Program(exprs): &ast::Program) -> Result<(), VErr> {
    let env = Env::root(self);
    for expr in exprs.iter() {
      self.eval(env, expr)?;
    }
    Ok(())
  }

  fn eval(&mut self, mut env: GcPtr<Env>, expr: &ast::Expr) -> VRes {
    // Increase eval count
    self.eval_cnt += 1;
    // Run GC cycle
    if self.eval_cnt > 10000 {
      // self.gc.collect(env);
      self.eval_cnt = 0;
    }

    match expr {
      ast::Expr::Nil => Ok(self.alloc(Val::Nil)),
      ast::Expr::Bool(b) => Ok(self.alloc(Val::Bool(b.clone()))),
      ast::Expr::Int(i) => Ok(self.alloc(Val::Int(i.clone()))),
      ast::Expr::Str(s) => Ok(self.alloc(Val::Str(s.clone()))),
      ast::Expr::Id(id) => env.get(id),
      ast::Expr::Call(func, args) => {
        let v_func = self.eval(env, func)?;

        let mut v_args = Vec::new();
        for arg in args.iter() {
          v_args.push(self.eval(env, arg)?);
        }

        self.eval_call(v_func, v_args)
      }
      ast::Expr::Dot(obj, id) => {
        let v_obj = self.eval(env, obj)?;
        self.eval_get(v_obj, id)
      }
      ast::Expr::Un(op, arg) => {
        let v_arg = self.eval(env, arg)?;
        self.eval_un(op, v_arg)
      }
      ast::Expr::Bin(op, lhs, rhs) => {
        let v_lhs = self.eval(env, lhs)?;
        let v_rhs = self.eval(env, rhs)?;
        self.eval_bin(op, v_lhs, v_rhs)
      }
      ast::Expr::And(lhs, rhs) => {
        let v_lhs = self.eval(env, lhs)?;
        match &*v_lhs {
          // Need to eval RHS
          Val::Bool(true) => {
            let v_rhs = self.eval(env, rhs)?;
            match &*v_rhs {
              Val::Bool(..) => Ok(v_rhs),
                          _ => Err(VErr::WrongType)
            }
          }
          // Short circuit false
          Val::Bool(false) => Ok(v_lhs),
          // Type error
          _ => Err(VErr::WrongType)
        }
      }
      ast::Expr::Or(lhs, rhs) => {
        let v_lhs = self.eval(env, lhs)?;
        match &*v_lhs {
          // Short circuit true
          Val::Bool(true) => Ok(v_lhs),
          // Need to eval RHS
          Val::Bool(false) => {
            let v_rhs = self.eval(env, rhs)?;
            match &*v_rhs {
              Val::Bool(..) => Ok(v_rhs),
                          _ => Err(VErr::WrongType)
            }
          }
          // Type error
          _ => Err(VErr::WrongType)
        }
      }
      ast::Expr::Block(exprs) => {
        // Create block environment
        let block_env = Env::child(self, env);
        // Evaluate block body
        let mut val = self.alloc(Val::Nil);
        for expr in exprs.iter() {
          val = self.eval(block_env, expr)?;
        }
        Ok(val)
      }
      ast::Expr::Var(id, val) => {
        let tmp = self.eval(env, val)?;
        env.insert(id, tmp)?;
        Ok(self.alloc(Val::Nil))
      }
      ast::Expr::As(dest, val) => {
        let v_val = self.eval(env, val)?;
        match &**dest {
          // Assign to variable
          ast::Expr::Id(id) => {
            env.set(id, v_val)?;
            Ok(self.alloc(Val::Nil))
          }
          // Assign to object field
          ast::Expr::Dot(obj, id) => {
            let v_obj = self.eval(env, obj)?;
            self.eval_set(v_obj, id, v_val)
          }
          _ => return Err(VErr::WrongAssign)
        }
      }
      ast::Expr::Continue => {
        Err(VErr::WrongContinue)
      }
      ast::Expr::Break => {
        Err(VErr::WrongBreak)
      }
      ast::Expr::Return(val) => {
        let v_val = self.eval(env, val)?;
        Err(VErr::WrongReturn(v_val))
      }
      ast::Expr::If(cond, arg1, arg2) => {
        match &*self.eval(env, cond)? {
          Val::Bool(true)   => self.eval(env, arg1),
          Val::Bool(false)  => self.eval(env, arg2),
                          _ => Err(VErr::WrongType)
        }
      }
      ast::Expr::While(cond, body) => {
        loop {
          // Break if the condition is false
          match &*self.eval(env, cond)? {
            Val::Bool(true)   => (),
            Val::Bool(false)  => break,
                            _ => return Err(VErr::WrongType)
          };

          // Evaluate body
          match self.eval(env, body) {
            Ok(..)                    => (),
            Err(VErr::WrongContinue)  => (),
            Err(VErr::WrongBreak)     => break,
            Err(err)                  => return Err(err)
          }
        }
        Ok(self.alloc(Val::Nil))
      }
      ast::Expr::For(id, iter, body) => {
        let v_iter = self.eval(env, iter)?;
        loop {
          // Read next element from iterator
          let v_next = self.eval_get(v_iter, "next")?;
          let val = self.eval_call(v_next, vec![])?;
          if let Val::Nil = &*val {
            break
          }
          // Create environment
          let mut forenv = Env::child(self, env);
          forenv.insert(id, val)?;
          // Evaluate body
          match self.eval(forenv, body) {
            Ok(..)                    => (),
            Err(VErr::WrongContinue)  => (),
            Err(VErr::WrongBreak)     => break,
            Err(err)                  => return Err(err)
          }
        }
        Ok(self.alloc(Val::Nil))
      }
      ast::Expr::Func((id, params, body)) => {
        let v_func = self.alloc(Val::Func(env, params, &**body));
        env.insert(id, v_func)?;
        Ok(self.alloc(Val::Nil))
      }
      ast::Expr::Type(id, fields, methods) => {
        let v_type = self.alloc(Val::TypeCtor(env, fields, methods));
        env.insert(id, v_type)?;
        Ok(self.alloc(Val::Nil))
      }
      ast::Expr::Lambda(params, body) => {
        Ok(self.alloc(Val::Func(env, params, &**body)))
      }
    }
  }

  fn eval_call(&mut self, func: VRef, args: Vec<VRef>) -> VRes {
    // Evaluate call
    match &*func {
      Val::Func(def_env, params, body) => {
        // Make sure we've received the correct number of arguments
        if args.len() != unsafe{(**params).len()} {
          return Err(VErr::WrongArgs)
        }
        // Bind arguments to parameters
        let mut env = Env::child(self, *def_env);
        for (param, arg) in unsafe{(**params).iter()}.zip(args.into_iter()) {
          env.insert(param, arg)?;
        }
        // Evaluate body within argument scope
        match self.eval(env, unsafe{&**body}) {
          Err(VErr::WrongReturn(val)) => Ok(val),
          result => result,
        }
      }
      Val::TypeCtor(def_env, fields, methods) => {
        // Make sure there is a value for each field
        if args.len() != unsafe{(**fields).len()} {
          return Err(VErr::WrongArgs)
        }
        // Create fields
        let mut env = Env::child(self, *def_env);
        for (id, val) in unsafe{(**fields).iter()}.zip(args.into_iter()) {
          env.insert(id, val)?;
        }
        // Create methods
        for (id, params, body) in unsafe{(**methods).iter()} {
          let v_method = self.alloc(Val::Func(env, params, &**body));
          env.insert(id, v_method)?;
        }
        // Construct type object
        let obj = self.alloc(Val::TypeObj(env));
        // Add "self" binding
        env.insert("self", obj)?;
        // Yield object
        Ok(obj)
      }
      Val::Builtin(native_fn, param_cnt) => {
        if args.len() != *param_cnt {
          return Err(VErr::WrongArgs)
        }
        native_fn(self, args)
      }
      _ => Err(VErr::WrongType)
    }
  }

  fn eval_get(&mut self, obj: VRef, id: &str) -> VRes {
    match &*obj {
      Val::TypeObj(env) => {
        match env.names.get(id) {
          Some(val) => Ok(*val),
          None      => Err(VErr::WrongField(str::to_owned(id)))
        }
      }
      _ => Err(VErr::WrongType)
    }
  }

  fn eval_set(&mut self, mut obj: VRef, id: &str, val: VRef) -> VRes {
    match &mut *obj {
      Val::TypeObj(env) => {
        match env.names.get_mut(id) {
          Some(ptr) => { *ptr = val; Ok(self.alloc(Val::Nil)) }
          None      => Err(VErr::WrongField(str::to_owned(id)))
        }
      }
      _ => Err(VErr::WrongType)
    }
  }

  fn eval_un(&mut self, op: &ast::UnOp, arg: VRef) -> VRes {
    match (op, &*arg) {
      (ast::UnOp::Neg, Val::Int(i))   => Ok(self.alloc(Val::Int(-i))),
      (ast::UnOp::Not, Val::Bool(b))  => Ok(self.alloc(Val::Bool(!b))),

                                    _ => Err(VErr::WrongType)
    }
  }

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

  fn alloc<T: 'static + GcObj>(&mut self, val: T) -> GcPtr<T> {
    self.gc.alloc(val)
  }
}
