use crate::ast;
use crate::val::*;
use crate::gc::*;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

/// Function value

struct Func {
  env: GcPtr<Env>,
  params: *const Vec<String>,
  body: *const ast::Expr
}

impl Func {
  fn new(interp: &mut Interpreter, env: GcPtr<Env>, params: &Vec<String>, body: &ast::Expr) -> GcPtr<Func> {
    interp.gc.alloc(Func { env, params, body })
  }
}

impl VTrait for Func {
  fn eval_call(&self, interp: &mut Interpreter, args: Vec<VRef>) -> VRes {
    // Make sure we've received the correct number of arguments
    if args.len() != unsafe{(*self.params).len()} {
      return Err(VErr::WrongArgs)
    }
    // Bind arguments to parameters
    let mut env = Env::child(interp, self.env);
    for (param, arg) in unsafe{(*self.params).iter()}.zip(args.into_iter()) {
      env.insert(param, arg)?;
    }
    // Evaluate body within argument scope
    match interp.eval(env, unsafe{&*self.body}) {
      Err(VErr::WrongReturn(val)) => Ok(val),
      result => result,
    }
  }

  fn to_str(&self, interp: &mut Interpreter) -> Result<GcPtr<VStr>, VErr> {
    Ok(VStr::new(interp, format!("func {:p}", self as *const _)))
  }
}

impl GcObj for Func {
  fn visit_children(&self, f: fn(GcPtr<dyn GcObj>)) {
    f(self.env)
  }
}

/// Type constructor

struct TypeCtor {
  env: GcPtr<Env>,
  fields: *const Vec<String>,
  methods: *const Vec<ast::FuncDef>
}

impl TypeCtor {
  fn new(interp: &mut Interpreter, env: GcPtr<Env>, fields: &Vec<String>, methods: &Vec<ast::FuncDef>) -> GcPtr<TypeCtor> {
    interp.gc.alloc(TypeCtor { env, fields, methods })
  }
}

impl VTrait for TypeCtor {
  fn eval_call(&self, interp: &mut Interpreter, args: Vec<VRef>) -> VRes {
    // Make sure there is a value for each field
    if args.len() != unsafe{(*self.fields).len()} {
      return Err(VErr::WrongArgs)
    }
    // Create fields
    let mut env = Env::child(interp, self.env);
    for (id, val) in unsafe{(*self.fields).iter()}.zip(args.into_iter()) {
      env.insert(id, val)?;
    }
    for (id, params, body) in unsafe{(*self.methods).iter()} {
      let v_method = Func::new(interp, env, params, body);
      env.insert(id, v_method)?;
    }
    // Construct type object
    let obj = TypeObj::new(interp, env);
    // Add "self" binding
    env.insert("self", obj.clone())?;
    // Yield object
    Ok(obj)
  }

  fn to_str(&self, interp: &mut Interpreter) -> Result<GcPtr<VStr>, VErr> {
    Ok(VStr::new(interp, format!("ctor {:p}", self as *const _)))
  }
}

impl GcObj for TypeCtor {
  fn visit_children(&self, f: fn(GcPtr<dyn GcObj>)) {
    f(self.env)
  }
}

/// Type object

pub struct TypeObj {
  env: GcPtr<Env>
}

impl TypeObj {
  fn new(interp: &mut Interpreter, env: GcPtr<Env>) -> GcPtr<TypeObj> {
    interp.gc.alloc(TypeObj { env })
  }
}

impl VTrait for TypeObj {
  fn eval_get(&self, _: &mut Interpreter, field: &str) -> VRes {
    if let Some(val) = self.env.names.get(field) {
      Ok(val.clone())
    } else {
      Err(VErr::WrongField(str::to_owned(field)))
    }
  }

  fn eval_set(&mut self, interp: &mut Interpreter, field: &str, val: VRef) -> VRes {
    if let Some(dest) = self.env.names.get_mut(field) {
      *dest = val;
      Ok(VNil::new(interp))
    } else {
      Err(VErr::WrongField(str::to_owned(field)))
    }
  }

  fn to_str(&self, interp: &mut Interpreter) -> Result<GcPtr<VStr>, VErr> {
    Ok(VStr::new(interp, format!("obj {:p}", self as *const _)))
  }
}

impl GcObj for TypeObj {
  fn visit_children(&self, f: fn(GcPtr<dyn GcObj>)) {
    f(self.env)
  }
}

/// Builtin function

struct Builtin {
  f: fn(&mut Interpreter, Vec<VRef>) -> VRes,
  nparams: usize
}

impl Builtin {
  fn new(interp: &mut Interpreter, f: fn(&mut Interpreter, Vec<VRef>) -> VRes, nparams: usize) -> GcPtr<Builtin> {
    interp.gc.alloc(Builtin { f, nparams })
  }
}

impl VTrait for Builtin {
  fn eval_call(&self, interp: &mut Interpreter, args: Vec<VRef>) -> VRes {
    // Make sure we've received the correct number of arguments
    if args.len() != self.nparams {
      return Err(VErr::WrongArgs)
    }
    // Call native function
    (self.f)(interp, args)
  }
}

impl GcObj for Builtin {}

/// Builtin implementations

fn builtin_print(interp: &mut Interpreter, vals: Vec<VRef>) -> VRes {
  // Convert to string
  let s_val = vals[0].to_str(interp)?;
  // Print the string
  println!("{}", s_val.v());
  // Print returns nothing
  Ok(VNil::new(interp))
}

fn builtin_time(interp: &mut Interpreter, _: Vec<VRef>) -> VRes {
  Ok(VInt::new(interp, BigInt::from(SystemTime::now()
                                    .duration_since(UNIX_EPOCH)
                                    .unwrap()
                                    .as_secs())))
}

/// Name resolution environment

struct Env {
  parent: Option<GcPtr<Env>>,
  names: HashMap<String, VRef>
}

impl Env {
  fn root(interp: &mut Interpreter) -> GcPtr<Env> {
    let names = HashMap::from([
      (str::to_owned("print"), Builtin::new(interp, builtin_print, 1) as VRef),
      (str::to_owned("time"), Builtin::new(interp, builtin_time, 0) as VRef)
    ]);
    interp.gc.alloc(Self {
      parent: None,
      names: names
    })
  }

  fn child(interp: &mut Interpreter, parent: GcPtr<Env>) -> GcPtr<Env> {
    interp.gc.alloc(Self {
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
}

impl VTrait for Env {
  fn eval_get(&self, interp: &mut Interpreter, id: &str) -> VRes {
    if let Some(val) = self.names.get(id) {
      Ok(val.clone())
    } else if let Some(parent) = &self.parent {
      parent.eval_get(interp, id)
    } else {
      Err(VErr::UnknownId(str::to_owned(id)))
    }
  }

  fn eval_set(&mut self, interp: &mut Interpreter, id: &str, val: VRef) -> VRes {
    if let Some(dest) = self.names.get_mut(id) {
      *dest = val;
      Ok(VNil::new(interp))
    } else if let Some(parent) = &mut self.parent {
      parent.eval_set(interp, id, val)
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

/// Simple AST interpreter

pub struct Interpreter { pub gc: GcHeap, eval_cnt: usize }

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
      ast::Expr::Nil => Ok(VNil::new(self)),
      ast::Expr::Bool(b) => Ok(VBool::new(self, b.clone())),
      ast::Expr::Int(i) => Ok(VInt::new(self, i.clone())),
      ast::Expr::Str(s) => Ok(VStr::new(self, str::to_owned(s))),
      ast::Expr::Id(id) => env.eval_get(self, id),
      ast::Expr::Call(func, args) => {
        // Evaluate function
        let v_func = self.eval(env, func)?;

        // Evaluate arguments
        let mut v_args = Vec::new();
        for arg in args.iter() {
          v_args.push(self.eval(env, arg)?);
        }

        // Evaluate call
        v_func.eval_call(self, v_args)
      }
      ast::Expr::Dot(obj, field) => {
        let v_obj = self.eval(env, obj)?;
        v_obj.eval_get(self, field)
      }
      ast::Expr::Un(op, arg) => {
        let v_arg = self.eval(env, arg)?;
        v_arg.eval_un(self, op)
      }
      ast::Expr::Bin(op, lhs, rhs) => {
        let v_lhs = self.eval(env, lhs)?;
        let v_rhs = self.eval(env, rhs)?;
        v_lhs.eval_bin(self, op, &v_rhs)
      }
      ast::Expr::And(lhs, rhs) => {
        let v_lhs = self.eval(env, lhs)?;
        match v_lhs.downcast_bool() {
          // Need to eval RHS
          Some(b) if b.v() => {
            let v_rhs = self.eval(env, rhs)?;
            match v_rhs.downcast_bool() {
              Some(..) => Ok(v_rhs),
              None => Err(VErr::WrongType)
            }
          }
          // Short circuit false
          Some(..) => Ok(v_lhs),
          // Type error
          _ => Err(VErr::WrongType)
        }
      }
      ast::Expr::Or(lhs, rhs) => {
        let v_lhs = self.eval(env, lhs)?;
        match v_lhs.downcast_bool() {
          // Short circuit true
          Some(b) if b.v() => Ok(v_lhs),
          // Need to eval RHS
          Some(..) => {
            let v_rhs = self.eval(env, rhs)?;
            match v_rhs.downcast_bool() {
              Some(..) => Ok(v_rhs),
              None => Err(VErr::WrongType)
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
        let mut val: VRef = VNil::new(self);
        for expr in exprs.iter() {
          val = self.eval(block_env, expr)?;
        }
        Ok(val)
      }
      ast::Expr::Var(id, val) => {
        let tmp = self.eval(env, val)?;
        env.insert(id, tmp)?;
        Ok(VNil::new(self))
      }
      ast::Expr::As(dest, val) => {
        let v_val = self.eval(env, val)?;
        match &**dest {
          // Can assign to variable
          ast::Expr::Id(id) => {
            env.eval_set(self, id, v_val)
          }
          // Can assign  to field
          ast::Expr::Dot(arg, id) => {
            let mut v_arg = self.eval(env, arg)?;
            v_arg.eval_set(self, id, v_val)
          }
          // Cannot assign to anything else
          _ => {
            return Err(VErr::WrongAssign)
          }
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
        match self.eval(env, cond)?.downcast_bool() {
          Some(b) if b.v() => self.eval(env, arg1),
          Some(..)         => self.eval(env, arg2),
                         _ => Err(VErr::WrongType)
        }
      }
      ast::Expr::While(cond, body) => {
        loop {
          // Break if the condition is false
          match self.eval(env, cond)?.downcast_bool() {
            Some(b) if b.v() => (),
            Some(..) => break,
            _ => return Err(VErr::WrongType)
          };

          // Evaluate body
          match self.eval(env, body) {
            Err(VErr::WrongContinue) => (),
            Err(VErr::WrongBreak) => break,
            e @ Err(..) => return e,
            _ => (),
          }
        }
        Ok(VNil::new(self))
      }
      ast::Expr::For(id, iter, body) => {
        let v_iter = self.eval(env, iter)?;
        loop {
          // Read next element from iterator
          let val = v_iter.eval_get(self, "next")?.eval_call(self, vec![])?;
          if let Some(..) = val.downcast_nil() {
            break
          }
          // Create environment
          let mut forenv = Env::child(self, env);
          // Bind element
          forenv.insert(id, val)?;
          // Decide to continue
          match self.eval(forenv, body) {
            Err(VErr::WrongContinue) => (),
            Err(VErr::WrongBreak) => break,
            e @ Err(..) => return e,
            _ => (),
          }
        }
        Ok(VNil::new(self))
      }
      ast::Expr::Func((id, params, body)) => {
        let v_func = Func::new(self, env, params, &**body);
        env.insert(id, v_func)?;
        Ok(VNil::new(self))
      }
      ast::Expr::Type(id, fields, methods) => {
        let v_type = TypeCtor::new(self, env, fields, methods);
        env.insert(id, v_type)?;
        Ok(VNil::new(self))
      }
      ast::Expr::Lambda(params, body) => {
        Ok(Func::new(self, env, params, &**body))
      }
    }
  }
}
