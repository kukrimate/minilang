use crate::ast;
use crate::val::*;
use num_bigint::BigInt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

/// Function value

struct Func {
  env: Env,
  params: *const Vec<String>,
  body: *const ast::Expr
}

impl Func {
  fn new(env: Env, params: &Vec<String>, body: &ast::Expr) -> Rc<Func> {
    Rc::new(Func { env, params, body })
  }
}

impl VTrait for Func {
  fn eval_call(&self, args: Vec<VRef>) -> VRes {
    // Make sure we've received the correct number of arguments
    if args.len() != unsafe{(*self.params).len()} {
      return Err(VErr::WrongArgs)
    }
    // Bind arguments to parameters
    let env = EnvS::child(self.env.clone());
    for (param, arg) in unsafe{(*self.params).iter()}.zip(args.into_iter()) {
      env.borrow_mut().insert(param, arg)?;
    }
    // Evaluate body with arguments
    eval(env.clone(), unsafe{&*self.body})
  }
}

/// Builtin function

struct Builtin {
  f: fn(Vec<VRef>) -> VRes,
  nparams: usize
}

impl Builtin {
  fn new(f: fn(Vec<VRef>) -> VRes, nparams: usize) -> Rc<Builtin> {
    Rc::new(Builtin { f, nparams })
  }
}

impl VTrait for Builtin {
  fn eval_call(&self, args: Vec<VRef>) -> VRes {
    // Make sure we've received the correct number of arguments
    if args.len() != self.nparams {
      return Err(VErr::WrongArgs)
    }
    // Call native function
    (self.f)(args)
  }
}

/// Builtin implementations

fn builtin_print<'a>(vals: Vec<VRef>) -> VRes {
  // Convert to string
  let s_val = vals[0].to_str()?;
  // Print the string
  println!("{}", s_val.v());
  // Print returns nothing
  Ok(VNil::new())
}

fn builtin_time<'a>(_: Vec<VRef>) -> VRes {
  Ok(VInt::new(BigInt::from(SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_secs())))
}

/// Name resolution environment

type Env = Rc<RefCell<EnvS>>;

struct EnvS {
  parent: Option<Env>,
  names: HashMap<String, VRef>
}

impl EnvS {
  fn root() -> Env {
    Rc::new(RefCell::new(Self {
      parent: None,
      names: HashMap::from([
        (str::to_owned("print"), Builtin::new(builtin_print, 1) as VRef),
        (str::to_owned("time"), Builtin::new(builtin_time, 0) as VRef)
      ])
    }))
  }

  fn child(parent: Env) -> Env {
    Rc::new(RefCell::new(Self {
      parent: Some(parent),
      names: HashMap::new()
    }))
  }

  fn get(&self, id: &str) -> VRes {
    if let Some(val) = self.names.get(id) {
      Ok(val.clone())
    } else if let Some(parent) = &self.parent {
      parent.borrow_mut().get(id)
    } else {
      Err(VErr::UnknownId(str::to_owned(id)))
    }
  }

  fn replace(&mut self, id: &str, val: VRef) -> Result<(), VErr> {
    if let Some(dest) = self.names.get_mut(id) {
      *dest = val;
      Ok(())
    } else if let Some(parent) = &mut self.parent {
      parent.borrow_mut().replace(id, val)
    } else {
      Err(VErr::UnknownId(str::to_owned(id)))
    }
  }

  fn insert(&mut self, id: &str, val: VRef) -> Result<(), VErr> {
    match self.names.insert(str::to_owned(id), val) {
      Some(..) => Err(VErr::RedefinedId(str::to_owned(id))),
      None => Ok(())
    }
  }
}

/// Simple AST interpreter

pub fn execute(ast::Program(exprs): &ast::Program) -> Result<(), VErr> {
  let env = EnvS::root();
  for expr in exprs.iter() {
    eval(env.clone(), expr)?;
  }
  Ok(())
}

fn eval<'a>(env: Env, expr: &'a ast::Expr) -> VRes {
  match expr {
    ast::Expr::Nil => Ok(VNil::new()),
    ast::Expr::Bool(b) => Ok(VBool::new(b.clone())),
    ast::Expr::Int(i) => Ok(VInt::new(i.clone())),
    ast::Expr::Str(s) => Ok(VStr::new(str::to_owned(s))),
    ast::Expr::Id(id) => env.borrow().get(id),
    ast::Expr::Call(func, args) => {
      // Evaluate function
      let func = eval(env.clone(), func)?;

      // Evaluate arguments
      let mut v_args = Vec::new();
      for arg in args.iter() {
        v_args.push(eval(env.clone(), arg)?);
      }

      // Evaluate call
      func.eval_call(v_args)
    }
    ast::Expr::Un(op, arg) => {
      let v_arg = eval(env.clone(), arg)?;
      v_arg.eval_un(op)
    }
    ast::Expr::Bin(op, lhs, rhs) => {
      let v_lhs = eval(env.clone(), lhs)?;
      let v_rhs = eval(env.clone(), rhs)?;
      v_lhs.eval_bin(op, &v_rhs)
    }
    ast::Expr::And(lhs, rhs) => {
      let v_lhs = eval(env.clone(), lhs)?;
      match v_lhs.downcast_bool() {
        // Need to eval RHS
        Some(b) if b.v() => {
          let v_rhs = eval(env.clone(), rhs)?;
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
      let v_lhs = eval(env.clone(), lhs)?;
      match v_lhs.downcast_bool() {
        // Short circuit true
        Some(b) if b.v() => Ok(v_lhs),
        // Need to eval RHS
        Some(..) => {
          let v_rhs = eval(env.clone(), rhs)?;
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
      let env = EnvS::child(env);
      // Evaluate block body
      let mut val: VRef = VNil::new();
      for expr in exprs.iter() {
        val = eval(env.clone(), expr)?;
      }
      Ok(val)
    }
    ast::Expr::Var(id, val) => {
      let tmp = eval(env.clone(), val)?;
      env.borrow_mut().insert(id, tmp)?;
      Ok(VNil::new())
    }
    ast::Expr::As(id, val) => {
      let v_val = eval(env.clone(), val)?;
      env.borrow_mut().replace(id, v_val)?;
      Ok(VNil::new())
    }
    ast::Expr::Continue => {
      todo!()
    }
    ast::Expr::Break => {
      todo!()
    }
    ast::Expr::Return(..) => {
      todo!()
    }
    ast::Expr::If(cond, arg1, arg2) => {
      match eval(env.clone(), cond)?.downcast_bool() {
        Some(b) if b.v() => eval(env.clone(), arg1),
        Some(..) => eval(env.clone(), arg2),
        _ => Err(VErr::WrongType)
      }
    }
    ast::Expr::While(cond, body) => {
      loop {
        match eval(env.clone(), cond)?.downcast_bool() {
          Some(b) if b.v() => eval(env.clone(), body)?,
          Some(..) => break,
          _ => return Err(VErr::WrongType)
        };
      }
      Ok(VNil::new())
    }
    ast::Expr::Func(id, params, body) => {
      let v_func = Func::new(env.clone(), params, &**body);
      env.borrow_mut().insert(id, v_func)?;
      Ok(VNil::new())
    }
  }
}
