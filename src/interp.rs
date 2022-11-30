use crate::ast;
use crate::val::*;
use std::collections::HashMap;
use std::rc::Rc;

/// Function value

pub struct Func {
  params: *const Vec<String>,
  body: *const ast::Expr
}

impl Func {
  pub fn new(params: &Vec<String>, body: &ast::Expr) -> Rc<Func> {
    Rc::new(Func { params, body })
  }
}

impl VTrait for Func {
  fn eval_call(&self, args: Vec<VRef>) -> VRes {
    // Make sure we've received the correct number of arguments
    if args.len() != unsafe{(*self.params).len()} {
      return Err(VErr::WrongArgs)
    }
    // Bind arguments to parameters
    let mut env = Env::root();
    for (param, arg) in unsafe{(*self.params).iter()}.zip(args.into_iter()) {
      env.insert(param, arg);
    }
    // Evaluate body with arguments
    eval(&mut env, unsafe{&*self.body})
  }
}

/// Builtin function

struct Builtin {
  f: fn(Vec<VRef>) -> VRes,
  nparams: usize
}

impl Builtin {
  pub fn new(f: fn(Vec<VRef>) -> VRes, nparams: usize) -> Rc<Builtin> {
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


/// Name resolution environment

struct Env<'a> {
  parent: Option<*mut Env<'a>>,
  names: HashMap<&'a str, VRef>
}

impl<'a> Env<'a> {
  fn root() -> Env<'a> {
    Self {
      parent: None,
      names: HashMap::from([
        ("print", Builtin::new(builtin_print, 1) as VRef)
      ])
    }
  }

  fn child(parent: *mut Self) -> Env<'a> {
    Self {
      parent: Some(parent),
      names: HashMap::new()
    }
  }

  fn get(&self, id: &str) -> Option<&VRef> {
    if let Some(val) = self.names.get(id) {
      Some(val)
    } else if let Some(parent) = &self.parent {
      unsafe {(**parent).get(id)}
    } else {
      None
    }
  }

  fn get_mut(&mut self, id: &str) -> Option<&mut VRef> {
    if let Some(val) = self.names.get_mut(id) {
      Some(val)
    } else if let Some(parent) = &mut self.parent {
      unsafe {(**parent).get_mut(id)}
    } else {
      None
    }
  }

  fn insert(&mut self, id: &'a str, val: VRef) {
    self.names.insert(id, val);
  }
}

/// Simple AST interpreter

pub fn execute<'a>(ast::Program(exprs): &'a ast::Program) -> Result<(), VErr> {
  let mut env = Env::root();
  for expr in exprs.iter() {
    eval(&mut env, expr)?;
  }
  Ok(())
}

fn eval<'a>(env: &mut Env<'a>, expr: &'a ast::Expr) -> VRes {
  match expr {
    ast::Expr::Nil => Ok(VNil::new()),
    ast::Expr::Bool(b) => Ok(VBool::new(b.clone())),
    ast::Expr::Int(i) => Ok(VInt::new(i.clone())),
    ast::Expr::Str(s) => Ok(VStr::new(str::to_owned(s))),
    ast::Expr::Id(id) => {
      match env.get(id) {
        Some(val) => Ok(val.clone()),
        None => Err(VErr::UnknownId(id.clone()))
      }
    }
    ast::Expr::Call(func, args) => {
      // Evaluate function
      let func = eval(env, func)?;

      // Evaluate arguments
      let mut v_args = Vec::new();
      for arg in args.iter() {
        v_args.push(eval(env, arg)?);
      }

      // Evaluate call
      func.eval_call(v_args)
    }
    ast::Expr::Un(op, arg) => {
      let v_arg = eval(env, arg)?;
      v_arg.eval_un(op)
    }
    ast::Expr::Bin(op, lhs, rhs) => {
      let v_lhs = eval(env, lhs)?;
      let v_rhs = eval(env, rhs)?;
      v_lhs.eval_bin(op, &v_rhs)
    }
    ast::Expr::And(lhs, rhs) => {
      let v_lhs = eval(env, lhs)?;
      match v_lhs.downcast_bool() {
        // Need to eval RHS
        Some(b) if b.v() => {
          let v_rhs = eval(env, rhs)?;
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
      let v_lhs = eval(env, lhs)?;
      match v_lhs.downcast_bool() {
        // Short circuit true
        Some(b) if b.v() => Ok(v_lhs),
        // Need to eval RHS
        Some(..) => {
          let v_rhs = eval(env, rhs)?;
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
      let mut benv = Env::child(env);
      // Evaluate block body
      let mut val: VRef = VNil::new();
      for expr in exprs.iter() {
        val = eval(&mut benv, expr)?;
      }
      Ok(val)
    }
    ast::Expr::Var(id, val) => {
      let tmp = eval(env, val)?;
      env.insert(id, tmp);
      Ok(VNil::new())
    }
    ast::Expr::As(id, val) => {
      // Evaluate value
      let v_val = eval(env, val)?;
      // Write to destination
      match env.get_mut(&**id) {
        Some(dest) => {
          *dest = v_val;
          Ok(VNil::new())
        },
        None => Err(VErr::UnknownId(id.clone()))
      }
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
      match eval(env, cond)?.downcast_bool() {
        Some(b) if b.v() => eval(env, arg1),
        Some(..) => eval(env, arg2),
        _ => Err(VErr::WrongType)
      }
    }
    ast::Expr::While(cond, body) => {
      loop {
        match eval(env, cond)?.downcast_bool() {
          Some(b) if b.v() => eval(env, body)?,
          Some(..) => break,
          _ => return Err(VErr::WrongType)
        };
      }
      Ok(VNil::new())
    }
    ast::Expr::Func(id, params, body) => {
      env.insert(id, Func::new(params, &**body));
      Ok(VNil::new())
    }
  }
}
