use crate::ast;
use num_bigint::BigInt;
use std::collections::HashMap;

/// Runtime value

#[derive(Clone)]
pub enum IVal<'a> {
  // Nothing
  Nil,
  // Boolean
  Bool(bool),
  // Integer
  Int(BigInt),
  // String
  Str(&'a str),
  // Function definition
  Func {
    params: &'a Vec<String>,
    body: &'a ast::Expr
  },
  Builtin(fn(Vec<IVal<'a>>) -> IVal<'a>, usize)
}

/// Runtime error

#[derive(Clone,Debug)]
pub enum IErr<'a> {
  UnknownId(&'a str),
  WrongType,
  DivideByZero,
  WrongArgs
}

/// Name resolution environment

struct Env<'a> {
  parent: Option<*mut Env<'a>>,
  names: HashMap<&'a str, IVal<'a>>
}

impl<'a> Env<'a> {
  fn root() -> Env<'a> {
    Self {
      parent: None,
      names: HashMap::from([
        ("print", IVal::Builtin(builtin_print, 1))
      ])
    }
  }

  fn child(parent: *mut Self) -> Env<'a> {
    Self {
      parent: Some(parent),
      names: HashMap::new()
    }
  }

  fn get(&self, id: &str) -> Option<&IVal<'a>> {
    if let Some(val) = self.names.get(id) {
      Some(val)
    } else if let Some(parent) = &self.parent {
      unsafe {(**parent).get(id)}
    } else {
      None
    }
  }

  fn get_mut(&mut self, id: &str) -> Option<&mut IVal<'a>> {
    if let Some(val) = self.names.get_mut(id) {
      Some(val)
    } else if let Some(parent) = &mut self.parent {
      unsafe {(**parent).get_mut(id)}
    } else {
      None
    }
  }

  fn insert(&mut self, id: &'a str, val: IVal<'a>) {
    self.names.insert(id, val);
  }
}

/// Simple AST interpreter

pub fn execute<'a>(ast::Program(exprs): &'a ast::Program) -> Result<(), IErr<'a>> {
  let mut env = Env::root();
  for expr in exprs.iter() {
    eval(&mut env, expr)?;
  }
  Ok(())
}

fn eval<'a>(env: &mut Env<'a>, expr: &'a ast::Expr) -> Result<IVal<'a>, IErr<'a>> {
  match expr {
    ast::Expr::Nil => Ok(IVal::Nil),
    ast::Expr::Bool(b) => Ok(IVal::Bool(*b)),
    ast::Expr::Int(i) => Ok(IVal::Int(i.clone())),
    ast::Expr::Str(s) => Ok(IVal::Str(s)),
    ast::Expr::Id(id) => {
      match env.get(id) {
        Some(val) => Ok(val.clone()),
        None => Err(IErr::UnknownId(id))
      }
    }
    ast::Expr::Call(func, args) => {
      // Find function
      let (params, body) = match eval(env, func)? {
        IVal::Func { params, body } => {
          (params, body)
        },
        IVal::Builtin(f, nparams) => {
          let mut vals = Vec::new();
          if args.len() != nparams {
            return Err(IErr::WrongArgs)
          }
          for arg in args.iter() {
            vals.push(eval(env, arg)?);
          }
          return Ok(f(vals))
        }
        _ => return Err(IErr::WrongType)
      };

      // Create empty environmnet for function
      let mut fenv = Env::root();
      // Bind argument values to parameters
      if args.len() != params.len() {
        return Err(IErr::WrongArgs)
      }
      for (param, arg) in params.into_iter().zip(args.iter()) {
        let tmp = eval(env, arg)?;
        fenv.insert(param, tmp);
      }
      // Evaluate the function body
      eval(&mut fenv, body)
    }
    ast::Expr::Un(op, arg) => {
      use ast::UnOp::*;
      match (op, eval(env, arg)?) {
        (Neg, IVal::Int(i))  => Ok(IVal::Int(-i)),
        (Not, IVal::Bool(b)) => Ok(IVal::Bool(!b)),
                           _ => Err(IErr::WrongType)
      }
    }
    ast::Expr::Bin(op, lhs, rhs) => {
      use ast::BinOp::*;
      match (op, eval(env, lhs)?, eval(env, rhs)?) {
        // Arithmetic
        (Add, IVal::Int(i), IVal::Int(j)) => Ok(IVal::Int(i + j)),
        (Sub, IVal::Int(i), IVal::Int(j)) => Ok(IVal::Int(i - j)),
        (Mul, IVal::Int(i), IVal::Int(j)) => Ok(IVal::Int(i * j)),
        // Catch division by zero
        (Div | Mod, IVal::Int(_), IVal::Int(j))
          if j == BigInt::from(0i32) => {
            Err(IErr::DivideByZero)
          }
        (Div, IVal::Int(i), IVal::Int(j)) => Ok(IVal::Int(i / j)),
        (Mod, IVal::Int(i), IVal::Int(j)) => Ok(IVal::Int(i % j)),
        // Comparisons
        (Eq, IVal::Int(i), IVal::Int(j))  => Ok(IVal::Bool(i == j)),
        (Ne, IVal::Int(i), IVal::Int(j))  => Ok(IVal::Bool(i != j)),
        (Lt, IVal::Int(i), IVal::Int(j))  => Ok(IVal::Bool(i < j)),
        (Gt, IVal::Int(i), IVal::Int(j))  => Ok(IVal::Bool(i > j)),
        (Le, IVal::Int(i), IVal::Int(j))  => Ok(IVal::Bool(i <= j)),
        (Ge, IVal::Int(i), IVal::Int(j))  => Ok(IVal::Bool(i >= j)),
                                        _ => Err(IErr::WrongType)
      }
    }
    ast::Expr::And(lhs, rhs) => {
      match eval(env, lhs)? {
        // Need to eval RHS
        IVal::Bool(true) => {
          match eval(env, rhs)? {
            IVal::Bool(b) => Ok(IVal::Bool(b)),
            _ => Err(IErr::WrongType)
          }
        }
        // Short circuit false
        IVal::Bool(false) => {
          Ok(IVal::Bool(false))
        }
        _ => Err(IErr::WrongType)
      }
    }
    ast::Expr::Or(lhs, rhs) => {
      match eval(env, lhs)? {
        // Short circuit true
        IVal::Bool(true) => {
          Ok(IVal::Bool(true))
        }
        // Need to eval RHS
        IVal::Bool(false) => {
          match eval(env, rhs)? {
            IVal::Bool(b) => Ok(IVal::Bool(b)),
            _ => Err(IErr::WrongType)
          }
        }
        _ => Err(IErr::WrongType)
      }
    }
    ast::Expr::Block(exprs) => {
      // Create block environment
      let mut benv = Env::child(env);
      // Evaluate block body
      let mut val = IVal::Nil;
      for expr in exprs.iter() {
        val = eval(&mut benv, expr)?;
      }
      Ok(val)
    }
    ast::Expr::Var(id, val) => {
      let tmp = eval(env, val)?;
      env.insert(id, tmp);
      Ok(IVal::Nil)
    }
    ast::Expr::As(id, val) => {
      let tmp = eval(env, val)?;
      match env.get_mut(&**id) {
        Some(dest) => {
          *dest = tmp;
          Ok(IVal::Nil)
        },
        None => Err(IErr::UnknownId(id))
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
      match eval(env, cond)? {
        IVal::Bool(true)  => eval(env, arg1),
        IVal::Bool(false) => eval(env, arg2),
                        _ => Err(IErr::WrongType)
      }
    }
    ast::Expr::While(cond, body) => {
      loop {
        match eval(env, cond)? {
          IVal::Bool(true)  => eval(env, body)?,
          IVal::Bool(false) => break Ok(IVal::Nil),
                          _ => break Err(IErr::WrongType)
        };
      }
    }
    ast::Expr::Func(id, params, body) => {
      env.insert(id, IVal::Func {
        params: params,
        body: &**body
      });
      Ok(IVal::Nil)
    }
  }
}

/// Builtin implementations

fn builtin_print<'a>(vals: Vec<IVal<'a>>) -> IVal<'a> {
  match &vals[0] {
    IVal::Nil => println!("nil"),
    IVal::Bool(b) => println!("{}", b),
    IVal::Int(i) => println!("{}", i),
    IVal::Str(s) => println!("{}", s),
    IVal::Func {..} => println!("func"),
    IVal::Builtin(..) => println!("builtin")
  }
  IVal::Nil
}
