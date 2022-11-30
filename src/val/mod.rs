use crate::ast;
use num_bigint::BigInt;
use std::rc::Rc;

/// Runtime result

pub type VRes = Result<VRef, VErr>;

/// Runtime values

pub type VRef = Rc<dyn VTrait>;

pub trait VTrait {
  // Call as a function
  fn eval_call(&self, _: Vec<VRef>) -> VRes { Err(VErr::WrongType) }

  // Access field
  fn eval_dot(&self, _: &str) -> VRes { Err(VErr::WrongType) }

  // Apply a unary operator to it
  fn eval_un(&self, op: &ast::UnOp) -> VRes {
    let id = match op {
      ast::UnOp::Neg => "neg",
      ast::UnOp::Not => "not",
    };
    let func = self.eval_dot(id)?;
    func.eval_call(Vec::new())
  }

  // Apply a binary operator to it
  fn eval_bin(&self, op: &ast::BinOp, rhs: &VRef) -> VRes {
    let id = match op {
      ast::BinOp::Add => "add",
      ast::BinOp::Sub => "sub",
      ast::BinOp::Mul => "mul",
      ast::BinOp::Div => "div",
      ast::BinOp::Mod => "mod",
      ast::BinOp::Eq  => "eq",
      ast::BinOp::Ne  => "ne",
      ast::BinOp::Lt  => "lt",
      ast::BinOp::Gt  => "gt",
      ast::BinOp::Le  => "le",
      ast::BinOp::Ge  => "ge"
    };
    let func = self.eval_dot(id)?;
    func.eval_call(Vec::from([rhs.clone()]))
  }

  // Conversion to string
  fn to_str(&self) -> Result<Rc<VStr>, VErr> { Err(VErr::WrongType) }

  // Type assertions
  fn downcast_nil(&self) -> Option<&VNil> { None }
  fn downcast_bool(&self) -> Option<&VBool> { None }
  fn downcast_int(&self) -> Option<&VInt> { None }
  fn downcast_str(&self) -> Option<&VStr> { None }
}

/// Runtime error

#[derive(Debug)]
pub enum VErr {
  UnknownId(String),
  RedefinedId(String),
  WrongType,
  DivideByZero,
  WrongArgs,
  WrongField
}

mod v_bool;
mod v_int;
mod v_nil;
mod v_str;

pub use v_bool::*;
pub use v_int::*;
pub use v_nil::*;
pub use v_str::*;
