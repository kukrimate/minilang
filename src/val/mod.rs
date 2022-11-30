use crate::ast;
use num_bigint::BigInt;
use std::rc::Rc;

/// Runtime result

pub type VRes = Result<VRef, VErr>;

/// Runtime values

pub type VRef = Rc<dyn VTrait>;

pub trait VTrait {
  // Call it as a function
  fn eval_call(&self, _: Vec<VRef>) -> VRes { Err(VErr::WrongType) }

  // Apply a unary operator to it
  fn eval_un(&self, _: &ast::UnOp) -> VRes { Err(VErr::WrongType) }

  // Apply a binary operator to it
  fn eval_bin(&self, _: &ast::BinOp, _: &VRef) -> VRes { Err(VErr::WrongType) }

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
  WrongType,
  DivideByZero,
  WrongArgs
}

mod v_bool;
mod v_int;
mod v_nil;
mod v_str;

pub use v_bool::*;
pub use v_int::*;
pub use v_nil::*;
pub use v_str::*;
