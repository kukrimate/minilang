use crate::ast;
use crate::interp::*;
use crate::gc::*;
use num_bigint::BigInt;
use std::fmt;

/// Runtime result

pub type VRes = Result<VRef, VErr>;

/// Runtime values

pub type VRef = GcPtr<dyn VTrait>;

pub trait VTrait: GcObj {
  // Call as a function
  fn eval_call(&self, _: &mut Interpreter, _: Vec<VRef>) -> VRes { Err(VErr::WrongType) }

  // Access field
  fn eval_dot(&self, _: &mut Interpreter, _: &str) -> VRes { Err(VErr::WrongType) }

  // Apply a unary operator to it
  fn eval_un(&self, interp: &mut Interpreter, op: &ast::UnOp) -> VRes {
    let id = match op {
      ast::UnOp::Neg => "neg",
      ast::UnOp::Not => "not",
    };
    let v_func = self.eval_dot(interp, id)?;
    v_func.eval_call(interp, Vec::new())
  }

  // Apply a binary operator to it
  fn eval_bin(&self, interp: &mut Interpreter, op: &ast::BinOp, rhs: &VRef) -> VRes {
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
    let v_func = self.eval_dot(interp, id)?;
    v_func.eval_call(interp, Vec::from([rhs.clone()]))
  }

  // Conversion to string
  fn to_str(&self, _: &mut Interpreter) -> Result<GcPtr<VStr>, VErr> { Err(VErr::WrongType) }

  // Type assertions
  fn downcast_nil(&self) -> Option<&VNil> { None }
  fn downcast_bool(&self) -> Option<&VBool> { None }
  fn downcast_int(&self) -> Option<&VInt> { None }
  fn downcast_str(&self) -> Option<&VStr> { None }
}

/// Runtime error

pub enum VErr {
  UnknownId(String),
  RedefinedId(String),
  WrongType,
  DivideByZero,
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
      VErr::WrongArgs => write!(f, "Division by zero"),
      VErr::WrongField(id) => write!(f, "Unknown field {}", id),
      VErr::WrongContinue => write!(f, "Continue outside loop"),
      VErr::WrongBreak => write!(f, "Break outside loop"),
      VErr::WrongReturn(..) => write!(f, "Return outside function")
    }
  }
}

mod v_bool;
mod v_int;
mod v_nil;
mod v_str;

pub use v_bool::*;
pub use v_int::*;
pub use v_nil::*;
pub use v_str::*;
