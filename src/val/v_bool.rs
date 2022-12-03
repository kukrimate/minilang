use super::*;

/// Booolean value

pub struct VBool(bool);

impl VBool {
  pub fn new(interp: &mut Interpreter, b: bool) -> GcPtr<VBool> {
    interp.gc.alloc(VBool(b))
  }

  pub fn v(&self) -> bool { self.0 }
}

impl VTrait for VBool {
  fn eval_un(&self, interp: &mut Interpreter, op: &ast::UnOp) -> VRes {
    use ast::UnOp::*;
    match op {
      Not => Ok(VBool::new(interp,  !self.0)),
      _ => Err(VErr::WrongType)
    }
  }

  fn eval_bin(&self, interp: &mut Interpreter, op: &ast::BinOp, rhs: &VRef) -> VRes {
    use ast::BinOp::*;
    match (op, rhs.downcast_bool()) {
      (Eq, Some(b)) => Ok(VBool::new(interp, self.0 == b.0)),
      (Ne, Some(b)) => Ok(VBool::new(interp, self.0 != b.0)),
      (Eq, None)    => Ok(VBool::new(interp, false)),
      (Ne, None)    => Ok(VBool::new(interp, true)),
      _ => Err(VErr::WrongType)
    }
  }

  fn to_str(&self, interp: &mut Interpreter) -> Result<GcPtr<VStr>, VErr> {
    Ok(VStr::new(interp, format!("{}", self.0)))
  }

  fn downcast_bool(&self) -> Option<&VBool> { Some(self) }
}

impl GcObj for VBool {}
