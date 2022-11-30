use super::*;

/// Booolean value

pub struct VBool(bool);

impl VBool {
  pub fn new(b: bool) -> Rc<VBool> {
    Rc::new(VBool(b))
  }

  pub fn v(&self) -> bool { self.0 }
}

impl VTrait for VBool {
  fn eval_un(&self, op: &ast::UnOp) -> VRes {
    use ast::UnOp::*;
    match op {
      Not => Ok(VBool::new(!self.0)),
      _ => Err(VErr::WrongType)
    }
  }

  fn eval_bin(&self, op: &ast::BinOp, rhs: &VRef) -> VRes {
    use ast::BinOp::*;
    match (op, rhs.downcast_bool()) {
      (Eq, Some(b)) => Ok(VBool::new(self.0 == b.0)),
      (Ne, Some(b)) => Ok(VBool::new(self.0 != b.0)),
      (Eq, None)    => Ok(VBool::new(false)),
      (Ne, None)    => Ok(VBool::new(true)),
      _ => Err(VErr::WrongType)
    }
  }

  fn to_str(&self) -> Result<Rc<VStr>, VErr> {
    Ok(VStr::new(format!("{}", self.0)))
  }

  fn downcast_bool(&self) -> Option<&VBool> { Some(self) }
}
