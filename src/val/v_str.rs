use super::*;

/// String value

pub struct VStr(String);

impl VStr {
  pub fn new(interp: &mut Interpreter, s: String) -> GcPtr<VStr> {
    interp.gc.alloc(VStr(s))
  }

  pub fn v(&self) -> &str { &self.0 }
}

impl VTrait for VStr {
  fn eval_bin(&self, interp: &mut Interpreter, op: &ast::BinOp, rhs: &VRef) -> VRes {
    use ast::BinOp::*;
    match (op, rhs.downcast_str()) {
      (Add, Some(r)) => Ok(VStr::new(interp, format!("{}{}", self.0, r.0))),
      (Eq, Some(r))  => Ok(VBool::new(interp, &self.0 == &r.0)),
      (Ne, Some(r))  => Ok(VBool::new(interp, &self.0 != &r.0)),
      (Gt, Some(r))  => Ok(VBool::new(interp, &self.0 > &r.0)),
      (Lt, Some(r))  => Ok(VBool::new(interp, &self.0 < &r.0)),
      (Ge, Some(r))  => Ok(VBool::new(interp, &self.0 >= &r.0)),
      (Le, Some(r))  => Ok(VBool::new(interp, &self.0 <= &r.0)),
      (Eq, None)     => Ok(VBool::new(interp, false)),
      (Ne, None)     => Ok(VBool::new(interp, true)),
      _ => Err(VErr::WrongType)
    }
  }

  fn to_str(&self, interp: &mut Interpreter) -> Result<GcPtr<VStr>, VErr> {
    Ok(VStr::new(interp, format!("{}", self.0)))
  }

  fn downcast_str(&self) -> Option<&VStr> { Some(self) }
}

impl GcObj for VStr {}
