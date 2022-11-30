use super::*;

/// String value

pub struct VStr(String);

impl VStr {
  pub fn new(s: String) -> Rc<VStr> {
    Rc::new(VStr(s))
  }

  pub fn v(&self) -> &str { &self.0 }
}

impl VTrait for VStr {
  fn eval_bin(&self, op: &ast::BinOp, rhs: &VRef) -> VRes {
    use ast::BinOp::*;
    match (op, rhs.downcast_str()) {
      (Add, Some(r)) => Ok(VStr::new(format!("{}{}", self.0, r.0))),
      (Eq, Some(r))  => Ok(VBool::new(&self.0 == &r.0)),
      (Ne, Some(r))  => Ok(VBool::new(&self.0 != &r.0)),
      (Gt, Some(r))  => Ok(VBool::new(&self.0 > &r.0)),
      (Lt, Some(r))  => Ok(VBool::new(&self.0 < &r.0)),
      (Ge, Some(r))  => Ok(VBool::new(&self.0 >= &r.0)),
      (Le, Some(r))  => Ok(VBool::new(&self.0 <= &r.0)),
      (Eq, None)     => Ok(VBool::new(false)),
      (Ne, None)     => Ok(VBool::new(true)),
      _ => Err(VErr::WrongType)
    }
  }

  fn to_str(&self) -> Result<Rc<VStr>, VErr> {
    Ok(VStr::new(format!("{}", self.0)))
  }

  fn downcast_str(&self) -> Option<&VStr> { Some(self) }
}
