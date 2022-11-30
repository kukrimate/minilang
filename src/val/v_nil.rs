use super::*;

/// Nonexistent value

pub struct VNil;

impl VNil {
  pub fn new() -> Rc<VNil> {
    Rc::new(VNil)
  }
}

impl VTrait for VNil {
  fn eval_bin(&self, op: &ast::BinOp, rhs: &VRef) -> VRes {
    use ast::BinOp::*;
    match (op, rhs.downcast_nil()) {
      (Eq, r) => Ok(VBool::new(r.is_some())),
      (Ne, r) => Ok(VBool::new(r.is_none())),
      _ => Err(VErr::WrongType)
    }
  }

  fn to_str(&self) -> Result<Rc<VStr>, VErr> {
    Ok(VStr::new(format!("nil")))
  }

  fn downcast_nil(&self) -> Option<&VNil> { Some(self) }
}
