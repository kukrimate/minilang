use super::*;

/// Nonexistent value

pub struct VNil;

impl VNil {
  pub fn new(interp: &mut Interpreter) -> GcPtr<VNil> {
    interp.gc.alloc(VNil)
  }
}

impl VTrait for VNil {
  fn eval_bin(&self, interp: &mut Interpreter, op: &ast::BinOp, rhs: &VRef) -> VRes {
    use ast::BinOp::*;
    match (op, rhs.downcast_nil()) {
      (Eq, r) => Ok(VBool::new(interp, r.is_some())),
      (Ne, r) => Ok(VBool::new(interp, r.is_none())),
      _ => Err(VErr::WrongType)
    }
  }

  fn to_str(&self, interp: &mut Interpreter) -> Result<GcPtr<VStr>, VErr> {
    Ok(VStr::new(interp, format!("nil")))
  }

  fn downcast_nil(&self) -> Option<&VNil> { Some(self) }
}

impl GcObj for VNil {}
