use super::*;

/// Integer value

pub struct VInt(BigInt);

impl VInt {
  pub fn new(i: BigInt) -> Rc<VInt> {
    Rc::new(VInt(i))
  }

  #[allow(dead_code)]
  pub fn v(&self) -> &BigInt { &self.0 }
}

impl VTrait for VInt {
  fn eval_un(&self, op: &ast::UnOp) -> VRes {
    use ast::UnOp::*;
    match op {
      Neg => Ok(VInt::new(-&self.0)),
      _ => Err(VErr::WrongType)
    }
  }

  fn eval_bin(&self, op: &ast::BinOp, rhs: &VRef) -> VRes {
    use ast::BinOp::*;
    match (op, rhs.downcast_int()) {
      (Add, Some(r)) => Ok(VInt::new(&self.0 + &r.0)),
      (Sub, Some(r)) => Ok(VInt::new(&self.0 - &r.0)),
      (Mul, Some(r)) => Ok(VInt::new(&self.0 * &r.0)),
      (Div|Mod, Some(r)) if r.0 == BigInt::from(0i32) => { Err(VErr::DivideByZero) }
      (Div, Some(r)) => Ok(VInt::new(&self.0 / &r.0)),
      (Mod, Some(r)) => Ok(VInt::new(&self.0 % &r.0)),
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

  fn downcast_int(&self) -> Option<&VInt> { Some(self) }
}
