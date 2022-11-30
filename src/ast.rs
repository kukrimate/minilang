use num_bigint::BigInt;

#[derive(Debug)]
pub enum UnOp { Neg }

#[derive(Debug)]
pub enum BinOp { Add, Sub, Mul, Div, Mod, Eq, Ne, Lt, Gt, Le, Ge, And, Or }

#[derive(Debug)]
pub enum Expr {
  Nil,
  Bool(bool),
  Int(BigInt),
  Str(String),
  Id(String),
  Call(Box<Expr>, Vec<Expr>),
  Un(UnOp, Box<Expr>),
  Bin(BinOp, Box<Expr>, Box<Expr>),
  Block(Vec<Expr>),
  Var(String, Box<Expr>),
  As(String, Box<Expr>),
  Continue,
  Break,
  Return(Box<Expr>),
  If(Box<Expr>, Box<Expr>, Box<Expr>),
  While(Box<Expr>, Box<Expr>),
  Func(String, Vec<String>, Box<Expr>)
}

#[derive(Debug)]
pub struct Program(pub Vec<Expr>);
