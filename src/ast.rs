use num_bigint::BigInt;

#[derive(Clone,Debug)]
pub enum UnOp { Neg, Not }

#[derive(Clone,Debug)]
pub enum BinOp { Add, Sub, Mul, Div, Mod, Eq, Ne, Lt, Gt, Le, Ge }

#[derive(Clone,Debug)]
pub enum Expr {
  Nil,
  Bool(bool),
  Int(BigInt),
  Str(String),
  Id(String),
  Call(Box<Expr>, Vec<Expr>),
  Dot(Box<Expr>, String),
  Un(UnOp, Box<Expr>),
  Bin(BinOp, Box<Expr>, Box<Expr>),
  And(Box<Expr>, Box<Expr>),
  Or(Box<Expr>, Box<Expr>),
  Block(Vec<Expr>),
  Var(String, Box<Expr>),
  As(String, Box<Expr>),
  Continue,
  Break,
  Return(Box<Expr>),
  If(Box<Expr>, Box<Expr>, Box<Expr>),
  While(Box<Expr>, Box<Expr>),
  Func(FuncDef),
  Type(String, Vec<String>, Vec<FuncDef>)
}

pub type FuncDef = (String, Vec<String>, Box<Expr>);

#[derive(Clone,Debug)]
pub struct Program(pub Vec<Expr>);
