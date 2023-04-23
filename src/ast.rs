use num_bigint::BigInt;

#[derive(Clone,Debug)]
pub struct Span(pub usize, pub usize);

#[derive(Clone,Debug)]
pub enum UnOp { Neg, Not }

#[derive(Clone,Debug)]
pub enum BinOp { Add, Sub, Mul, Div, Mod, Eq, Ne, Lt, Gt, Le, Ge }

#[derive(Clone,Debug)]
pub enum Expr {
  Nil(Span),
  Bool(Span, bool),
  Int(Span, BigInt),
  Str(Span, String),
  Id(Span, String),
  Call(Span, Box<Expr>, Vec<Expr>),
  Dot(Span, Box<Expr>, String),
  Un(Span, UnOp, Box<Expr>),
  Bin(Span, BinOp, Box<Expr>, Box<Expr>),
  And(Span, Box<Expr>, Box<Expr>),
  Or(Span, Box<Expr>, Box<Expr>),
  Block(Span, Vec<Expr>),
  Var(Span, String, Box<Expr>),
  As(Span, Box<Expr>, Box<Expr>),
  Continue(Span),
  Break(Span),
  Return(Span, Box<Expr>),
  If(Span, Box<Expr>, Box<Expr>, Box<Expr>),
  While(Span, Box<Expr>, Box<Expr>),
  For(Span, String, Box<Expr>, Box<Expr>),
  Func(Span, FuncDef),
  Type(Span, String, Vec<String>, Vec<FuncDef>),
  Lambda(Span, Vec<String>, Box<Expr>)
}

impl Expr {
  pub fn span(&self) -> &Span {
    match self {
      Expr::Nil(span) => span,
      Expr::Bool(span, _) => span,
      Expr::Int(span, _) => span,
      Expr::Str(span, _) => span,
      Expr::Id(span, _) => span,
      Expr::Call(span, _, _) => span,
      Expr::Dot(span, _, _) => span,
      Expr::Un(span, _, _) => span,
      Expr::Bin(span, _, _, _) => span,
      Expr::And(span, _, _) => span,
      Expr::Or(span, _, _) => span,
      Expr::Block(span, _) => span,
      Expr::Var(span, _, _) => span,
      Expr::As(span, _, _) => span,
      Expr::Continue(span) => span,
      Expr::Break(span) => span,
      Expr::Return(span, _) => span,
      Expr::If(span, _, _, _) => span,
      Expr::While(span, _, _) => span,
      Expr::For(span, _, _, _) => span,
      Expr::Func(span, _) => span,
      Expr::Type(span, _, _, _) => span,
      Expr::Lambda(span, _, _) => span,
    }
  }
}

pub type FuncDef = (String, Vec<String>, Box<Expr>);

#[derive(Clone,Debug)]
pub struct Program(pub Vec<Expr>);
