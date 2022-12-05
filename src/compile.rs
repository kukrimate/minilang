use crate::ast;
use crate::vm::*;
use crate::gc::*;

pub enum Insn {
  // Constant
  Const(GcPtr<Val>),
  // Functions and type constructors
  Func(Vec<String>, usize),
  Ctor(Vec<String>, Vec<(String, Vec<String>, usize)>),
  // Variables
  Load(String),
  Store(String),
  Declare(String),
  // Field access
  LoadField(String),
  StoreField(String),
  // N-ary call
  Call(usize),
  // Unary operation
  Un(ast::UnOp),
  // Binary operation
  Bin(ast::BinOp),
  // Jumps
  Jump(usize),
  JumpTrue(usize),
  JumpFalse(usize),
  // Stack frame allocation and de-allocation
  Enter,
  Exit,
  // Duplicate last temporary
  Dup,
  // Discard the last temporary
  Discard,
  // Return from a function
  Return,
  // Terminate program
  Terminate
}

#[derive(Debug)]
pub enum CErr {
  LValueRequired,
  ContinueOutsideLoop,
  BreakOutsideLoop,
  ReturnOutsideFunc
}

struct Compiler<'a> {
  vm: &'a mut Vm,
  funcs: Vec<FuncCtx>,
  loops: Vec<LoopCtx>,
}

struct FuncCtx {
  block_cnt: usize
}

struct LoopCtx {
  block_cnt: usize,
  continues: Vec<usize>,
  breaks: Vec<usize>
}

impl<'a> Compiler<'a> {
  fn new(vm: &'a mut Vm) -> Compiler {
    Compiler {
      vm,
      funcs: Vec::new(),
      loops: Vec::new()
    }
  }

  /// Compile an expression for its value

  fn compile_value_expr(&mut self, expr: &ast::Expr) -> Result<(), CErr> {
    match expr {
      ast::Expr::Nil => { self.vm.emit_const(Val::Nil); }
      ast::Expr::Bool(b) => { self.vm.emit_const(Val::Bool(b.clone())); }
      ast::Expr::Int(i) => { self.vm.emit_const(Val::Int(i.clone())); }
      ast::Expr::Str(s) => { self.vm.emit_const(Val::Str(s.clone())); }
      ast::Expr::Id(id) => { self.vm.emit(Insn::Load(id.clone())); }
      ast::Expr::Call(func, args) => {
        for arg in args.iter() {
          self.compile_value_expr(arg)?;
        }
        self.compile_value_expr(func)?;
        self.vm.emit(Insn::Call(args.len()));
      }
      ast::Expr::Dot(obj, id) => {
        self.compile_value_expr(obj)?;
        self.vm.emit(Insn::LoadField(id.clone()));
      }
      ast::Expr::Un(op, arg) => {
        self.compile_value_expr(arg)?;
        self.vm.emit(Insn::Un(op.clone()));
      }
      ast::Expr::Bin(op, lhs, rhs) => {
        self.compile_value_expr(lhs)?;
        self.compile_value_expr(rhs)?;
        self.vm.emit(Insn::Bin(op.clone()));
      }
      ast::Expr::And(lhs, rhs) => {
        self.compile_value_expr(lhs)?;
        self.vm.emit(Insn::Dup);
        let patch1 = self.vm.emit(Insn::JumpFalse(0));
        self.vm.emit(Insn::Discard);
        self.compile_value_expr(rhs)?;
        let end_ip = self.vm.insn_cnt();
        self.vm.patch(patch1, Insn::JumpFalse(end_ip));
      }
      ast::Expr::Or(lhs, rhs) => {
        self.compile_value_expr(lhs)?;
        self.vm.emit(Insn::Dup);
        let patch1 = self.vm.emit(Insn::JumpTrue(0));
        self.vm.emit(Insn::Discard);
        self.compile_value_expr(rhs)?;
        let end_ip = self.vm.insn_cnt();
        self.vm.patch(patch1, Insn::JumpTrue(end_ip));
      }
      ast::Expr::Block(exprs) => {
        if exprs.len() > 0 {
          self.compile_enter();
          {
            // Evaluate the first N-1 expressions for side effects
            for expr in exprs[0..exprs.len()-1].iter() {
              self.compile_side_effect_expr(expr)?;
            }
            // Evaluate the last expression for its value
            self.compile_value_expr(&exprs[exprs.len()-1])?;
          }
          self.compile_exit();
        } else {
          // If there are no expressions the block yields nil
          self.vm.emit_const(Val::Nil);
        }
      }
      ast::Expr::If(cond, arg1, arg2) => {
        // Compile conditional jump
        self.compile_value_expr(cond)?;
        let patch1 = self.vm.emit(Insn::JumpFalse(0));
        // Compile true case
        self.compile_value_expr(arg1)?;
        let patch2 = self.vm.emit(Insn::Jump(0));
        // Compile false case
        let false_ip = self.vm.insn_cnt();
        self.compile_value_expr(arg2)?;
        // Backpatch jump targets
        self.vm.patch(patch1, Insn::JumpFalse(false_ip));
        let end_ip = self.vm.insn_cnt();
        self.vm.patch(patch2, Insn::Jump(end_ip));
      }
      ast::Expr::Lambda(params, body) => {
        let func_ip = self.compile_func(body)?;
        self.vm.emit(Insn::Func(params.clone(), func_ip));
      }
      // These expressions only have side effects, and always yield nil
      ast::Expr::Var(..) |
      ast::Expr::As(..) |
      ast::Expr::Continue |
      ast::Expr::Break |
      ast::Expr::Return(..) |
      ast::Expr::While(..) |
      ast::Expr::For(..) |
      ast::Expr::Func(..) |
      ast::Expr::Type(..) => {
        self.compile_side_effect_expr(expr)?;
        self.vm.emit_const(Val::Nil);
      }
    }
    Ok(())
  }

  /// Compile an expression for its side effects

  fn compile_side_effect_expr(&mut self, expr: &ast::Expr) -> Result<(), CErr> {
    match expr {
      ast::Expr::Nil |
      ast::Expr::Bool(..) |
      ast::Expr::Int(..) |
      ast::Expr::Str(..) |
      ast::Expr::Id(..) |
      ast::Expr::Lambda(..) => {}
      ast::Expr::Call(func, args) => {
        for arg in args.iter() {
          self.compile_value_expr(arg)?;
        }
        self.compile_value_expr(func)?;
        self.vm.emit(Insn::Call(args.len()));
        self.vm.emit(Insn::Discard);
      }
      ast::Expr::Dot(obj, _) => {
        self.compile_side_effect_expr(obj)?;
      }
      ast::Expr::Un(_, arg) => {
        self.compile_side_effect_expr(arg)?;
      }
      ast::Expr::Bin(_, lhs, rhs) => {
        self.compile_side_effect_expr(lhs)?;
        self.compile_side_effect_expr(rhs)?;
      }
      ast::Expr::And(lhs, rhs) => {
        self.compile_value_expr(lhs)?;
        let patch1 = self.vm.emit(Insn::JumpFalse(0));
        self.compile_side_effect_expr(rhs)?;
        let end_ip = self.vm.insn_cnt();
        self.vm.patch(patch1, Insn::JumpFalse(end_ip));
      }
      ast::Expr::Or(lhs, rhs) => {
        self.compile_value_expr(lhs)?;
        let patch1 = self.vm.emit(Insn::JumpTrue(0));
        self.compile_side_effect_expr(rhs)?;
        let end_ip = self.vm.insn_cnt();
        self.vm.patch(patch1, Insn::JumpTrue(end_ip));
      }
      ast::Expr::Block(exprs) => {
        if exprs.len() > 0 {
          self.compile_enter();
          {
            for expr in exprs.iter() {
              self.compile_side_effect_expr(expr)?;
            }
          }
          self.compile_exit();
        }
      }
      ast::Expr::Var(id, val) => {
        self.compile_value_expr(val)?;
        self.vm.emit(Insn::Declare(id.clone()));
      }
      ast::Expr::As(dest, val) => {
        self.compile_value_expr(val)?;
        self.compile_assignment(dest)?;
      }
      ast::Expr::Continue => {
        self.compile_continue()?;
      }
      ast::Expr::Break => {
        self.compile_break()?;
      }
      ast::Expr::Return(val) => {
        self.compile_value_expr(val)?;
        self.compile_return()?;
      }
      ast::Expr::If(cond, arg1, arg2) => {
        // Compile conditional jump
        self.compile_value_expr(cond)?;
        let patch1 = self.vm.emit(Insn::JumpFalse(0));
        // Compile true case
        self.compile_side_effect_expr(arg1)?;
        let patch2 = self.vm.emit(Insn::Jump(0));
        // Compile false case
        let false_ip = self.vm.insn_cnt();
        self.compile_side_effect_expr(arg2)?;
        // Backpatch jump targets
        self.vm.patch(patch1, Insn::JumpFalse(false_ip));
        self.vm.patch(patch2, Insn::Jump(self.vm.insn_cnt()));
      }
      ast::Expr::While(cond, body) => {
        // Save continue pooint
        let cont_ip = self.vm.insn_cnt();

        // Compile conditional
        self.compile_value_expr(cond)?;
        let patch1 = self.vm.emit(Insn::JumpFalse(0));

        // Compile loop body
        let ctx = self.compile_loop(|this| {
          this.compile_side_effect_expr(body)
        })?;
        // Jump back to start
        self.vm.emit(Insn::Jump(cont_ip));

        // Save break point
        let brk_ip = self.vm.insn_cnt();

        // Backpatch jump targets
        self.vm.patch(patch1, Insn::JumpFalse(brk_ip));
        for patch in ctx.continues.iter() {
          self.vm.patch(*patch, Insn::Jump(cont_ip));
        }
        for patch in ctx.breaks.iter() {
          self.vm.patch(*patch, Insn::Jump(brk_ip));
        }
      }
      ast::Expr::For(id, iter, body) => {
        // Push iterator to stack
        self.compile_value_expr(iter)?;

        // Save continue point
        let cont_ip = self.vm.insn_cnt();

        // Read next value from iterator
        self.vm.emit(Insn::Dup);
        self.vm.emit(Insn::LoadField(String::from("next")));
        self.vm.emit(Insn::Call(0));

        // Jump to end if nil
        self.vm.emit(Insn::Dup);
        self.vm.emit_const(Val::Nil);
        self.vm.emit(Insn::Bin(ast::BinOp::Eq));
        let patch1 = self.vm.emit(Insn::JumpTrue(0));

        // Compile loop body
        let ctx = self.compile_loop(|this| {
          this.compile_enter();
          this.vm.emit(Insn::Declare(id.clone()));
          this.compile_side_effect_expr(body)?;
          this.compile_exit();
          Ok(())
        })?;

        // Jump back to start
        self.vm.emit(Insn::Jump(cont_ip));

        // Save break point
        let brk_ip = self.vm.insn_cnt();

        // Remove iterator and nil from stack
        self.vm.emit(Insn::Discard);
        self.vm.emit(Insn::Discard);

        // Backpatch jump targets
        self.vm.patch(patch1, Insn::JumpTrue(brk_ip));
        for patch in ctx.continues.iter() {
          self.vm.patch(*patch, Insn::Jump(cont_ip));
        }
        for patch in ctx.breaks.iter() {
          self.vm.patch(*patch, Insn::Jump(brk_ip));
        }
      }
      ast::Expr::Func((id, params, body)) => {
        let func_ip = self.compile_func(body)?;
        self.vm.emit(Insn::Func(params.clone(), func_ip));
        self.vm.emit(Insn::Declare(id.clone()));
      }
      ast::Expr::Type(id, fields, parsed_methods) => {
        let mut methods = vec![];
        for (name, params, body) in parsed_methods.iter() {
          let method_ip = self.compile_func(body)?;
          methods.push((name.clone(), params.clone(), method_ip));
        }
        self.vm.emit(Insn::Ctor(fields.clone(), methods));
        self.vm.emit(Insn::Declare(id.clone()));
      }
    }
    Ok(())
  }

  fn compile_loop<F>(&mut self, cb: F) -> Result<LoopCtx, CErr>
    where F: Fn(&mut Compiler) -> Result<(), CErr>
  {
    self.loops.push(LoopCtx {
      continues: Vec::new(),
      breaks: Vec::new(),
      block_cnt: 0 });
    cb(self)?;
    Ok(self.loops.pop().unwrap())
  }

  fn compile_func(&mut self, body: &ast::Expr) -> Result<usize, CErr>
  {
    // Jump over function
    let patch1 = self.vm.emit(Insn::Jump(0));
    // Save beginning of the function
    let func_ip = self.vm.insn_cnt();
    // Compile function body
    self.funcs.push(FuncCtx { block_cnt: 0 });
    self.compile_value_expr(body)?;
    self.vm.emit(Insn::Return);
    self.funcs.pop();
    // Backpatch jump target
    let end_ip = self.vm.insn_cnt();
    self.vm.patch(patch1, Insn::Jump(end_ip));
    Ok(func_ip)
  }

  fn compile_assignment(&mut self, dest: &ast::Expr) -> Result<(), CErr> {
    match dest {
      ast::Expr::Id(id) => {
        self.vm.emit(Insn::Store(id.clone()));
        Ok(())
      }
      ast::Expr::Dot(obj, id) => {
        self.compile_value_expr(obj)?;
        self.vm.emit(Insn::StoreField(id.clone()));
        Ok(())
      }
      _ => {
        Err(CErr::LValueRequired)
      }
    }
  }

  fn compile_enter(&mut self) {
    if let Some(curfunc) = self.funcs.last_mut() {
      curfunc.block_cnt += 1;
    }
    if let Some(curloop) = self.loops.last_mut() {
      curloop.block_cnt += 1;
    }
    self.vm.emit(Insn::Enter);
  }

  fn compile_exit(&mut self) {
    if let Some(curfunc) = self.funcs.last_mut() {
      curfunc.block_cnt -= 1;
    }
    if let Some(curloop) = self.loops.last_mut() {
      curloop.block_cnt -= 1;
    }
    self.vm.emit(Insn::Exit);
  }

  fn compile_continue(&mut self) -> Result<(), CErr> {
    for _ in 0..self.loops.last().unwrap().block_cnt { self.vm.emit(Insn::Exit); }
    let patch = self.vm.emit(Insn::Jump(0));
    if let Some(curloop) = self.loops.last_mut() {
      curloop.continues.push(patch);
      Ok(())
    } else {
      Err(CErr::ContinueOutsideLoop)
    }
  }

  fn compile_break(&mut self) -> Result<(), CErr> {
    for _ in 0..self.loops.last().unwrap().block_cnt { self.vm.emit(Insn::Exit); }
    let patch = self.vm.emit(Insn::Jump(0));
    if let Some(curloop) = self.loops.last_mut() {
      curloop.breaks.push(patch);
      Ok(())
    } else {
      Err(CErr::BreakOutsideLoop)
    }
  }

  fn compile_return(&mut self) -> Result<(), CErr> {
    if let Some(_) = self.funcs.last() {
      for _ in 0..self.funcs.last().unwrap().block_cnt { self.vm.emit(Insn::Exit); }
      self.vm.emit(Insn::Return);
      Ok(())
    } else {
      Err(CErr::ReturnOutsideFunc)
    }
  }
}

pub fn compile_program(vm: &mut Vm, ast::Program(exprs): &ast::Program) -> Result<(), CErr> {
  let mut compiler = Compiler::new(vm);
  for expr in exprs.iter() {
    compiler.compile_side_effect_expr(expr)?;
  }
  compiler.vm.emit(Insn::Terminate);
  Ok(())
}
