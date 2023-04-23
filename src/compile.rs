use crate::ast;
use crate::error::*;
use crate::vm::*;

pub enum Insn {
  // Constant
  Const(Val),
  // Functions and type constructors
  Func(Vec<String>, usize),
  Ctor(Vec<String>, Vec<(String, Vec<String>, usize)>),
  // Variables
  Load(ast::Span, String),
  Store(ast::Span, String),
  Declare(ast::Span, String),
  // Field access
  LoadField(ast::Span, String),
  StoreField(ast::Span, String),
  // N-ary call
  Call(ast::Span, usize),
  // Unary operation
  Un(ast::Span, ast::UnOp),
  // Binary operation
  Bin(ast::Span, ast::BinOp),
  // Jumps
  Jump(usize),
  JumpTrue(ast::Span, usize),
  JumpFalse(ast::Span, usize),
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

struct Compiler<'err_ctx, 'a> {
  err_ctx: &'err_ctx mut ErrorContext,
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

impl<'err_ctx, 'a> Compiler<'err_ctx, 'a> {
  fn new(err_ctx: &'err_ctx mut ErrorContext, vm: &'a mut Vm) -> Self {
    Compiler {
      err_ctx,
      vm,
      funcs: Vec::new(),
      loops: Vec::new()
    }
  }

  /// Compile an expression
  fn compile_expr(&mut self, expr: &ast::Expr) {
    match expr {
      ast::Expr::Nil(_span) => { self.vm.emit_const(Val::Nil); }
      ast::Expr::Bool(_span, b) => { self.vm.emit_const(Val::Bool(b.clone())); }
      ast::Expr::Int(_span, i) => { self.vm.emit_const(Val::Int(i.clone())); }
      ast::Expr::Str(_span, s) => { self.vm.emit_const(Val::Str(s.clone())); }
      ast::Expr::Id(span, id) => { self.vm.emit(Insn::Load(span.clone(), id.clone())); }
      ast::Expr::Call(span, func, args) => {
        for arg in args.iter() {
          self.compile_expr(arg);
        }
        self.compile_expr(func);
        self.vm.emit(Insn::Call(span.clone(), args.len()));
      }
      ast::Expr::Dot(span, obj, id) => {
        self.compile_expr(obj);
        self.vm.emit(Insn::LoadField(span.clone(), id.clone()));
      }
      ast::Expr::Un(span, op, arg) => {
        self.compile_expr(arg);
        self.vm.emit(Insn::Un(span.clone(), op.clone()));
      }
      ast::Expr::Bin(span, op, lhs, rhs) => {
        self.compile_expr(lhs);
        self.compile_expr(rhs);
        self.vm.emit(Insn::Bin(span.clone(), op.clone()));
      }
      ast::Expr::And(_span, lhs, rhs) => {
        self.compile_expr(lhs);
        self.vm.emit(Insn::Dup);
        let patch1 = self.vm.emit(Insn::JumpFalse(lhs.span().clone(), 0));
        self.vm.emit(Insn::Discard);
        self.compile_expr(rhs);
        let end_ip = self.vm.insn_cnt();
        self.vm.patch(patch1, Insn::JumpFalse(lhs.span().clone(), end_ip));
      }
      ast::Expr::Or(_span, lhs, rhs) => {
        self.compile_expr(lhs);
        self.vm.emit(Insn::Dup);
        let patch1 = self.vm.emit(Insn::JumpTrue(lhs.span().clone(), 0));
        self.vm.emit(Insn::Discard);
        self.compile_expr(rhs);
        let end_ip = self.vm.insn_cnt();
        self.vm.patch(patch1, Insn::JumpTrue(lhs.span().clone(), end_ip));
      }
      ast::Expr::Block(_span, exprs) => {
        if exprs.len() > 0 {
          self.compile_scoped(|this| {
            // Evaluate the first N-1 expressions for side effects
            for expr in exprs[0..exprs.len()-1].iter() {
              this.compile_expr(expr);
              this.vm.emit(Insn::Discard);
            }
            // Evaluate the last expression for its value
            this.compile_expr(&exprs[exprs.len()-1]);
          });
        } else {
          // If there are no expressions the block yields nil
          self.vm.emit_const(Val::Nil);
        }
      }
      ast::Expr::Var(span, id, val) => {
        self.compile_expr(val);
        self.vm.emit(Insn::Declare(span.clone(), id.clone()));

        self.vm.emit_const(Val::Nil);
      }
      ast::Expr::As(span, dest, val) => {
        self.compile_expr(val);
        self.compile_assignment(span, dest);

        self.vm.emit_const(Val::Nil);
      }
      ast::Expr::Continue(span) => {
        self.compile_continue(span);

        self.vm.emit_const(Val::Nil);
      }
      ast::Expr::Break(span) => {
        self.compile_break(span);

        self.vm.emit_const(Val::Nil);
      }
      ast::Expr::Return(span, val) => {
        self.compile_expr(val);
        self.compile_return(span);

        self.vm.emit_const(Val::Nil);
      }
      ast::Expr::If(_span, cond, arg1, arg2) => {
        // Compile conditional jump
        self.compile_expr(cond);
        let patch1 = self.vm.emit(Insn::JumpFalse(cond.span().clone(), 0));
        // Compile true case
        self.compile_expr(arg1);
        let patch2 = self.vm.emit(Insn::Jump(0));
        // Compile false case
        let false_ip = self.vm.insn_cnt();
        self.compile_expr(arg2);
        // Backpatch jump targets
        self.vm.patch(patch1, Insn::JumpFalse(cond.span().clone(), false_ip));
        let end_ip = self.vm.insn_cnt();
        self.vm.patch(patch2, Insn::Jump(end_ip));
      }
      ast::Expr::While(_span, cond, body) => {
        // Save continue point
        let cont_ip = self.vm.insn_cnt();

        // Compile conditional
        self.compile_expr(cond);
        let patch1 = self.vm.emit(Insn::JumpFalse(cond.span().clone(), 0));

        // Compile loop body
        let ctx = self.compile_loop(|this| {
          this.compile_expr(body);
          this.vm.emit(Insn::Discard);
        });
        // Jump back to start
        self.vm.emit(Insn::Jump(cont_ip));

        // Save break point
        let brk_ip = self.vm.insn_cnt();

        // Backpatch jump targets
        self.vm.patch(patch1, Insn::JumpFalse(cond.span().clone(), brk_ip));
        for patch in ctx.continues.iter() {
          self.vm.patch(*patch, Insn::Jump(cont_ip));
        }
        for patch in ctx.breaks.iter() {
          self.vm.patch(*patch, Insn::Jump(brk_ip));
        }

        self.vm.emit_const(Val::Nil);
      }
      ast::Expr::For(span, id, iter, body) => {
        // Push iterator to stack
        self.compile_expr(iter);

        // Save continue point
        let cont_ip = self.vm.insn_cnt();

        // Read next value from iterator
        self.vm.emit(Insn::Dup);
        self.vm.emit(Insn::LoadField(iter.span().clone(), String::from("next")));
        self.vm.emit(Insn::Call(iter.span().clone(), 0));

        // Jump to end if nil
        self.vm.emit(Insn::Dup);
        self.vm.emit_const(Val::Nil);
        // NOTE: spans here are irrelevant as below shouldn't cause type errors
        self.vm.emit(Insn::Bin(ast::Span(0,0), ast::BinOp::Eq));
        let patch1 = self.vm.emit(Insn::JumpTrue(ast::Span(0,0), 0));

        // Compile loop body
        let ctx = self.compile_loop(|this| {
          this.compile_scoped(|this| {
            this.vm.emit(Insn::Declare(span.clone(), id.clone()));
            this.compile_expr(body);
            this.vm.emit(Insn::Discard);
          });
        });

        // Jump back to start
        self.vm.emit(Insn::Jump(cont_ip));

        // Save break point
        let brk_ip = self.vm.insn_cnt();

        // Remove iterator and nil from stack
        self.vm.emit(Insn::Discard);
        self.vm.emit(Insn::Discard);

        // Backpatch jump targets
        self.vm.patch(patch1, Insn::JumpTrue(ast::Span(0,0), brk_ip));
        for patch in ctx.continues.iter() {
          self.vm.patch(*patch, Insn::Jump(cont_ip));
        }
        for patch in ctx.breaks.iter() {
          self.vm.patch(*patch, Insn::Jump(brk_ip));
        }

        self.vm.emit_const(Val::Nil);
      }
      ast::Expr::Func(span, (id, params, body)) => {
        let func_ip = self.compile_func(body);
        self.vm.emit(Insn::Func(params.clone(), func_ip));
        self.vm.emit(Insn::Declare(span.clone(), id.clone()));

        self.vm.emit_const(Val::Nil);
      }
      ast::Expr::Type(span, id, fields, parsed_methods) => {
        let mut methods = vec![];
        for (name, params, body) in parsed_methods.iter() {
          let method_ip = self.compile_func(body);
          methods.push((name.clone(), params.clone(), method_ip));
        }
        self.vm.emit(Insn::Ctor(fields.clone(), methods));
        self.vm.emit(Insn::Declare(span.clone(), id.clone()));

        self.vm.emit_const(Val::Nil);
      }
      ast::Expr::Lambda(_span, params, body) => {
        let func_ip = self.compile_func(body);
        self.vm.emit(Insn::Func(params.clone(), func_ip));
      }
    }
  }

  fn compile_scoped<F>(&mut self, cb: F)
    where F: Fn(&mut Compiler)
  {
    if let Some(curfunc) = self.funcs.last_mut() { curfunc.block_cnt += 1; }
    if let Some(curloop) = self.loops.last_mut() { curloop.block_cnt += 1; }
    self.vm.emit(Insn::Enter);
    cb(self);
    if let Some(curfunc) = self.funcs.last_mut() { curfunc.block_cnt -= 1; }
    if let Some(curloop) = self.loops.last_mut() { curloop.block_cnt -= 1; }
    self.vm.emit(Insn::Exit);
  }

  fn compile_loop<F>(&mut self, cb: F) -> LoopCtx
    where F: Fn(&mut Compiler)
  {
    self.loops.push(LoopCtx {
      continues: Vec::new(),
      breaks: Vec::new(),
      block_cnt: 0
    });
    cb(self);
    self.loops.pop().unwrap()
  }

  fn compile_func(&mut self, body: &ast::Expr) -> usize {
    // Jump over function
    let patch1 = self.vm.emit(Insn::Jump(0));
    // Save beginning of the function
    let func_ip = self.vm.insn_cnt();
    // Compile function body
    self.funcs.push(FuncCtx { block_cnt: 0 });
    self.compile_expr(body);
    self.vm.emit(Insn::Return);
    self.funcs.pop();
    // Backpatch jump target
    let end_ip = self.vm.insn_cnt();
    self.vm.patch(patch1, Insn::Jump(end_ip));
    func_ip
  }

  fn compile_assignment(&mut self, span: &ast::Span, dest: &ast::Expr) {
    match dest {
      ast::Expr::Id(_span, id) => {
        self.vm.emit(Insn::Store(span.clone(), id.clone()));
      }
      ast::Expr::Dot(_span, obj, id) => {
        self.compile_expr(obj);
        self.vm.emit(Insn::StoreField(span.clone(), id.clone()));
      }
      expr => {
        self.err_ctx.err(expr.span().clone(), ErrorCondition::LValueRequired);
      }
    }
  }

  fn compile_continue(&mut self, span: &ast::Span) {
    for _ in 0..self.loops.last().unwrap().block_cnt { self.vm.emit(Insn::Exit); }
    let patch = self.vm.emit(Insn::Jump(0));
    if let Some(curloop) = self.loops.last_mut() {
      curloop.continues.push(patch);
    } else {
      self.err_ctx.err(span.clone(), ErrorCondition::ContinueOutsideLoop);
    }
  }

  fn compile_break(&mut self, span: &ast::Span) {
    for _ in 0..self.loops.last().unwrap().block_cnt { self.vm.emit(Insn::Exit); }
    let patch = self.vm.emit(Insn::Jump(0));
    if let Some(curloop) = self.loops.last_mut() {
      curloop.breaks.push(patch);
    } else {
      self.err_ctx.err(span.clone(), ErrorCondition::BreakOutsideLoop);
    }
  }

  fn compile_return(&mut self, span: &ast::Span) {
    if let Some(_) = self.funcs.last() {
      for _ in 0..self.funcs.last().unwrap().block_cnt { self.vm.emit(Insn::Exit); }
      self.vm.emit(Insn::Return);
    } else {
      self.err_ctx.err(span.clone(), ErrorCondition::ReturnOutsideFunc);
    }
  }
}

pub fn compile_program(err_ctx: &mut ErrorContext, vm: &mut Vm, ast::Program(exprs): &ast::Program) {
  let mut compiler = Compiler::new(err_ctx, vm);
  for expr in exprs.iter() {
    compiler.compile_expr(expr);
    compiler.vm.emit(Insn::Discard);
  }
  compiler.vm.emit(Insn::Terminate);
}
