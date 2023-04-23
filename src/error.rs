use std::cmp::{max, min};
use crate::*;
use crate::ast::*;

/// Error handling context
pub struct ErrorContext {
  errors: Vec<(Span, ErrorCondition)>
}

impl ErrorContext {
  pub fn new() -> ErrorContext {
    ErrorContext {
      errors: Vec::new()
    }
  }

  pub fn err(&mut self, span: Span, cond: ErrorCondition) {
    self.errors.push((span, cond));
  }

  pub fn display_all(&self, path: &Path, input: &str) -> bool {
    // Bail on no errors
    if self.errors.len() == 0 { return false }
    // Report errors
    for (span, condition) in self.errors.iter() {
      display_err(path, input, span, condition);
    }
    true
  }
}

/// List of error conditions from all modules
pub enum ErrorCondition {
  InvalidToken,
  UnexpectedToken,
  LValueRequired,
  ContinueOutsideLoop,
  BreakOutsideLoop,
  ReturnOutsideFunc,
  UnknownIdentifier(String),
  RedefinitionOfIdentifier(String),
  TypeError,
  DivisionByZero
}

impl std::fmt::Display for ErrorCondition {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ErrorCondition::InvalidToken => "invalid token".fmt(f),
      ErrorCondition::UnexpectedToken => "unexpected token".fmt(f),
      ErrorCondition::LValueRequired => "lvalue expression required".fmt(f),
      ErrorCondition::ContinueOutsideLoop => "continue statement outside loop".fmt(f),
      ErrorCondition::BreakOutsideLoop => "break statement outside loop".fmt(f),
      ErrorCondition::ReturnOutsideFunc => "return statement outside function".fmt(f),
      ErrorCondition::UnknownIdentifier(id) => write!(f, "unknown identifier {}", id),
      ErrorCondition::RedefinitionOfIdentifier(id) => write!(f, "re-definition of {}", id),
      ErrorCondition::TypeError => write!(f, "type error"),
      ErrorCondition::DivisionByZero => write!(f, "division by zero")
    }
  }
}

/// Display an error condition
fn display_err(path: &Path, input: &str, Span(err_begin, err_end): &Span, condition: &ErrorCondition) {
  // Compute line and column numbers
  let (line, col) = position_to_line_column(input, *err_begin);

  // Split surrounding context into lines
  let lines = split_lines(input,
                            scan_backward_nl(input, *err_begin),
                            scan_forward_nl(input, *err_end));

  // Width to pad line numbers to
  let max_line_num_width = (line + lines.len() - 1).to_string().len();

  // Print heading
  println!("error: {}\n{}--> {}:{}:{}\n{} |",
           condition,
           " ".repeat(max_line_num_width),
           path.to_string_lossy(),
           line,
           col,
           " ".repeat(max_line_num_width));

  // Print lines
  for (index, (start, text)) in lines.iter().enumerate() {
    // Print line number + line text
    println!("{} | {}", line + index, text);
    // Highlight offending text
    if *start < *err_end {
      let pad = max(*err_begin, *start) - *start;
      let cnt = min(*err_end - *start, text.len()) - pad;
      println!("{} | {}{}",
               " ".repeat(max_line_num_width),
               " ".repeat(pad),
               "^".repeat(cnt));
    }
  }
}

/// Convert a position to a line and column number
fn position_to_line_column(input: &str, pos: usize) -> (usize, usize) {
  let mut line = 1;
  let mut col = 1;
  for index in 0..pos {
    if &input[index..index+1] == "\n" {
      line += 1;
      col = 1;
    } else {
      col += 1;
    }
  }
  (line, col)
}

/// Scan backward for a newline
fn scan_backward_nl(input: &str, mut pos: usize) -> usize {
  while pos > 0 && &input[pos - 1..pos] != "\n" {
    pos -= 1;
  }
  pos
}

/// Scan forward for a newline
fn scan_forward_nl(input: &str, mut pos: usize) -> usize {
  while pos < input.len() && &input[pos..pos+1] != "\n" {
    pos += 1;
  }
  pos
}

/// Collect a list of lines contained in an input span
fn split_lines(input: &str, mut pos: usize, end: usize) -> Vec<(usize, &str)> {
  let mut lines = Vec::new();
  while {
    let begin = pos;
    while pos < end && &input[pos..pos + 1] != "\n" {
      pos += 1;
    }
    if begin < pos {
      lines.push((begin, &input[begin..pos]));
    }
    pos < end
  } {
    pos += 1
  }
  lines
}
