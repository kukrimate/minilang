#![feature(coerce_unsized)]
#![feature(hash_raw_entry)]
#![feature(linked_list_cursors)]
#![feature(unsize)]
#![feature(if_let_guard)]

use pico_args;
use std::path::{PathBuf,Path};

mod ast;              // Abstract syntax tree
mod parse;            // Syntax analyzer
mod compile;          // Bytecode compiler
mod error;            // Error handling
mod gc;               // Garbage collector
mod vm;               // Virtual machine

const HELP: &str = "\
Interpreter
USAGE:
  interpreter [OPTIONS] [INPUT]
FLAGS:
  -h, --help            Prints help information
OPTIONS:
ARGS:
  <INPUT>               Input file
";

#[derive(Debug)]
enum ArgsError {
  HelpRequested,
  PicoArgsError(pico_args::Error)
}

impl std::fmt::Display for ArgsError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::HelpRequested => {}
      Self::PicoArgsError(err) => { err.fmt(f)?; "\n".fmt(f)?; }
    }
    HELP.fmt(f)
  }
}

impl From<pico_args::Error> for ArgsError {
  fn from(err: pico_args::Error) -> ArgsError {
    ArgsError::PicoArgsError(err)
  }
}

struct Args {
  input: PathBuf
}

fn parse_args() -> Result<Args, ArgsError> {
  let mut pargs = pico_args::Arguments::from_env();
  if pargs.contains(["-h", "--help"]) {
    return Err(ArgsError::HelpRequested);
  }

  Ok(Args {
    input: pargs.free_from_os_str(|path| -> Result<_, ArgsError> { Ok(PathBuf::from(path)) })?,
  })
}

fn main() {
  // Parse arguments
  let args = match parse_args() {
    Ok(args) => args,
    Err(err) => {
      eprintln!("{}", err);
      std::process::exit(1);
    }
  };

  // Read program from disk
  let input = match std::fs::read_to_string(&args.input) {
    Ok(file) => file,
    Err(err) => {
      eprintln!("Failed to read {}: {}", args.input.to_string_lossy(), err);
      std::process::exit(1);
    }
  };

  // Create error context
  let mut err_ctx = error::ErrorContext::new();

  // Create virtual machine
  let mut vm = vm::Vm::new();

  // Parse and compile program
  parse::parse_program(&mut err_ctx, &input)
    .map(|program|
      compile::compile_program(&mut err_ctx, &mut vm, &program));

  // Check for parse or compile errors
  if err_ctx.display_all(&args.input, &input) { std::process::exit(1) }

  // Execute program
  vm.execute(&mut err_ctx);

  // Check for runtime errors
  if err_ctx.display_all(&args.input, &input) { std::process::exit(1) }
}
