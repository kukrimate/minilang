#![feature(coerce_unsized)]
#![feature(hash_raw_entry)]
#![feature(linked_list_cursors)]
#![feature(unsize)]

use lalrpop_util::lalrpop_mod;
use pico_args;
use std::fmt;
use std::path::PathBuf;

lalrpop_mod!(parse);  // Parser
mod ast;              // AST definition
mod compile;          // Bytecode compiler
mod gc;               // Garbage collector
mod vm;               // Virtual machine
mod util;             // Utilities

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

impl fmt::Display for ArgsError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::HelpRequested => {}
      Self::PicoArgsError(err) => { err.fmt(f)? }
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
    input: pargs.free_from_str()?,
  })
}

fn main() {
  // Parse arguments
  let args = parse_args().unwrap();
  // Read program from disk
  let input = std::fs::read_to_string(args.input).unwrap();
  // Parse program
  let parser = parse::ProgramParser::new();
  let program = parser.parse(&input).unwrap();

  // Create virtual machine
  let mut vm = vm::Vm::new();
  // Compile program
  compile::compile_program(&mut vm, &program).unwrap();
  // Execute program
  vm.execute().unwrap();
}
