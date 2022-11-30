use lalrpop_util::lalrpop_mod;
use pico_args;
use std::fmt;
use std::path::PathBuf;

mod ast;              // AST definition
mod interp;           // AST interpreter
lalrpop_mod!(parse);  // Parser
mod val;              // Value types
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
  // Execute program
  interp::execute(&program).unwrap();
}
