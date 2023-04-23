use crate::ast::*;
use crate::error::*;
use lalrpop_util::{lalrpop_mod, ParseError};

lalrpop_mod!(grammar, "/parse/grammar.rs");
mod util;

pub fn parse_program(err_ctx: &mut ErrorContext, input: &str) -> Option<Program> {
  grammar::ProgramParser::new()
    .parse(input)
    .map_err(|err| {
      match err {
        ParseError::InvalidToken { location } => {
          err_ctx.err(Span(location, location + 1), ErrorCondition::InvalidToken);
        }
        ParseError::UnrecognizedToken { token: (begin, _, end), .. } => {
          err_ctx.err(Span(begin, end), ErrorCondition::UnexpectedToken);
        }
        ParseError::UnrecognizedEOF { .. } |
        ParseError::ExtraToken { .. } |
        ParseError::User { .. } => {
          // NOTE: these are not possible with our current grammar
          unreachable!()
        }
      }
    })
    .ok()
}
