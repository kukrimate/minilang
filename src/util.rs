pub fn parse_str_lit(lit: &str) -> Option<String> {
  let mut chars = lit.chars();
  let mut result = String::new();

  // First character must be "
  match chars.next() {
    Some('"') => (),
    _ => return None,
  }

  // Loop until closing "
  loop {
    match chars.next() {
      // Missing delimiter
      None => return None,
      // End of string
      Some('"') => break,
      // Escape sequence
      Some('\\') => result.push(parse_esc(&mut chars)?),
      // Regular char
      Some(c) => result.push(c)
    }
  }

  // Make sure there is nothing left
  if let Some(..) = chars.next() {
    return None
  }

  Some(result)
}

fn parse_esc(chars: &mut std::str::Chars<'_>) -> Option<char> {
  match chars.next() {
    Some('n') => Some('\n'),
    Some('r') => Some('\r'),
    Some('t') => Some('\t'),
    _ => None
  }
}
