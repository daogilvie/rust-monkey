use std::io::{self, stdin, stdout, Write};
use crate::lex;
const PROMPT: &str = ">> ";

pub fn start(preamble: String) -> io::Result<()> {
    let stdin = stdin();
    let mut out = stdout();
    writeln!(out, "{}", preamble).expect("Error writing to stdout");
    loop {
        let mut input_buffer = String::new();
        write!(out, "{}", PROMPT).expect("Error writing to stdout");
        out.flush()?;
        stdin.read_line(&mut input_buffer)?;
        // If we type "quit", then quit
        let trimmed = input_buffer.trim();
        if trimmed == "quit" {
            break;
        }
        let lexer = lex::Lexer::for_str(&trimmed);
        for token in lexer {
            writeln!(out, "{:?}", token).unwrap();
        }
    }
    Ok(())
}
