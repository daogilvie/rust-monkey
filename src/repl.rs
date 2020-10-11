use crate::eval;
use crate::lex;
use crate::parse;
use std::io::{self, stdin, stdout, Write};

const PROMPT: &str = ">> ";

pub fn start(preamble: String) -> io::Result<()> {
    let stdin = stdin();
    let mut out = stdout();
    let env = eval::Environment::new();
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
        let mut parser = parse::Parser::for_lexer(lexer);
        let program;
        {
            program = parse::parse(&mut parser);
        }
        match program {
            Ok(p) => match eval::eval_program(p, Some(&env)) {
                Ok(r) => print!("{}", r),
                Err(e) => eprintln!("Eval Error: {}", e),
            },
            Err(s) => {
                eprintln!("{}", s);
                for e in parser.get_errors() {
                    eprintln!(" -> {}", e)
                }
            }
        }
        println!("");
    }
    Ok(())
}
