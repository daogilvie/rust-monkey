// mod eval;
mod lex;
// mod object;
mod parse;
// mod repl;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");
fn main() {
    println!("Rust Monkey v{}", VERSION);
    // repl::start(format!("Rust Monkey v{}", VERSION)).expect("Error running repl!");
}
