mod lex;
const VERSION: &'static str = env!("CARGO_PKG_VERSION");
fn main() {
    println!("Rust Monkey v{}", VERSION);
}
