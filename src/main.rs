mod repl;
const VERSION: &'static str = env!("CARGO_PKG_VERSION");
fn main() {
    repl::start(format!("Rust Monkey v{}", VERSION)).expect("Error running repl!");
}
