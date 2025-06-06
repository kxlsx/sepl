mod cli;
mod repl;

fn main() {
    cli::process_args().unwrap();
}
