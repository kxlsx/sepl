use anyhow::Result;

mod repl;

fn main() -> Result<()> {
    // TODO: print errors nicely
    repl::repl()
}
