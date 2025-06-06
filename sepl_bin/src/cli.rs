use anyhow::Result;

use crate::repl::repl;

pub fn process_args() -> Result<()> {
    repl()?;
    Ok(())
}