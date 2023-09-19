#[macro_use]
extern crate log;

use clap::Parser;
use cli::Cli;
use color_eyre::eyre;

mod cli;
mod ir;
mod parser;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    Cli::parse().run()?;
    Ok(())
}
