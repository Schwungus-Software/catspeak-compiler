use std::{
    fs::File,
    path::{Path, PathBuf},
};

use clap::Parser;
use log::Level;

use crate::{
    eyre::{self, eyre},
    ir::{InterfacePreset, Script},
    parser::CatspeakParser,
};

#[derive(Parser)]
#[command(author, version, about, long_about)]
pub struct Cli {
    #[clap(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
    #[arg(value_parser = parse_file_only)]
    files: Vec<PathBuf>,
}

fn parse_file_only(s: &str) -> eyre::Result<PathBuf> {
    let path: PathBuf = s.parse()?;

    if path.is_file() {
        Ok(path)
    } else {
        Err(eyre!("`{}` is a directory, not a file", s))
    }
}

impl Cli {
    pub fn run(mut self) -> eyre::Result<()> {
        if self.files.is_empty() {
            return Err(eyre!("No files given to compile"));
        }

        let mut error_count = 0;

        for file in self.files.clone() {
            match self.process_file(&file) {
                Ok(()) => (),
                Err(err) => {
                    error!("{}", err);
                    error_count += 1
                }
            }
        }

        let level = {
            if error_count > 0 {
                Level::Error
            } else {
                Level::Info
            }
        };

        log!(level, "Encountered {} errors", error_count);

        Ok(())
    }

    fn process_file(&mut self, path: &Path) -> eyre::Result<()> {
        let path = path.to_path_buf();
        let file = File::open(&path)?;

        let ast = CatspeakParser::parse_input(file)?;

        let ast_output = path.with_extension("ast.json");
        let ast_output = File::create(ast_output)?;
        serde_json::to_writer_pretty(ast_output, &ast)?;

        let ir = Script::new(BasicInterface).produce_ir(ast)?;

        let ir_output = path.with_extension("ir.json");
        let ir_output = File::create(ir_output)?;
        serde_json::to_writer_pretty(ir_output, &ir)?;

        Ok(())
    }
}

struct BasicInterface;

impl InterfacePreset for BasicInterface {
    fn modify(&self, script: &mut Script) {
        script.import_global("show_debug_message");
    }
}
