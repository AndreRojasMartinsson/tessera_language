use core::str;
use std::{fs::File, path::PathBuf};

use bumpalo::Bump;
use clap::Parser;
use codespan_reporting::files::SimpleFiles;
use color_eyre::eyre::{Context, Result, ensure};
use memmap2::Mmap;
use tessera_language::{lexer::Lexer, parser};

#[derive(Parser)]
#[command(version, long_about = None, about)]
struct Args {
    #[arg(value_name = "INPUT")]
    input_path: PathBuf,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Debug)
        .init();

    let args = Args::parse();
    let path = args.input_path;

    ensure!(
        path.exists(),
        "Ensure input `{}` points to a path that exists",
        path.display()
    );

    ensure!(
        path.is_file(),
        "Ensure input `{}` points to a file",
        path.display()
    );

    let file = File::open(&path).context("Failed to open file")?;
    let mmap = unsafe { Mmap::map(&file).context("Failed to read file")? };
    let source = str::from_utf8(&mmap[..]).context("Source code is not valid UTF-8")?;

    let lexer = Lexer::new(source);
    let mut files = SimpleFiles::new();

    let file_id = files.add(path.file_name().and_then(|s| s.to_str()).unwrap(), source);

    let arena = Bump::new();

    let mut parser = parser::Parser::new(&arena, source, lexer, files, file_id);

    let source_node = parser.parse().context("Failed to parse file")?;

    println!("{source_node:#?}");

    Ok(())
}
