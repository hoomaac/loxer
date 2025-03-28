use std::{fs, io, path::PathBuf};
use clap::{Parser, Subcommand};
use loxer::Lexer;
use miette::{Context, IntoDiagnostic};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize {
        filename: PathBuf
    },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    parse(&args) 
}

fn parse(args: &Args) -> miette::Result<()> {
    match &args.command {
        Commands::Tokenize{filename} => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("failed to read file: {}", filename.display()))?;
            
            for token in Lexer::new(&file_contents) {
                let token = token?;
                println!("{token}");
            }

        },
    }

    Ok(())
}
