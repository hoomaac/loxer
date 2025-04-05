use clap::{Parser, Subcommand};
use loxer::{Lexer, TokenError};
use miette::{Context, IntoDiagnostic};
use std::{fs, io, path::PathBuf};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    parse(&args)
}

fn parse(args: &Args) -> miette::Result<()> {
    let mut has_error = false;
    match &args.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("failed to read file: {}", filename.display()))?;

            for token in Lexer::new(&file_contents) {
                let token = match token {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("{e:?}");
                        if let Some(invalid) = e.downcast_ref::<TokenError>() {
                            has_error = true;
                            eprintln!(
                                "[line {}] Error: Invalid character: {}",
                                invalid.line(),
                                invalid.token
                            );
                        }
                        continue;
                    }
                };
                println!("{token}");
            }
            println!("EOF null");
        }
    }

    has_error.then(|| std::process::exit(1));

    Ok(())
}
