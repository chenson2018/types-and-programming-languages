use isorec::builtins::BUILTINS;
use isorec::error::LcErrorReporter;
use isorec::parser::Parser;
use isorec::scanner::Scanner;

use clap::Parser as CliParser;
use main_error::MainError;

use std::path::PathBuf;

#[derive(CliParser, Debug)]
pub struct Cli {
    #[arg(short, long)]
    tokens: bool,

    #[arg(short, long)]
    ast: bool,

    #[arg(long)]
    skip_typecheck: bool,

    #[arg(long)]
    skip_eval: bool,

    #[arg(long)]
    std: bool,

    path: PathBuf,
}

fn main() -> Result<(), MainError> {
    let args = Cli::parse();
    let source = std::fs::read_to_string(&args.path)?;

    let source = if args.std {
        format!("{}\n{}", BUILTINS, source)
    } else {
        source
    };

    let mut scanner = Scanner::new(&source, false);
    scanner.scan()?;

    if args.tokens {
        println!("Tokens: \n\n{:#?}\n", scanner.borrow_tokens());
    }

    let mut parser = Parser::from(&scanner);

    match parser.parse() {
        Ok(ast) => {
            if args.ast {
                println!("AST: \n\n{:#?}\n", ast);
            }

            println!("Input code:\n\n{}\n", ast);

            if !args.skip_typecheck {
                match ast.dtype() {
                    Ok(dtype) => println!("Type:\n\n{}\n", dtype),
                    Err(e) => return Err(LcErrorReporter::new(e, args.path, source).into()),
                }
            }

            if !args.skip_eval {
                let eval = ast.full();
                println!("Full Beta Reduction:\n\n{}", eval);
            }

            Ok(())
        }
        Err(e) => Err(LcErrorReporter::new(e, args.path, source).into()),
    }
}
