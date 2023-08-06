use simply_typed::error::LcErrorReporter;
use simply_typed::parser::Parser;
use simply_typed::scanner::Scanner;

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

    let mut scanner = Scanner::new(&source);
    scanner.scan()?;

    if args.tokens {
        println!("Tokens: \n\n{:#?}\n", scanner.borrow_tokens());
    }

    let mut parser = Parser::from(&scanner);

    match parser.parse(args.std) {
        Ok(ast) => {
            if args.ast {
                println!("AST: \n\n{:#?}\n", ast);
            }

            println!("Input code:\n\n{}\n", ast);

            if !args.skip_typecheck {
                let dtype = ast.dtype()?;
                println!("Type:\n\n{}\n", dtype);
            }

            if !args.skip_eval {
                println!("Full Beta Reduction:\n\n{}", ast.full());
            }

            Ok(())
        }
        Err(e) => Err(LcErrorReporter::new(e, args.path, source, "Parser").into()),
    }
}
