use simply_typed::parser::Parser;
use simply_typed::scanner::Scanner;

use clap::Parser as CliParser;

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

    path: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Cli::parse();
    let source = std::fs::read_to_string(&args.path)?;

    let mut scanner = Scanner::new(&source);
    scanner.scan()?;

    if args.tokens {
        println!("Tokens: \n\n{:#?}\n", scanner.borrow_tokens());
    }

    let mut parser = Parser::from(&scanner);
    let ast = parser.parse()?;

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
