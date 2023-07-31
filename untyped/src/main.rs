use clap::Parser as CliParser;
use std::path::PathBuf;
use untyped::parser::Parser;
use untyped::scanner::Scanner;

#[derive(CliParser, Debug)]
pub struct Cli {
    #[arg(short, long)]
    tokens: bool,

    #[arg(short, long)]
    ast: bool,

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
    println!("Full Beta Reduction:\n\n{}", ast.full());

    Ok(())
}
