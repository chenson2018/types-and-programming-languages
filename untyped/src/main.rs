use untyped::church::*;
use untyped::scanner::Scanner;
use untyped::term::Term;
use untyped::term::{abs, app, var};

fn main() {
    let source = "λ x. (a b)\n";
    let mut scanner = Scanner::new(source);
    scanner.scan().unwrap();
    //println!("Tokens: \n\n{:#?}\n", scanner.tokens);

    //    for i in 0..5 {
    //        let term = <Term as From<usize>>::from(i);
    //        println!("original: {term}");
    //
    //        let full = term.full();
    //        println!("full: {full}\n\n");
    //    }

    //    let term = app(NOT.clone(), TRU.clone());
    //    println!("{term}");
    //    println!("full: {}", term.full());

    let term = app(app(PLUS.clone(), 1.into()), 0.into());
    println!("{term}");
    println!("full: {}", term.full());

    //let term = abs("x", abs("y", var("x")));
    //let term = abs("x", abs("y", abs("z", app(app(var("x"), var("z"))  , app(var("y"), var("z")))          )));
    //    let term = abs("z", app( abs("y", app(var("y"), abs("x", var("x")))) ,  abs("x", app(var("z"), var("x"))) ));
    //    println!("{}", term);
    //    println!("{}", term.to_anon());

    //    let term = abs("x", app(var("x"), var("y")));
    //    println!("{}", term);
    //    println!("{}", term.to_anon());
    //
    //    let term = abs("a", app(var("a"), var("b")));
    //    println!("\n{}", term);
    //    println!("{}", term.to_anon());
}
