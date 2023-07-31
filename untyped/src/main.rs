use untyped::church::*;
//use untyped::scanner::Scanner;
use untyped::term::*;
use untyped::term::{Term, TermAnon};

fn main() {
    let term = app(app(Term::times(), 2.into()), 2.into());
    println!("{term}");
    println!("{}", term.to_anon());
    println!("{}", term.full());
}
