use untyped::church::*;
//use untyped::scanner::Scanner;
use untyped::term::*;
use untyped::term::{Term, TermAnon};

fn main() {
    let t: TermAnon = 3.into();
    println!("{t}");
}
