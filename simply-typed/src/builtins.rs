use crate::term::*;
use crate::types::{con, Type};
use std::collections::HashMap;

lazy_static! {
    pub static ref BUILTINS: HashMap<Term, Term> = {
        HashMap::from([
            (var("and"), Term::and()),
            (var("or"), Term::or()),
            (var("not"), Term::not()),
            (var("plus"), Term::plus()),
            (var("times"), Term::times()),
            //            (var("pow"), Term::pow()),
        ])
    };
}

impl Term {
    pub fn not() -> Self {
        abs(
            "a",
            Type::Bool,
            Term::If(box var("a"), box Term::False, box Term::True),
        )
    }

    pub fn and() -> Self {
        abs(
            "a",
            Type::Bool,
            abs(
                "b",
                Type::Bool,
                Term::If(box var("a"), box var("b"), box Term::False),
            ),
        )
    }

    pub fn or() -> Self {
        abs(
            "a",
            Type::Bool,
            abs(
                "b",
                Type::Bool,
                Term::If(box var("a"), box Term::True, box var("b")),
            ),
        )
    }

    pub fn plus() -> Self {
        Term::Fix(box abs(
            "f",
            con(&Type::Nat, &con(&Type::Nat, &Type::Nat)),
            abs(
                "n",
                Type::Nat,
                abs(
                    "m",
                    Type::Nat,
                    Term::If(
                        box Term::IsZero(box var("n")),
                        box var("m"),
                        box app(
                            app(var("f"), Term::Pred(box var("n"))),
                            Term::Succ(box var("m")),
                        ),
                    ),
                ),
            ),
        ))
    }

    pub fn times() -> Self {
        Term::Fix(box abs(
            "f",
            con(&Type::Nat, &con(&Type::Nat, &Type::Nat)),
            abs(
                "n",
                Type::Nat,
                abs(
                    "m",
                    Type::Nat,
                    Term::If(
                        box app(
                            app(Term::or(), Term::IsZero(box var("n"))),
                            Term::IsZero(box var("m")),
                        ),
                        box Term::Zero,
                        box Term::If(
                            box Term::IsZero(box Term::Pred(box var("n"))),
                            box var("m"),
                            box app(
                                app(Term::plus(), var("m")),
                                app(app(var("f"), Term::Pred(box var("n"))), var("m")),
                            ),
                        ),
                    ),
                ),
            ),
        ))
    }
}
