use crate::term::*;
use crate::types::{con, Type};
use std::collections::HashMap;

lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Term> = {
        HashMap::from([
            ("and", Term::and()),
            ("or", Term::or()),
            ("not", Term::not()),
            ("plus", Term::plus()),
            ("times", Term::times()),
            ("pow", Term::pow()),
            ("fact", Term::fact()),
            ("rev_nat", Term::rev_nat()),
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

    pub fn pow() -> Self {
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
                        box Term::Succ(box Term::Zero),
                        box Term::If(
                            box Term::IsZero(box Term::Pred(box var("n"))),
                            box var("m"),
                            box app(
                                app(Term::times(), var("m")),
                                app(app(var("f"), Term::Pred(box var("n"))), var("m")),
                            ),
                        ),
                    ),
                ),
            ),
        ))
    }

    pub fn fact() -> Self {
        Term::Fix(box abs(
            "f",
            con(&Type::Nat, &Type::Nat),
            abs(
                "n",
                Type::Nat,
                Term::If(
                    box app(
                        app(Term::or(), Term::IsZero(box var("n"))),
                        Term::IsZero(box Term::Pred(box var("n"))),
                    ),
                    box Term::Succ(box Term::Zero),
                    box app(
                        app(Term::times(), var("n")),
                        app(var("f"), Term::Pred(box var("n"))),
                    ),
                ),
            ),
        ))
    }

    pub fn rev_nat() -> Self {
        app(
            Term::Fix(box abs(
                "f",
                con(
                    &Type::List(box Type::Nat),
                    &con(&Type::List(box Type::Nat), &Type::List(box Type::Nat)),
                ),
                abs(
                    "x",
                    Type::List(box Type::Nat),
                    abs(
                        "y",
                        Type::List(box Type::Nat),
                        Term::If(
                            box Term::IsNil(box var("y")),
                            box var("x"),
                            box app(
                                app(
                                    var("f"),
                                    Term::Cons(box Term::Head(box var("y")), box var("x")),
                                ),
                                Term::Tail(box var("y")),
                            ),
                        ),
                    ),
                ),
            )),
            Term::Nil(Type::Nat),
        )
    }
}
