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
    // TODO this "works", but makes netsed builtins like "times" prone to stack overflows
    pub fn expand_builtin(self) -> Term {
        match self {
            Self::Var(_) => {
                if let Some(builtin) = BUILTINS.get(&self) {
                    builtin.clone()
                } else {
                    self
                }
            }
            Self::If(t1, t2, t3) => Self::If(
                box t1.expand_builtin(),
                box t2.expand_builtin(),
                box t3.expand_builtin(),
            ),
            Self::Abs(x, dtype, t1) => Self::Abs(x, dtype, box t1.expand_builtin()),
            Self::App(t1, t2) => Self::App(box t1.expand_builtin(), box t2.expand_builtin()),
            Self::Succ(t1) => Self::Succ(box t1.expand_builtin()),
            Self::Pred(t1) => Self::Pred(box t1.expand_builtin()),
            Self::IsZero(t1) => Self::IsZero(box t1.expand_builtin()),
            Self::Fix(t1) => Self::Fix(box t1.expand_builtin()),
            Self::Zero | Self::True | Self::False => self,
        }
    }

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
                            app(var("or"), Term::IsZero(box var("n"))),
                            Term::IsZero(box var("m")),
                        ),
                        box Term::Zero,
                        box Term::If(
                            box Term::IsZero(box Term::Pred(box var("n"))),
                            box var("m"),
                            box app(
                                app(var("plus"), var("n")),
                                app(app(var("f"), Term::Pred(box var("m"))), var("m")),
                            ),
                        ),
                    ),
                ),
            ),
        ))
    }
}
