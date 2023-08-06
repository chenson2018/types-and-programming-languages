use crate::term::*;
use crate::types::{con, Type};
use std::collections::HashMap;

const ANON: (usize, usize) = (0, 0);

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
        abs_no_loc(
            "a",
            Type::Bool,
            Term::If(box var_no_loc("a"), box Term::False, box Term::True, ANON),
        )
    }

    pub fn and() -> Self {
        abs_no_loc(
            "a",
            Type::Bool,
            abs_no_loc(
                "b",
                Type::Bool,
                Term::If(
                    box var_no_loc("a"),
                    box var_no_loc("b"),
                    box Term::False,
                    ANON,
                ),
            ),
        )
    }

    pub fn or() -> Self {
        abs_no_loc(
            "a",
            Type::Bool,
            abs_no_loc(
                "b",
                Type::Bool,
                Term::If(
                    box var_no_loc("a"),
                    box Term::True,
                    box var_no_loc("b"),
                    ANON,
                ),
            ),
        )
    }

    pub fn plus() -> Self {
        Term::Fix(
            box abs_no_loc(
                "f",
                con(&Type::Nat, &con(&Type::Nat, &Type::Nat)),
                abs_no_loc(
                    "n",
                    Type::Nat,
                    abs_no_loc(
                        "m",
                        Type::Nat,
                        Term::If(
                            box Term::IsZero(box var_no_loc("n"), ANON),
                            box var_no_loc("m"),
                            box app_no_loc(
                                app_no_loc(var_no_loc("f"), Term::Pred(box var_no_loc("n"), ANON)),
                                Term::Succ(box var_no_loc("m"), ANON),
                            ),
                            ANON,
                        ),
                    ),
                ),
            ),
            ANON,
        )
    }

    pub fn times() -> Self {
        Term::Fix(
            box abs_no_loc(
                "f",
                con(&Type::Nat, &con(&Type::Nat, &Type::Nat)),
                abs_no_loc(
                    "n",
                    Type::Nat,
                    abs_no_loc(
                        "m",
                        Type::Nat,
                        Term::If(
                            box app_no_loc(
                                app_no_loc(Term::or(), Term::IsZero(box var_no_loc("n"), ANON)),
                                Term::IsZero(box var_no_loc("m"), ANON),
                            ),
                            box Term::Zero,
                            box Term::If(
                                box Term::IsZero(box Term::Pred(box var_no_loc("n"), ANON), ANON),
                                box var_no_loc("m"),
                                box app_no_loc(
                                    app_no_loc(Term::plus(), var_no_loc("m")),
                                    app_no_loc(
                                        app_no_loc(
                                            var_no_loc("f"),
                                            Term::Pred(box var_no_loc("n"), ANON),
                                        ),
                                        var_no_loc("m"),
                                    ),
                                ),
                                ANON,
                            ),
                            ANON,
                        ),
                    ),
                ),
            ),
            ANON,
        )
    }

    pub fn pow() -> Self {
        Term::Fix(
            box abs_no_loc(
                "f",
                con(&Type::Nat, &con(&Type::Nat, &Type::Nat)),
                abs_no_loc(
                    "n",
                    Type::Nat,
                    abs_no_loc(
                        "m",
                        Type::Nat,
                        Term::If(
                            box Term::IsZero(box var_no_loc("n"), ANON),
                            box Term::Succ(box Term::Zero, ANON),
                            box Term::If(
                                box Term::IsZero(box Term::Pred(box var_no_loc("n"), ANON), ANON),
                                box var_no_loc("m"),
                                box app_no_loc(
                                    app_no_loc(Term::times(), var_no_loc("m")),
                                    app_no_loc(
                                        app_no_loc(
                                            var_no_loc("f"),
                                            Term::Pred(box var_no_loc("n"), ANON),
                                        ),
                                        var_no_loc("m"),
                                    ),
                                ),
                                ANON,
                            ),
                            ANON,
                        ),
                    ),
                ),
            ),
            ANON,
        )
    }

    pub fn fact() -> Self {
        Term::Fix(
            box abs_no_loc(
                "f",
                con(&Type::Nat, &Type::Nat),
                abs_no_loc(
                    "n",
                    Type::Nat,
                    Term::If(
                        box app_no_loc(
                            app_no_loc(Term::or(), Term::IsZero(box var_no_loc("n"), ANON)),
                            Term::IsZero(box Term::Pred(box var_no_loc("n"), ANON), ANON),
                        ),
                        box Term::Succ(box Term::Zero, ANON),
                        box app_no_loc(
                            app_no_loc(Term::times(), var_no_loc("n")),
                            app_no_loc(var_no_loc("f"), Term::Pred(box var_no_loc("n"), ANON)),
                        ),
                        ANON,
                    ),
                ),
            ),
            ANON,
        )
    }

    pub fn rev_nat() -> Self {
        app_no_loc(
            Term::Fix(
                box abs_no_loc(
                    "f",
                    con(
                        &Type::List(box Type::Nat),
                        &con(&Type::List(box Type::Nat), &Type::List(box Type::Nat)),
                    ),
                    abs_no_loc(
                        "x",
                        Type::List(box Type::Nat),
                        abs_no_loc(
                            "y",
                            Type::List(box Type::Nat),
                            Term::If(
                                box Term::IsNil(box var_no_loc("y"), ANON),
                                box var_no_loc("x"),
                                box app_no_loc(
                                    app_no_loc(
                                        var_no_loc("f"),
                                        Term::Cons(
                                            box Term::Head(box var_no_loc("y"), ANON),
                                            box var_no_loc("x"),
                                            ANON,
                                        ),
                                    ),
                                    Term::Tail(box var_no_loc("y"), ANON),
                                ),
                                ANON,
                            ),
                        ),
                    ),
                ),
                ANON,
            ),
            Term::Nil(Type::Nat, ANON),
        )
    }
}
