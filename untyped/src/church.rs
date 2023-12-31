use crate::term::*;
use std::collections::HashMap;

impl Term {
    pub fn env() -> HashMap<Term, Term> {
        HashMap::from([
            (var("zero"), Term::zero()),
            (var("fls"), Term::zero()),
            (var("tru"), Term::tru()),
            (var("test"), Term::test()),
            (var("and"), Term::and()),
            (var("or"), Term::or()),
            (var("not"), Term::not()),
            (var("succ"), Term::succ()),
            (var("plus"), Term::plus()),
            (var("times"), Term::times()),
            (var("pow"), Term::pow()),
        ])
    }

    pub fn zero() -> Self {
        abs("s", abs("z", var("z")))
    }

    pub fn tru() -> Self {
        abs("s", abs("z", var("s")))
    }

    pub fn test() -> Self {
        abs(
            "l",
            abs("m", abs("n", app(app(var("l"), var("m")), var("n")))),
        )
    }

    pub fn and() -> Self {
        abs("a", abs("b", app(app(var("a"), var("b")), var("a"))))
    }

    pub fn or() -> Self {
        abs("a", abs("b", app(app(var("a"), var("a")), var("b"))))
    }

    pub fn not() -> Self {
        abs(
            "bool",
            abs("a", abs("b", app(app(var("bool"), var("b")), var("a")))),
        )
    }

    pub fn succ() -> Self {
        abs(
            "n",
            abs(
                "s",
                abs("z", app(var("s"), app(app(var("n"), var("s")), var("z")))),
            ),
        )
    }

    pub fn plus() -> Self {
        abs(
            "m",
            abs(
                "n",
                abs(
                    "s",
                    abs(
                        "z",
                        app(
                            app(var("m"), var("s")),
                            app(app(var("n"), var("s")), var("z")),
                        ),
                    ),
                ),
            ),
        )
    }

    pub fn times() -> Self {
        abs(
            "m",
            abs(
                "n",
                app(app(var("m"), app(Term::plus(), var("n"))), Term::zero()),
            ),
        )
    }

    pub fn pow() -> Self {
        abs(
            "m",
            abs(
                "n",
                app(
                    app(var("m"), app(Term::times(), var("n"))),
                    app(Term::succ(), Term::zero()),
                ),
            ),
        )
    }
}

impl From<usize> for Term {
    fn from(value: usize) -> Term {
        let mut term = Term::zero();

        for _ in 0..value {
            term = app(Term::succ(), term);
        }

        term
    }
}

impl From<usize> for TermAnon {
    fn from(value: usize) -> TermAnon {
        let mut term = Term::zero().to_anon();

        for _ in 0..value {
            term = appa(Term::succ().to_anon(), term);
        }

        term
    }
}
