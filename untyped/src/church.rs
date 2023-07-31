use crate::term::*;

impl Term {
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
}

impl From<usize> for TermAnon {
    fn from(value: usize) -> TermAnon {
        let mut term = Term::zero().to_anon();

        for _ in 0..value {
            term = appa(Term::succ().to_anon(), term);
        }

        term.full()
    }
}
