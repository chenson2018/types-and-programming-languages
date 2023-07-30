use crate::term::{abs, app, var, Term};

lazy_static! {
    pub static ref ZERO: Term = abs("s", abs("z", var("z")));
    pub static ref TRU: Term = abs("s", abs("z", var("s")));
    pub static ref TEST: Term = abs(
        "l",
        abs("m", abs("n", app(app(var("l"), var("m")), var("n"))))
    );
    pub static ref AND: Term = abs("a", abs("b", app(app(var("a"), var("b")), var("a"))));
    pub static ref OR: Term = abs("a", abs("b", app(app(var("a"), var("a")), var("b"))));
//    pub static ref NOT: Term = abs("bool", app(app(var("bool"), ZERO.clone()), TRU.clone()) );
    pub static ref NOT: Term = abs("bool", abs("a", abs("b", app(app(var("bool"), var("b")), var("a")) )));
//    pub static ref NOT: Term = abs(
//        "bool",
//        app(
//            app(app(TEST.clone(), var("bool")), ZERO.clone()),
//            TRU.clone()
//        )
//    );
    pub static ref SUCC: Term = abs(
        "n",
        abs(
            "s",
            abs(
                "z",
                app(var("s"), app(app(var("n"), var("s")), var("z")))
            )
        )
    );
    pub static ref PLUS: Term = abs("m", abs("n", abs("s", abs("z", app( app(var("m"), var("s")), app(app(var("n"), var("s")), var("z")) )))));
    // TODO this fails because of free variables
    // maybe convert everything to usize at the beginning and keep a map??
    pub static ref TIMES: Term = abs("m", abs("n", app(app(var("m"), app(PLUS.clone(), var("n"))), ZERO.clone())));
}

impl From<usize> for Term {
    fn from(value: usize) -> Term {
        let mut term = ZERO.clone();

        for _ in 0..value {
            term = app(SUCC.clone(), term);
        }

        term.full()
    }
}
