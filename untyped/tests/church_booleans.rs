mod test {
    use untyped::term::{app, appa, var};
    use untyped::term::{Term, TermAnon};

    #[test]
    fn not() {
        let term = app(Term::not(), Term::tru());
        assert_eq!(term.full(), Term::zero().to_anon());

        let term = app(Term::not(), Term::zero());
        assert_eq!(term.full(), Term::tru().to_anon());
    }

    #[test]
    fn or() {
        let term = app(app(Term::or(), Term::zero()), Term::zero());
        assert_eq!(term.full(), Term::zero().to_anon());

        let term = app(app(Term::or(), Term::tru()), Term::zero());
        assert_eq!(term.full(), Term::tru().to_anon());

        let term = app(app(Term::or(), Term::zero()), Term::tru());
        assert_eq!(term.full(), Term::tru().to_anon());

        let term = app(app(Term::or(), Term::tru()), Term::tru());
        assert_eq!(term.full(), Term::tru().to_anon());
    }

    #[test]
    fn and() {
        let term = app(app(Term::and(), Term::zero()), Term::zero());
        assert_eq!(term.full(), Term::zero().to_anon());

        let term = app(app(Term::and(), Term::tru()), Term::zero());
        assert_eq!(term.full(), Term::zero().to_anon());

        let term = app(app(Term::and(), Term::zero()), Term::tru());
        assert_eq!(term.full(), Term::zero().to_anon());

        let term = app(app(Term::and(), Term::tru()), Term::tru());
        assert_eq!(term.full(), Term::tru().to_anon());
    }

    #[test]
    fn test() {
        let term = app(app(app(Term::test(), Term::tru()), var("a")), var("b"));
        let (anon, ctx) = term.to_anon_and_ctx();
        let expected = var("a").remove_names(&ctx);
        assert_eq!(anon.full(), expected);

        let term = app(app(app(Term::test(), Term::zero()), var("a")), var("b"));
        let (anon, ctx) = term.to_anon_and_ctx();
        let expected = var("b").remove_names(&ctx);
        assert_eq!(anon.full(), expected);
    }

    #[test]
    fn plus() {
        let term = appa(appa(Term::plus().to_anon(), 1.into()), 2.into());
        assert_eq!(term.full(), <usize as Into<TermAnon>>::into(3_usize));
    }
}
