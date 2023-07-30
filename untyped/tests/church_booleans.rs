mod test {
    use untyped::church::{AND, NOT, OR, PLUS, TEST, TRU, ZERO};
    use untyped::term::Term;
    use untyped::term::{app, var};

    #[test]
    fn not() {
        let term = app(NOT.clone(), TRU.clone());
        assert_eq!(term.full().to_anon(), ZERO.to_anon());

        let term = app(NOT.clone(), ZERO.clone());
        assert_eq!(term.full().to_anon(), TRU.to_anon());
    }

    #[test]
    fn or() {
        let term = app(app(OR.clone(), ZERO.clone()), ZERO.clone());
        assert_eq!(term.full().to_anon(), ZERO.to_anon());

        let term = app(app(OR.clone(), TRU.clone()), ZERO.clone());
        assert_eq!(term.full().to_anon(), TRU.to_anon());

        let term = app(app(OR.clone(), ZERO.clone()), TRU.clone());
        assert_eq!(term.full().to_anon(), TRU.to_anon());

        let term = app(app(OR.clone(), TRU.clone()), TRU.clone());
        assert_eq!(term.full().to_anon(), TRU.to_anon());
    }

    #[test]
    fn and() {
        let term = app(app(AND.clone(), ZERO.clone()), ZERO.clone());
        assert_eq!(term.full().to_anon(), ZERO.to_anon());

        let term = app(app(AND.clone(), TRU.clone()), ZERO.clone());
        assert_eq!(term.full().to_anon(), ZERO.to_anon());

        let term = app(app(AND.clone(), ZERO.clone()), TRU.clone());
        assert_eq!(term.full().to_anon(), ZERO.to_anon());

        let term = app(app(AND.clone(), TRU.clone()), TRU.clone());
        assert_eq!(term.full().to_anon(), TRU.to_anon());
    }

    #[test]
    fn test() {
        let term = app(app(app(TEST.clone(), TRU.clone()), var("a")), var("b"));
        assert_eq!(term.full(), var("a"));

        let term = app(app(app(TEST.clone(), ZERO.clone()), var("a")), var("b"));
        assert_eq!(term.full(), var("b"));
    }

    #[test]
    fn plus() {
        let term = app(app(PLUS.clone(), 1.into()), 2.into());
        assert_eq!(
            term.full().to_anon(),
            <usize as Into<Term>>::into(3_usize).to_anon()
        );
    }
}
