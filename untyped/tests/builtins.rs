mod test {
    use untyped::term::Term;
    use untyped::term::{app, var};

    fn expect_lambda(input: &str, reduced: &str) {
        let term: Term = input.parse().unwrap();
        let expected: Term = reduced.parse().unwrap();
        assert_eq!(term.full(), expected.full());
    }

    #[test]
    fn not() {
        expect_lambda("(not tru)", "fls");
        expect_lambda("(not fls)", "tru");
    }

    #[test]
    fn or() {
        expect_lambda("(or tru tru)", "tru");
        expect_lambda("(or fls tru)", "tru");
        expect_lambda("(or tru fls)", "tru");
        expect_lambda("(or fls fls)", "fls");
    }

    #[test]
    fn and() {
        expect_lambda("(and tru tru)", "tru");
        expect_lambda("(and fls tru)", "fls");
        expect_lambda("(and tru fls)", "fls");
        expect_lambda("(and fls fls)", "fls");
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
        expect_lambda("(plus 1 2)", "(3)");
    }

    #[test]
    fn times() {
        expect_lambda("(times 2 3)", "(6)");
    }

    #[test]
    fn pow() {
        expect_lambda("(pow 2 3)", "(9)");
    }
}
