mod test {
    use simply_typed::term::Term;

    fn expect_lambda(input: &str, reduced: &str) {
        let term: Term = format!("{}\n", input).parse().unwrap();
        let expected: Term = format!("{}\n", reduced).parse().unwrap();
        assert_eq!(term.full(), expected.full());
    }

    #[test]
    fn not() {
        expect_lambda("not true", "false");
        expect_lambda("not false", "true");
    }

    #[test]
    fn or() {
        expect_lambda("or true true", "true");
        expect_lambda("or false true", "true");
        expect_lambda("or true false", "true");
        expect_lambda("or false false", "false");
    }

    #[test]
    fn and() {
        expect_lambda("and true true", "true");
        expect_lambda("and false true", "false");
        expect_lambda("and true false", "false");
        expect_lambda("and false false", "false");
    }

    #[test]
    fn iszero() {
        expect_lambda("iszero 0", "true");
        expect_lambda("iszero 1", "false");
    }

    #[test]
    fn pred() {
        expect_lambda("pred 0", "0");
        expect_lambda("pred 1", "0");
    }

    #[test]
    fn plus() {
        expect_lambda("plus 1 2", "3");
    }

    #[test]
    fn times() {
        expect_lambda("times 12 12", "144");
    }

    #[test]
    fn pow() {
        expect_lambda("pow 2 3", "9");
    }
}
