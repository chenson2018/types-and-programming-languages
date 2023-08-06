mod test {
    use simply_typed::term::Term;

    fn expect_lambda(input: &str, reduced: &str) {
        let term: Term = format!("{}\n", input).parse().unwrap();
        let expected: Term = format!("{}\n", reduced).parse().unwrap();
        assert_eq!(term.full(), expected.full());
    }

    #[test]
    fn let_binding() {
        expect_lambda("let x = 1; x", "1");
        expect_lambda("let x = 1; let y = succ x; y", "2");
    }

    #[test]
    fn list_sugar() {
        expect_lambda("list[Nat] [1, 2, 3]", "cons 1 (cons 2 (cons 3 nil[Nat]))")
    }

    #[test]
    fn isnil() {
        expect_lambda("isnil (list[Nat] [])", "true");
        expect_lambda("isnil (list[Nat] [1])", "false");
    }

    #[test]
    fn head() {
        expect_lambda("head (list[Nat] [1, 2, 3])", "1");
    }

    #[test]
    fn tail() {
        expect_lambda("tail (list[Nat] [1, 2, 3])", "list[Nat] [2, 3]");
    }

    #[test]
    fn variant() {
        expect_lambda(
            "case (<a=0> as <a:Nat, b:Bool>) of <a=x> → x | <b=b> → if b then 1 else 0",
            "0",
        );
        expect_lambda(
            "case (<a=1> as <a:Nat, b:Bool>) of <a=x> → x | <b=b> → if b then 1 else 0",
            "1",
        );
        expect_lambda(
            "case (<b=true> as <a:Nat, b:Bool>) of <a=x> → x | <b=b> → if b then 1 else 0",
            "1",
        );
        expect_lambda(
            "case (<b=false> as <a:Nat, b:Bool>) of <a=x> → x | <b=b> → if b then 1 else 0",
            "0",
        );
    }
}
