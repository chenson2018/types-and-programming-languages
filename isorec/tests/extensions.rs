mod test {
    use isorec::term::Term;

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
    fn isnil() {
        expect_lambda("isnil nil", "true");
        expect_lambda("isnil (cons 1 (cons 2 (cons 3 nil)))", "false");
    }

    #[test]
    fn head() {
        expect_lambda("hd (cons 1 (cons 2 (cons 3 nil)))", "1");
    }

    #[test]
    fn tail() {
        expect_lambda("tl (cons 1 (cons 2 (cons 3 nil)))", "(cons 2 (cons 3 nil))");
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

    #[test]
    fn record() {
        expect_lambda("{0, 1}.0", "0");
        expect_lambda("{0, 1}.1", "1");
        expect_lambda("{a=0, b=1}.a", "0");
        expect_lambda("{a=0, b=1}.b", "1");

        // works for nested records as well
        expect_lambda(r"(\x:{Nat, {Nat, Nat}} . x.0) {0, {1, 2}}", "0");
        expect_lambda(r"(\x:{Nat, {Nat, Nat}} . x.1.0) {0, {1, 2}}", "1");
        expect_lambda(r"(\x:{Nat, {Nat, Nat}} . x.1.1) {0, {1, 2}}", "2");

        // can be mixed record/tuple
        expect_lambda(r"(\x:{a:Nat, {b:Nat, c:Nat}} . x.a) {a=0, {b=1, c=2}}", "0");
        expect_lambda(
            r"(\x:{a:Nat, {b:Nat, c:Nat}} . x.1.b) {a=0, {b=1, c=2}}",
            "1",
        );
        expect_lambda(
            r"(\x:{a:Nat, {b:Nat, c:Nat}} . x.1.c) {a=0, {b=1, c=2}}",
            "2",
        );
    }
}
