mod test {
    use simply_typed::term::Term;
    use simply_typed::types::Type;

    fn expect_err(input: &str, expected_err: &str) {
        let term: Term = format!("{}\n", input).parse().unwrap();
        let err = term.dtype().expect_err("Type:");
        assert_eq!(err, expected_err);
    }

    fn expect_type(input: &str, expected_type: Type) {
        let term: Term = format!("{}\n", input).parse().unwrap();
        let dtype = term.dtype().unwrap();
        assert_eq!(dtype, expected_type);
    }

    #[test]
    fn bool_literal() {
        expect_type("true", Type::Bool);
        expect_type("false", Type::Bool);
    }

    #[test]
    fn conditional() {
        expect_type("if true then true else false", Type::Bool);
        expect_type("if true then 1 else 0", Type::Nat);
    }

    #[test]
    fn nat_literal() {
        expect_type("0", Type::Nat);
        expect_type("1", Type::Nat);
    }

    #[test]
    fn succ_pred() {
        expect_type("succ 0", Type::Nat);
        expect_type("pred 0", Type::Nat);
    }

    #[test]
    fn iszero() {
        expect_type("iszero 0", Type::Bool);
    }

    #[test]
    fn list() {
        expect_type("list[Nat] [1, 2, 3]", Type::List(Box::new(Type::Nat)));
        expect_type("isnil[Nat] (list[Nat] [1, 2, 3])", Type::Bool);
        expect_type("head[Nat] (list[Nat] [1, 2, 3])", Type::Nat);
        expect_type(
            "tail[Nat] (list[Nat] [1, 2, 3])",
            Type::List(Box::new(Type::Nat)),
        );
    }

    #[test]
    fn condition_non_bool() {
        expect_err("if 1 then true else false", "non-boolean condition");
    }

    #[test]
    fn diverge() {
        expect_err("if true then 1 else false", "diverging conditional types");
    }

    #[test]
    fn pred_succ_non_nat() {
        expect_err("succ true", "non-nat pred/succ");
        expect_err("pred true", "non-nat pred/succ");
    }

    #[test]
    fn iszero_non_nat() {
        expect_err("iszero true", "non-nat iszero")
    }

    #[test]
    fn inconsistent_list_typing() {
        expect_err("list[Nat] [1, true]", "inconsistent list typing");
        expect_err(
            "isnil[Bool] (list[Nat] [1, 2, 3])",
            "inconsistent list typing",
        );
        expect_err(
            "head[Bool] (list[Nat] [1, 2, 3])",
            "inconsistent list typing",
        );
        expect_err(
            "tail[Bool] (list[Nat] [1, 2, 3])",
            "inconsistent list typing",
        );
    }
}
