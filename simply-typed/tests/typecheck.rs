mod test {
    use simply_typed::term::Term;
    use simply_typed::types::{con, Type};

    fn expect_err(input: &str, expected_err: &str) {
        let term: Term = format!("{}\n", input).parse().unwrap();
        let err = term.dtype().expect_err("Type:");
        assert_eq!(err.label, expected_err);
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
        expect_type("isnil (list[Nat] [1, 2, 3])", Type::Bool);
        expect_type("head (list[Nat] [1, 2, 3])", Type::Nat);
        expect_type(
            "tail (list[Nat] [1, 2, 3])",
            Type::List(Box::new(Type::Nat)),
        );
    }

    #[test]
    fn tag() {
        expect_type(
            "<a=1> as <a:Nat, b:Bool>",
            Type::Variant(vec![("a".into(), Type::Nat), ("b".into(), Type::Bool)]),
        )
    }

    #[test]
    fn variant() {
        expect_type(
            "case (<a=0> as <a:Nat, b:Bool>) of <a=x> → x | <b=b> → if b then 1 else 0",
            Type::Nat,
        );
        // note how x is shadowed here
        expect_type(
            r"\x:<a:Nat, b:Bool> . case x of <a=x> → x | <b=b> → if b then 1 else 0",
            con(
                &Type::Variant(vec![("a".into(), Type::Nat), ("b".into(), Type::Bool)]),
                &Type::Nat,
            ),
        );
    }

    #[test]
    fn record() {
        expect_type("{0, 1}.0", Type::Nat);
        expect_type("{0, 1}.1", Type::Nat);
        expect_type("{a=0, b=1}.a", Type::Nat);
        expect_type("{a=0, b=1}.b", Type::Nat);

        // works for nested records as well
        expect_type(r"(\x:{Nat, {Nat, Nat}} . x.0) {0, {1, 2}}", Type::Nat);
        expect_type(r"(\x:{Nat, {Nat, Nat}} . x.1.0) {0, {1, 2}}", Type::Nat);
        expect_type(r"(\x:{Nat, {Nat, Nat}} . x.1.1) {0, {1, 2}}", Type::Nat);

        // can be mixed record/tuple
        expect_type(
            r"(\x:{a:Nat, {b:Nat, c:Nat}} . x.a) {a=0, {b=1, c=2}}",
            Type::Nat,
        );
        expect_type(
            r"(\x:{a:Nat, {b:Nat, c:Nat}} . x.1.b) {a=0, {b=1, c=2}}",
            Type::Nat,
        );
        expect_type(
            r"(\x:{a:Nat, {b:Nat, c:Nat}} . x.1.c) {a=0, {b=1, c=2}}",
            Type::Nat,
        );

        expect_type(
            "{1, 2}",
            Type::Record(vec![("0".into(), Type::Nat), ("1".into(), Type::Nat)]),
        );
        expect_type(
            "{a=1, b=2}",
            Type::Record(vec![("a".into(), Type::Nat), ("b".into(), Type::Nat)]),
        );
    }

    #[test]
    fn type_alias() {
        expect_type(r"b = Nat; (\x:b . x) 1", Type::Nat);
        expect_type(
            r"
            OptionNat = <none:Unit, some:Nat>;
            let unwrap_or_zero = \x:OptionNat . case x of <none=a> → 0 | <some=a> → a;
            unwrap_or_zero <none=unit> as OptionNat",
            Type::Nat,
        );
        expect_type(
            r"
            a = Nat;
            b = Unit;
            OptionNat = <none:b, some:a>;
            let unwrap_or_zero = \x:OptionNat . case x of <none=a> → 0 | <some=a> → a;
            unwrap_or_zero <none=unit> as OptionNat",
            Type::Nat,
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
    }

    #[test]
    fn unique_variant_names() {
        expect_err(
            "<a=1> as <a:Nat, a:Nat>",
            "variant type with non-unique names",
        );
    }

    #[test]
    fn incompatible_variant_type() {
        expect_err(
            "<a=true> as <a:Nat, b:Bool>",
            "tag variant incompatible with variant type",
        );
        expect_err(
            "<b=1> as <a:Nat, b:Bool>",
            "tag variant incompatible with variant type",
        );
        expect_err(
            "<c=true> as <a:Nat, b:Bool>",
            "tag variant incompatible with variant type",
        );
    }

    #[test]
    fn non_tag_case() {
        expect_err("case 1 of <a=x> → x", "non-tag case");
    }

    #[test]
    fn inconsistent_case_types() {
        expect_err(
            "case (<a=0> as <a:Nat, b:Bool>) of <a=x> → x | <b=b> → b",
            "inconsistent case types",
        );
    }

    #[test]
    fn mismatched_case_variant() {
        // missing a case
        expect_err(
            "case (<a=0> as <a:Nat, b:Bool>) of <a=x> → x",
            "mismatched case variant",
        );

        // wrong order
        expect_err(
            "case (<a=0> as <a:Nat, b:Bool>) of <b=b> → 0 | <a=a> → 1",
            "mismatched case variant",
        );
    }

    #[test]
    fn invalid_accessor() {
        expect_err("{a = 1, b = 2}.c", "invalid record accessor");
    }

    #[test]
    fn non_record_proj() {
        expect_err("true.0", "non-record proj");
    }
}
