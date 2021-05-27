use egg_math::{math::*, rules::math_rules};

fn rules() -> Vec<Rewrite> {
    math_rules()
        .into_iter()
        .flat_map(|(_group, rewrites)| rewrites)
        .collect()
}

egg::test_fn! {
    math_simplify_root, rules(),
    // runner = Runner::default().with_node_limit(75_000),
    r#"
    (/ f64 1
       (- f64 (/ f64 (+ f64 1 (sqrt f64 five))
             2)
          (/ f64 (- f64 1 (sqrt f64 five))
             2)))"#
    =>
    "(/ f64 1 (sqrt f64 five))"
}

egg::test_fn! {
    math_simplify_neg, rules(),
    "(neg f64 1)" => "-1"
}

egg::test_fn! {
    math_test_simple_pow, rules(),
    "(pow f64 0 2)" => "0",
}

egg::test_fn! {
    math_test_neg_power, rules(),
    "(pow f64 2 -2)" => "1/4",
}

egg::test_fn! {
    math_test_pow_fails, rules(),
    "(pow f64 0 -1)" => "(pow f64 0 -1)"
    @check |r: egg::Runner<Math, ConstantFold>| assert_eq!(r.egraph.equivs(&"(pow f64 0 -1)".parse().unwrap(), &"0".parse().unwrap()), vec![])
}

egg::test_fn! {
    math_fold_log, rules(),
    "(log f64 1)" => "0",
}

egg::test_fn! {
    math_fold_cbrt, rules(),
    "(cbrt f64 1)" => "1",
}
