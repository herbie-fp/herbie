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
