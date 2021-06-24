use crate::math::*;

use indexmap::IndexMap;
use std::str::FromStr;

pub fn mk_rules(tuples: &[(&str, &str, &str)]) -> Vec<Rewrite> {
    tuples
        .iter()
        .map(|(name, left, right)| {
            let left = Pattern::from_str(left).unwrap();
            let right = Pattern::from_str(right).unwrap();
            Rewrite::new(*name, left, right).unwrap()
        })
        .collect()
}
