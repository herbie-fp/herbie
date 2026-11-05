use egg::*;
use std::sync::atomic::{AtomicBool, Ordering};

use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::Ratio;
use num_traits::{One, Pow, Signed, Zero};
use std::collections::HashMap;
use std::str::FromStr;

pub type Constant = num_rational::BigRational;
pub type RecExpr = egg::RecExpr<Math>;
pub type Pattern = egg::Pattern<Math>;
pub type EGraph = egg::EGraph<Math, ConstantFold>;
pub type Rewrite = egg::Rewrite<Math, ConstantFold>;
pub type Runner = egg::Runner<Math, ConstantFold, IterData>;
pub type Iteration = egg::Iteration<IterData>;

pub struct IterData {
    pub extracted: Vec<(Id, Extracted)>,
}

pub struct Extracted {
    pub best: RecExpr,
    pub cost: usize,
}

#[derive(Clone, Copy)]
enum Aggregation {
    Sum,
    If,
}

#[derive(Clone, Copy)]
struct OperatorCost {
    base: usize,
    aggregation: Aggregation,
}

#[derive(Clone, Default)]
pub struct ExtractionCosts {
    operators: HashMap<String, OperatorCost>,
    representations: HashMap<String, usize>,
    pow_exponents: HashMap<String, usize>,
}

impl ExtractionCosts {
    pub fn parse(serialized: &str) -> Self {
        let mut costs = Self::default();
        for line in serialized.lines() {
            let fields = line.split('\t').collect::<Vec<_>>();
            match fields.as_slice() {
                ["repr", repr, cost] => {
                    costs
                        .representations
                        .insert((*repr).to_string(), cost.parse().unwrap());
                }
                ["impl", op, "sum", cost] => {
                    costs.operators.insert(
                        (*op).to_string(),
                        OperatorCost {
                            base: cost.parse().unwrap(),
                            aggregation: Aggregation::Sum,
                        },
                    );
                }
                ["impl", op, "if", base] => {
                    costs.operators.insert(
                        (*op).to_string(),
                        OperatorCost {
                            base: base.parse().unwrap(),
                            aggregation: Aggregation::If,
                        },
                    );
                }
                ["pow", op, exponent] => {
                    costs
                        .pow_exponents
                        .insert((*op).to_string(), exponent.parse().unwrap());
                }
                _ => unreachable!("invalid serialized platform extraction cost"),
            }
        }
        costs
    }
}

// Parsed costs are cloned with Context; this short-lived view also borrows the current graph
// to inspect constant-folded exponents during extraction.
pub struct PlatformCost<'a> {
    egraph: &'a EGraph,
    costs: &'a ExtractionCosts,
}

impl<'a> PlatformCost<'a> {
    pub fn new(egraph: &'a EGraph, costs: &'a ExtractionCosts) -> Self {
        Self { egraph, costs }
    }
}

impl<'a> CostFunction<Math> for PlatformCost<'a> {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &Math, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if let Math::Other(op, children) = enode {
            let op = op.as_str();
            if op.starts_with("$do-lower.") {
                return usize::MAX;
            }
            if let Some(repr) = op.strip_prefix("$do-lower-constant.") {
                return self.costs.representations[repr];
            }
            if op.starts_with("$do-lower-variable.") {
                return 0;
            }
            if op == "$approx" {
                return costs(children[1]);
            }
            if let Some(operator_cost) = self.costs.operators.get(op) {
                if let Some(exponent) = self.costs.pow_exponents.get(op) {
                    if let Some((n, _reason)) = &self.egraph[children[*exponent]].data {
                        if !n.denom().is_one() && n.denom().is_odd() {
                            return usize::MAX;
                        }
                    }
                }
                return match operator_cost.aggregation {
                    Aggregation::Sum => children
                        .iter()
                        .fold(operator_cost.base, |sum, id| sum.saturating_add(costs(*id))),
                    Aggregation::If => {
                        let mut child_costs = children.iter().map(|id| costs(*id));
                        operator_cost
                            .base
                            .saturating_add(child_costs.next().unwrap())
                            .saturating_add(
                                child_costs.next().unwrap().max(child_costs.next().unwrap()),
                            )
                    }
                };
            }
        }

        if let Math::Pow([_, i]) = enode {
            if let Some((n, _reason)) = &self.egraph[*i].data {
                if !n.denom().is_one() && n.denom().is_odd() {
                    return usize::MAX;
                }
            }
        }

        enode.fold(1, |sum, id| usize::saturating_add(sum, costs(id)))
    }
}

#[cfg(test)]
mod platform_cost_tests {
    use super::*;

    #[test]
    fn uses_platform_costs_for_extraction() {
        let mut egraph = EGraph::new(ConstantFold::default());
        let x = egraph.add(Math::Symbol("x".into()));
        let y = egraph.add(Math::Symbol("y".into()));
        let sin = egraph.add(Math::Other("sin.f64".into(), vec![x]));
        let inner_add = egraph.add(Math::Other("+.f64".into(), vec![x, y]));
        let nested_add = egraph.add(Math::Other("+.f64".into(), vec![inner_add, y]));
        egraph.union(sin, nested_add);

        let p = egraph.add(Math::Symbol("p".into()));
        let q = egraph.add(Math::Symbol("q".into()));
        let half = egraph.add(Math::Constant(Ratio::new(BigInt::from(1), BigInt::from(2))));
        let third = egraph.add(Math::Constant(Ratio::new(BigInt::from(1), BigInt::from(3))));
        let pow_half = egraph.add(Math::Other("pow.f64".into(), vec![p, half]));
        let pow_third = egraph.add(Math::Other("pow.f64".into(), vec![q, third]));
        let expanded = egraph.add(Math::Other("*.f64".into(), vec![pow_half, pow_third]));
        let sum = egraph.add(Math::Other("+.f64".into(), vec![p, q]));
        egraph.union(expanded, sum);
        egraph.rebuild();

        let costs = ExtractionCosts::parse(
            "impl\tsin.f64\tsum\t4200\n\
             impl\t+.f64\tsum\t200\n\
             impl\tpow.f64\tsum\t20\n\
             impl\t*.f64\tsum\t1\n\
             pow\tpow.f64\t1",
        );
        let platform_extractor = Extractor::new(&egraph, PlatformCost::new(&egraph, &costs));
        let (_, platform_best) = platform_extractor.find_best(egraph.find(sin));
        assert_eq!(
            platform_best
                .as_ref()
                .iter()
                .filter(|node| matches!(node, Math::Other(op, _) if op.as_str() == "+.f64"))
                .count(),
            2
        );

        let (_, sum_best) =
            Extractor::new(&egraph, PlatformCost::new(&egraph, &costs)).find_best(egraph.find(sum));
        assert!(sum_best
            .as_ref()
            .iter()
            .any(|node| matches!(node, Math::Other(op, _) if op.as_str() == "+.f64")));
        assert!(!sum_best
            .as_ref()
            .iter()
            .any(|node| matches!(node, Math::Other(op, _) if op.as_str() == "pow.f64")));

        let size_extractor = Extractor::new(&egraph, AstSize);
        let (_, size_best) = size_extractor.find_best(egraph.find(sin));
        assert!(size_best
            .as_ref()
            .iter()
            .any(|node| matches!(node, Math::Other(op, _) if op.as_str() == "sin.f64")));
    }
}

impl IterationData<Math, ConstantFold> for IterData {
    fn make(runner: &Runner) -> Self {
        let extractor = Extractor::new(&runner.egraph, AstSize);
        let extracted = runner
            .roots
            .iter()
            .map(|&root| {
                let (cost, best) = extractor.find_best(root);
                let ext = Extracted { cost, best };
                (root, ext)
            })
            .collect();
        Self { extracted }
    }
}

// operators from FPCore
define_language! {
    pub enum Math {

        // constant-folding operators

        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "pow" = Pow([Id; 2]),
        "neg" = Neg([Id; 1]),
        "sqrt" = Sqrt([Id; 1]),
        "fabs" = Fabs([Id; 1]),
        "ceil" = Ceil([Id; 1]),
        "floor" = Floor([Id; 1]),
        "round" = Round([Id; 1]),
        "log" = Log([Id; 1]),
        "cbrt" = Cbrt([Id; 1]),

        Constant(Constant),
        Symbol(egg::Symbol),
        Other(egg::Symbol, Vec<Id>),
    }
}

pub struct ConstantFold {
    pub unsound: AtomicBool,
    pub max_abs_exponent: Ratio<BigInt>,
    pub prune: bool,
}

impl Clone for ConstantFold {
    fn clone(&self) -> Self {
        let unsound = AtomicBool::new(self.unsound.load(Ordering::SeqCst));
        Self {
            unsound,
            max_abs_exponent: self.max_abs_exponent.clone(),
            prune: self.prune,
        }
    }
}

impl Default for ConstantFold {
    fn default() -> Self {
        Self {
            unsound: AtomicBool::new(false),
            // Avoid calculating extremely large numbers. 16 is somewhat arbitrary, even 0 passes
            // all tests.
            max_abs_exponent: Ratio::new(BigInt::from(16), BigInt::from(1)),
            prune: true,
        }
    }
}

impl Analysis<Math> for ConstantFold {
    type Data = Option<(Constant, (PatternAst<Math>, Subst))>;
    fn make(egraph: &mut EGraph, enode: &Math, _id: Id) -> Self::Data {
        let x = |id: &Id| egraph[*id].data.clone().map(|x| x.0);
        let is_zero = |id: &Id| {
            let data = egraph[*id].data.as_ref();
            match data {
                Some(data) => data.0.is_zero(),
                None => false,
            }
        };

        Some((
            match enode {
                Math::Constant(c) => c.clone(),

                // real
                Math::Add([a, b]) => x(a)? + x(b)?,
                Math::Sub([a, b]) => x(a)? - x(b)?,
                Math::Mul([a, b]) => x(a)? * x(b)?,
                Math::Div([a, b]) => {
                    if x(b)?.is_zero() {
                        return None;
                    } else {
                        x(a)? / x(b)?
                    }
                }
                Math::Neg([a]) => -x(a)?,
                Math::Pow([a, b]) => {
                    if is_zero(a) {
                        if x(b)?.is_positive() {
                            Ratio::new(BigInt::from(0), BigInt::from(1))
                        } else {
                            return None;
                        }
                    } else if is_zero(b) {
                        Ratio::new(BigInt::from(1), BigInt::from(1))
                    } else if x(b)?.is_integer() && x(b)?.abs() <= egraph.analysis.max_abs_exponent
                    {
                        Pow::pow(x(a)?, x(b)?.to_integer())
                    } else {
                        return None;
                    }
                }
                Math::Sqrt([a]) => {
                    let a = x(a)?;
                    if *a.numer() > BigInt::from(0) && *a.denom() > BigInt::from(0) {
                        let s1 = a.numer().sqrt();
                        let s2 = a.denom().sqrt();
                        let is_perfect = &(&s1 * &s1) == a.numer() && &(&s2 * &s2) == a.denom();
                        if is_perfect {
                            Ratio::new(s1, s2)
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                Math::Log([a]) => {
                    if x(a)? == Ratio::new(BigInt::from(1), BigInt::from(1)) {
                        Ratio::new(BigInt::from(0), BigInt::from(1))
                    } else {
                        return None;
                    }
                }
                Math::Cbrt([a]) => {
                    if x(a)? == Ratio::new(BigInt::from(1), BigInt::from(1)) {
                        Ratio::new(BigInt::from(1), BigInt::from(1))
                    } else {
                        return None;
                    }
                }
                Math::Fabs([a]) => x(a)?.abs(),
                Math::Floor([a]) => x(a)?.floor(),
                Math::Ceil([a]) => x(a)?.ceil(),
                Math::Round([a]) => x(a)?.round(),

                _ => return None,
            },
            {
                let mut pattern: PatternAst<Math> = Default::default();
                let mut var_counter = 0;
                let mut subst: Subst = Default::default();
                enode.for_each(|child| {
                    if let Some(constant) = x(&child) {
                        pattern.add(ENodeOrVar::ENode(Math::Constant(constant)));
                    } else {
                        let var = ("?".to_string() + &var_counter.to_string())
                            .parse()
                            .unwrap();
                        pattern.add(ENodeOrVar::Var(var));
                        subst.insert(var, child);
                        var_counter += 1;
                    }
                });
                let mut counter = 0;
                let mut head = enode.clone();
                head.update_children(|_child| {
                    let res = Id::from(counter);
                    counter += 1;
                    res
                });
                pattern.add(ENodeOrVar::ENode(head));
                (pattern, subst)
            },
        ))
    }

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        match (&to, from) {
            (None, None) => DidMerge(false, false),
            (Some(_), None) => DidMerge(false, true), // no update needed
            (None, Some(c)) => {
                *to = Some(c);
                DidMerge(true, false)
            }
            (Some(a), Some(ref b)) => {
                if a.0 != b.0 && !self.unsound.swap(true, Ordering::SeqCst) {
                    log::warn!("Bad merge detected: {} != {}", a.0, b.0);
                }
                DidMerge(false, false)
            }
        }
    }

    fn modify(egraph: &mut EGraph, class_id: Id) {
        let class = &mut egraph[class_id];
        if let Some((c, (pat, subst))) = class.data.clone() {
            egraph.union_instantiations(
                &pat,
                &format!("{c}").parse().unwrap(),
                &subst,
                "metadata-eval".to_string(),
            );
        }
    }
}

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
