use egg::*;

use std::sync::atomic::{AtomicBool, Ordering};

use num_bigint::BigInt;
use num_rational::Ratio;
use num_traits::{Pow, Signed, Zero};

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

impl IterationData<Math, ConstantFold> for IterData {
    fn make(runner: &Runner) -> Self {
        let mut extractor = Extractor::new(&runner.egraph, AstSize);
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

        "+" = Add([Id; 3]),
        "-" = Sub([Id; 3]),
        "*" = Mul([Id; 3]),
        "/" = Div([Id; 3]),
        "pow" = Pow([Id; 3]),
        "neg" = Neg([Id; 2]),
        "sqrt" = Sqrt([Id; 2]),
        "fabs" = Fabs([Id; 2]),
        "ceil" = Ceil([Id; 2]),
        "floor" = Floor([Id; 2]),
        "round" = Round([Id; 2]),
        "log" = Log([Id; 2]),
        "cbrt" = Cbrt([Id; 2]),

        Constant(Constant),
        Symbol(egg::Symbol),
        Other(egg::Symbol, Vec<Id>),
    }
}

pub struct ConstantFold {
    pub unsound: AtomicBool,
    pub constant_fold: bool,
    pub prune: bool,
}

impl Default for ConstantFold {
    fn default() -> Self {
        Self {
            constant_fold: true,
            prune: true,
            unsound: AtomicBool::from(false),
        }
    }
}

impl Analysis<Math> for ConstantFold {
    type Data = Option<Constant>;
    fn make(egraph: &EGraph, enode: &Math) -> Self::Data {
        if !egraph.analysis.constant_fold {
            return None;
        }

        let x = |id: &Id| egraph[*id].data.as_ref();
        let is_zero = |id: &Id| {
            let data = egraph[*id].data.as_ref();
            match data {
                Some(data) => data.is_zero(),
                None => false,
            }
        };

        match enode {
            Math::Constant(c) => Some(c.clone()),

            // real
            Math::Add([_p, a, b]) => Some(x(a)? + x(b)?),
            Math::Sub([_p, a, b]) => Some(x(a)? - x(b)?),
            Math::Mul([_p, a, b]) => Some(x(a)? * x(b)?),
            Math::Div([_p, a, b]) => {
                if x(b)?.is_zero() {
                    None
                } else {
                    Some(x(a)? / x(b)?)
                }
            }
            Math::Neg([_p, a]) => Some(-x(a)?.clone()),
            Math::Pow([_p, a, b]) => {
                if is_zero(b) && !is_zero(a) {
                    Some(Ratio::new(BigInt::from(1), BigInt::from(1)))
                } else if is_zero(a) {
                    if x(b)?.is_positive() {
                        Some(Ratio::new(BigInt::from(0), BigInt::from(1)))
                    } else {
                        None
                    }
                } else if x(b)?.is_integer() {
                    Some(Pow::pow(x(a)?, x(b)?.to_integer()))
                } else {
                    None
                }
            }
            Math::Sqrt([_p, a]) => {
                let a = x(a)?;
                if *a.numer() > BigInt::from(0) && *a.denom() > BigInt::from(0) {
                    let s1 = a.numer().sqrt();
                    let s2 = a.denom().sqrt();
                    let is_perfect = &(&s1 * &s1) == a.numer() && &(&s2 * &s2) == a.denom();
                    if is_perfect {
                        Some(Ratio::new(s1, s2))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Math::Log([_p, a]) => {
                if x(a)? == &Ratio::new(BigInt::from(1), BigInt::from(1)) {
                    Some(Ratio::new(BigInt::from(0), BigInt::from(1)))
                } else {
                    None
                }
            }
            Math::Cbrt([_p, a]) => {
                if x(a)? == &Ratio::new(BigInt::from(1), BigInt::from(1)) {
                    Some(Ratio::new(BigInt::from(1), BigInt::from(1)))
                } else {
                    None
                }
            }
            Math::Fabs([_p, a]) => Some(x(a)?.clone().abs()),
            Math::Floor([_p, a]) => Some(x(a)?.floor()),
            Math::Ceil([_p, a]) => Some(x(a)?.ceil()),
            Math::Round([_p, a]) => Some(x(a)?.round()),

            _ => None,
        }
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        match (&to, from) {
            (None, None) => false,
            (Some(_), None) => false, // no update needed
            (None, Some(c)) => {
                *to = Some(c);
                true
            }
            (Some(a), Some(ref b)) => {
                if a != b {
                    if !self.unsound.swap(true, Ordering::SeqCst) {
                        log::warn!("Bad merge detected: {} != {}", a, b);
                    }
                }
                false
            }
        }
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        if let Some(constant) = egraph[id].data.clone() {
            let added = egraph.add(Math::Constant(constant));
            let (id, _) = egraph.union(id, added);
            if egraph.analysis.prune {
                egraph[id].nodes.retain(|n| n.is_leaf())
            }
        }
    }
}
