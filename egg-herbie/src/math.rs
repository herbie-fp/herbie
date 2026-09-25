use egg::*;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::polynomial;
pub use crate::polynomial::Polynomial;
use fxhash::{FxHashMap, FxHashSet};
use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::Ratio;
use num_traits::{One, Pow, Signed, Zero};
use std::borrow::Cow;
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

// cost function similar to AstSize except it will
// penalize `(pow _ p)` where p is a fraction
pub struct AltCost<'a> {
    pub egraph: &'a EGraph,
}

impl<'a> AltCost<'a> {
    pub fn new(egraph: &'a EGraph) -> Self {
        Self { egraph }
    }
}

impl<'a> CostFunction<Math> for AltCost<'a> {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &Math, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if let Math::Pow([_, i]) = enode {
            if let Some((n, _reason)) = &self.egraph[*i].data.constant {
                if !n.denom().is_one() && n.denom().is_odd() {
                    return usize::MAX;
                }
            }
        }

        enode.fold(1, |sum, id| usize::saturating_add(sum, costs(id)))
    }
}

const MAX_POLYNOMIAL_MATCHES: usize = 10_000;

fn atom_aliases(egraph: &EGraph) -> FxHashMap<u32, u32> {
    let has_aliases = egraph.analysis.atom_ids.keys().any(|atom| {
        let mut has_stale_child = false;
        atom.for_each(|child| has_stale_child |= egraph.find(child) != child);
        has_stale_child
    });
    if !has_aliases {
        return FxHashMap::default();
    }

    let mut atoms = Vec::with_capacity(egraph.analysis.atom_ids.len());
    for (atom, id) in &egraph.analysis.atom_ids {
        let mut canonical_atom = atom.clone();
        canonical_atom.update_children(|child| egraph.find(child));
        atoms.push((canonical_atom, *id));
    }

    let mut canonical_atoms: FxHashMap<&Math, u32> =
        FxHashMap::with_capacity_and_hasher(atoms.len(), Default::default());
    for (atom, id) in &atoms {
        canonical_atoms
            .entry(atom)
            .and_modify(|canonical_id| *canonical_id = (*canonical_id).min(*id))
            .or_insert(*id);
    }
    let mut aliases = FxHashMap::default();
    for (atom, id) in &atoms {
        let canonical_id = canonical_atoms[&atom];
        if *id != canonical_id {
            aliases.insert(*id, canonical_id);
        }
    }
    aliases
}

fn polynomials_for(egraph: &mut EGraph, enode: &Math) -> Vec<Polynomial> {
    match enode {
        Math::Constant(c) => vec![Polynomial::constant(c.clone())],
        Math::Add([a, b]) => polynomial::combine(
            &egraph[*a].data.polynomials,
            &egraph[*b].data.polynomials,
            Polynomial::add,
        ),
        Math::Sub([a, b]) => polynomial::combine(
            &egraph[*a].data.polynomials,
            &egraph[*b].data.polynomials,
            polynomial::subtract,
        ),
        Math::Mul([a, b]) => polynomial::combine(
            &egraph[*a].data.polynomials,
            &egraph[*b].data.polynomials,
            Polynomial::mul,
        ),
        Math::Neg([a]) => polynomial::normalize(
            egraph[*a]
                .data
                .polynomials
                .iter()
                .map(Polynomial::neg)
                .collect(),
        ),
        _ => {
            let mut atom = enode.clone();
            atom.update_children(|id| egraph.find(id));
            vec![Polynomial::variable(egraph.analysis.atom_id(atom))]
        }
    }
}

impl IterationData<Math, ConstantFold> for IterData {
    fn make(runner: &Runner) -> Self {
        let extractor = Extractor::new(&runner.egraph, AltCost::new(&runner.egraph));
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

#[derive(Clone, Debug)]
pub struct AnalysisData {
    pub constant: Option<(Constant, (PatternAst<Math>, Subst))>,
    pub polynomials: Vec<Polynomial>,
}

pub struct ConstantFold {
    pub unsound: AtomicBool,
    pub max_abs_exponent: Ratio<BigInt>,
    pub prune: bool,
    atom_ids: FxHashMap<Math, u32>,
    next_atom_id: u32,
}

impl Clone for ConstantFold {
    fn clone(&self) -> Self {
        let unsound = AtomicBool::new(self.unsound.load(Ordering::SeqCst));
        Self {
            unsound,
            max_abs_exponent: self.max_abs_exponent.clone(),
            prune: self.prune,
            atom_ids: self.atom_ids.clone(),
            next_atom_id: self.next_atom_id,
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
            atom_ids: FxHashMap::default(),
            next_atom_id: 0,
        }
    }
}

impl ConstantFold {
    fn atom_id(&mut self, atom: Math) -> u32 {
        if let Some(&id) = self.atom_ids.get(&atom) {
            id
        } else {
            let id = self.next_atom_id;
            self.atom_ids.insert(atom, id);
            self.next_atom_id += 1;
            id
        }
    }
}

impl Analysis<Math> for ConstantFold {
    type Data = AnalysisData;
    fn make(egraph: &mut EGraph, enode: &Math) -> Self::Data {
        let x = |id: &Id| egraph[*id].data.constant.as_ref().map(|x| x.0.clone());
        let is_zero = |id: &Id| {
            egraph[*id]
                .data
                .constant
                .as_ref()
                .is_some_and(|data| data.0.is_zero())
        };

        let constant = (|| -> Option<Constant> {
            Some(match enode {
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
            })
        })();

        let constant = constant.map(|constant| {
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
            (constant, (pattern, subst))
        });

        AnalysisData {
            constant,
            polynomials: polynomials_for(egraph, enode),
        }
    }

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        let AnalysisData {
            constant: from_constant,
            polynomials: from_polynomials,
        } = from;
        let mut to_merged = false;
        let mut from_merged = false;

        match (&to.constant, from_constant) {
            (None, None) => {}
            (Some(_), None) => from_merged = true,
            (None, Some(c)) => {
                to.constant = Some(c);
                to_merged = true;
            }
            (Some(a), Some(ref b)) => {
                if a.0 != b.0 {
                    if !self.unsound.swap(true, Ordering::SeqCst) {
                        log::warn!("Bad merge detected: {} != {}", a.0, b.0);
                    }
                    from_merged = true;
                }
            }
        }

        let old_polynomials = to.polynomials.clone();
        let mut merged_polynomials = old_polynomials.clone();
        for polynomial in from_polynomials.iter().cloned() {
            polynomial::insert(&mut merged_polynomials, polynomial);
        }
        let merged_polynomials = polynomial::normalize(merged_polynomials);
        let merged_polynomials = if let Some((constant, _)) = &to.constant {
            let constant = Polynomial::constant(constant.clone());
            merged_polynomials
                .into_iter()
                .filter(|polynomial| polynomial == &constant)
                .collect()
        } else {
            merged_polynomials
        };
        to_merged |= old_polynomials != merged_polynomials;
        from_merged |= from_polynomials != merged_polynomials;
        to.polynomials = merged_polynomials;

        DidMerge(to_merged, from_merged)
    }

    fn modify(egraph: &mut EGraph, class_id: Id) {
        let class = &mut egraph[class_id];
        if let Some((c, (pat, subst))) = class.data.constant.clone() {
            egraph.union_instantiations(
                &pat,
                &format!("{}", c).parse().unwrap(),
                &subst,
                "metadata-eval".to_string(),
            );
        }
    }
}

#[derive(Debug, Default)]
pub struct PolynomialSearcher;

impl Searcher<Math, ConstantFold> for PolynomialSearcher {
    fn search_eclass_with_limit(
        &self,
        _egraph: &EGraph,
        _eclass: Id,
        _limit: usize,
    ) -> Option<SearchMatches<Math>> {
        None
    }

    fn search_with_limit(&self, egraph: &EGraph, mut limit: usize) -> Vec<SearchMatches<Math>> {
        // The backoff scheduler gives this rule an unlimited budget, but keep the custom search
        // bounded because matching every residual polynomial pair can cost more than it saves.
        limit = limit.min(MAX_POLYNOMIAL_MATCHES);
        if limit == 0 {
            return vec![];
        }
        let atom_aliases = atom_aliases(egraph);
        let mut groups: FxHashMap<Cow<'_, Polynomial>, (Id, Vec<Id>)> =
            FxHashMap::with_capacity_and_hasher(egraph.number_of_classes(), Default::default());
        for class in egraph.classes() {
            for polynomial in &class.data.polynomials {
                let polynomial = polynomial.canonicalize(&atom_aliases);
                let group = groups
                    .entry(polynomial)
                    .or_insert_with(|| (class.id, Vec::new()));
                if group.0 != class.id {
                    group.1.push(class.id);
                }
            }
        }

        let mut pairs = FxHashSet::default();
        'groups: for (representative, ids) in groups.into_values() {
            for id in ids {
                if representative != id {
                    let pair = if usize::from(representative) < usize::from(id) {
                        (representative, id)
                    } else {
                        (id, representative)
                    };
                    pairs.insert(pair);
                    if pairs.len() == limit {
                        break 'groups;
                    }
                }
            }
        }

        let mut pairs_by_representative: FxHashMap<Id, Vec<Id>> = FxHashMap::default();
        for (representative, id) in pairs {
            pairs_by_representative
                .entry(representative)
                .or_default()
                .push(id);
        }

        let mut matches = vec![];
        for (representative, ids) in pairs_by_representative {
            if limit == 0 {
                continue;
            }
            let ids = ids.into_iter().take(limit).collect::<Vec<_>>();
            limit -= ids.len();
            let substs = ids
                .into_iter()
                .map(|id| {
                    let mut subst = Subst::default();
                    subst.insert(Var::from(0), id);
                    subst
                })
                .collect();
            matches.push(SearchMatches {
                eclass: representative,
                substs,
                ast: None,
            });
        }
        matches
    }

    fn vars(&self) -> Vec<Var> {
        vec![Var::from(0)]
    }
}

#[derive(Debug, Default)]
pub struct PolynomialApplier;

impl Applier<Math, ConstantFold> for PolynomialApplier {
    fn apply_one(
        &self,
        egraph: &mut EGraph,
        eclass: Id,
        subst: &Subst,
        _searcher_ast: Option<&PatternAst<Math>>,
        rule_name: Symbol,
    ) -> Vec<Id> {
        if egraph.union_trusted(eclass, subst[Var::from(0)], rule_name) {
            vec![egraph.find(eclass)]
        } else {
            vec![]
        }
    }
}

pub fn polynomial_rewrite() -> Rewrite {
    Rewrite::new("polynomial-equality", PolynomialSearcher, PolynomialApplier).unwrap()
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

#[cfg(test)]
mod tests {
    use super::*;

    fn add(egraph: &mut EGraph, expression: &str) -> Id {
        egraph.add_expr(&expression.parse().unwrap())
    }

    fn run_polynomial_rewrite(egraph: &mut EGraph) {
        let rewrite = polynomial_rewrite();
        let matches = rewrite.search(egraph);
        rewrite.apply(egraph, &matches);
        egraph.rebuild();
    }

    #[test]
    fn polynomial_rewrite_commutes_addends() {
        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let left = add(&mut egraph, "(+ x y)");
        let right = add(&mut egraph, "(+ y x)");
        egraph.rebuild();
        let size = egraph.total_size();

        run_polynomial_rewrite(&mut egraph);

        assert_eq!(egraph.total_size(), size);
        assert_eq!(egraph.find(left), egraph.find(right));
    }

    #[test]
    fn polynomial_rewrite_distributes_products() {
        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let factored = add(&mut egraph, "(* x (+ y z))");
        let expanded = add(&mut egraph, "(+ (* x y) (* x z))");
        egraph.rebuild();

        run_polynomial_rewrite(&mut egraph);

        assert_eq!(egraph.find(factored), egraph.find(expanded));
    }

    #[test]
    fn polynomial_rewrite_folds_zero_products() {
        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let product = add(&mut egraph, "(* 0 (foo x))");
        let zero = add(&mut egraph, "0");
        egraph.rebuild();

        run_polynomial_rewrite(&mut egraph);

        assert_eq!(egraph.find(product), egraph.find(zero));
    }

    #[test]
    fn polynomial_rewrite_handles_negation_and_subtraction() {
        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let cancelled = add(&mut egraph, "(- x x)");
        let zero = add(&mut egraph, "0");
        let negated_sum = add(&mut egraph, "(neg (+ x y))");
        let sum_of_negations = add(&mut egraph, "(+ (neg x) (neg y))");
        egraph.rebuild();

        run_polynomial_rewrite(&mut egraph);

        assert_eq!(egraph.find(cancelled), egraph.find(zero));
        assert_eq!(egraph.find(negated_sum), egraph.find(sum_of_negations));
    }

    #[test]
    fn polynomial_rewrite_solves_reassociated_subtraction() {
        let variables = ["a", "b", "c", "d", "e", "f"];
        let left = variables
            .iter()
            .skip(1)
            .fold(variables[0].to_string(), |left, right| {
                format!("(+ {} {})", left, right)
            });
        let right = variables
            .iter()
            .rev()
            .skip(1)
            .fold(variables[variables.len() - 1].to_string(), |right, left| {
                format!("(+ {} {})", left, right)
            });

        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let difference = add(&mut egraph, &format!("(- {} {})", left, right));
        let zero = add(&mut egraph, "0");
        egraph.rebuild();

        run_polynomial_rewrite(&mut egraph);

        assert_eq!(egraph.find(difference), egraph.find(zero));
    }

    #[test]
    fn polynomial_rewrite_keeps_merged_eclass_options() {
        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let opaque = add(&mut egraph, "(foo x)");
        let sum = add(&mut egraph, "(+ a b)");
        let reordered_sum = add(&mut egraph, "(+ b a)");
        egraph.union(opaque, sum);
        egraph.rebuild();

        run_polynomial_rewrite(&mut egraph);

        assert_eq!(egraph.find(sum), egraph.find(reordered_sum));
    }

    #[test]
    fn polynomial_rewrite_keeps_atoms_structural() {
        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let left = add(&mut egraph, "(+ (foo x) y)");
        let right = add(&mut egraph, "(+ y (foo x))");
        egraph.rebuild();

        run_polynomial_rewrite(&mut egraph);

        assert_eq!(egraph.find(left), egraph.find(right));
    }

    #[test]
    fn polynomial_rewrite_canonicalizes_stale_atoms() {
        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let left = add(&mut egraph, "(+ (foo x) (+ a b))");
        let right = add(&mut egraph, "(+ (foo y) (+ b a))");
        let x = add(&mut egraph, "x");
        let y = add(&mut egraph, "y");
        egraph.union(x, y);
        egraph.rebuild();

        run_polynomial_rewrite(&mut egraph);

        assert_eq!(egraph.find(left), egraph.find(right));
    }

    #[test]
    fn constant_folding_still_works() {
        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let sum = add(&mut egraph, "(+ 2 3)");
        let five = add(&mut egraph, "5");
        egraph.rebuild();

        assert_eq!(egraph.find(sum), egraph.find(five));
    }

    #[test]
    fn polynomial_rewrite_does_not_spread_inconsistent_constant() {
        let mut egraph = EGraph::new(Default::default()).with_explanations_enabled();
        let one = add(&mut egraph, "1");
        let cancelled = add(&mut egraph, "(- x x)");
        let zero = add(&mut egraph, "0");
        egraph.union(one, cancelled);
        egraph.rebuild();

        run_polynomial_rewrite(&mut egraph);

        assert!(!egraph.analysis.unsound.load(Ordering::SeqCst));
        assert_ne!(egraph.find(one), egraph.find(zero));
    }
}
