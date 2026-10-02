use fxhash::FxHashMap;
use num_rational::BigRational;
use num_traits::{One, Zero};
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};

const MAX_TERMS: usize = 64;
const MAX_DEGREE: u16 = 32;
const MAX_OPTIONS: usize = 4;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Monomial(Vec<(u32, u16)>);

#[derive(Clone, Debug)]
pub struct Polynomial {
    terms: Vec<(Monomial, BigRational)>,
    hash: u64,
}

impl PartialEq for Polynomial {
    fn eq(&self, other: &Self) -> bool {
        self.terms == other.terms
    }
}

impl Eq for Polynomial {}

impl Hash for Polynomial {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl Polynomial {
    pub(crate) fn constant(c: BigRational) -> Self {
        if c.is_zero() {
            Self::new(vec![])
        } else {
            Self::new(vec![(Monomial(vec![]), c)])
        }
    }

    pub(crate) fn variable(atom: u32) -> Self {
        Self::new(vec![(Monomial(vec![(atom, 1)]), BigRational::one())])
    }

    fn new(terms: Vec<(Monomial, BigRational)>) -> Self {
        let mut hasher = fxhash::FxHasher::default();
        terms.hash(&mut hasher);
        Self {
            terms,
            hash: hasher.finish(),
        }
    }

    pub(crate) fn add(&self, other: &Self) -> Option<Self> {
        let mut terms = BTreeMap::new();
        for (monomial, coefficient) in self.terms.iter().chain(other.terms.iter()) {
            *terms
                .entry(monomial.clone())
                .or_insert_with(BigRational::zero) += coefficient;
        }
        Self::from_terms(terms)
    }

    pub(crate) fn neg(&self) -> Self {
        Self::new(
            self.terms
                .iter()
                .map(|(monomial, coefficient)| (monomial.clone(), -coefficient))
                .collect(),
        )
    }

    pub(crate) fn mul(&self, other: &Self) -> Option<Self> {
        let mut terms = BTreeMap::new();
        for (left_monomial, left_coefficient) in &self.terms {
            for (right_monomial, right_coefficient) in &other.terms {
                let monomial = Monomial::mul(left_monomial, right_monomial)?;
                *terms.entry(monomial).or_insert_with(BigRational::zero) +=
                    left_coefficient * right_coefficient;
                if terms.len() > MAX_TERMS {
                    return None;
                }
            }
        }
        Self::from_terms(terms)
    }

    fn from_terms(mut terms: BTreeMap<Monomial, BigRational>) -> Option<Self> {
        terms.retain(|_, coefficient| !coefficient.is_zero());
        if terms.len() > MAX_TERMS {
            None
        } else {
            Some(Self::new(terms.into_iter().collect()))
        }
    }

    fn cost(&self) -> (u16, usize) {
        (
            self.terms
                .iter()
                .map(|(monomial, _)| monomial.degree())
                .max()
                .unwrap_or(0),
            self.terms.len(),
        )
    }

    // Atom IDs are allocated before rebuild can canonicalize their children. A later union can
    // therefore make two IDs denote the same atom; normalize those IDs before comparing options.
    pub(crate) fn canonicalize<'a>(&'a self, atom_aliases: &FxHashMap<u32, u32>) -> Cow<'a, Self> {
        if atom_aliases.is_empty()
            || !self.terms.iter().any(|(monomial, _)| {
                monomial
                    .0
                    .iter()
                    .any(|(atom, _)| atom_aliases.contains_key(atom))
            })
        {
            return Cow::Borrowed(self);
        }

        let mut terms = BTreeMap::new();
        for (monomial, coefficient) in &self.terms {
            let mut variables = BTreeMap::new();
            for (atom, exponent) in &monomial.0 {
                let atom = atom_aliases.get(atom).copied().unwrap_or(*atom);
                *variables.entry(atom).or_insert(0) += exponent;
            }
            *terms
                .entry(Monomial(variables.into_iter().collect()))
                .or_insert_with(BigRational::zero) += coefficient;
        }
        terms.retain(|_, coefficient| !coefficient.is_zero());
        Cow::Owned(Self::new(terms.into_iter().collect()))
    }
}

impl Monomial {
    fn degree(&self) -> u16 {
        self.0.iter().map(|(_, exponent)| exponent).sum()
    }

    fn mul(left: &Self, right: &Self) -> Option<Self> {
        let mut variables = BTreeMap::new();
        for (atom, exponent) in left.0.iter().chain(right.0.iter()) {
            *variables.entry(*atom).or_insert(0) += exponent;
        }
        let monomial = Self(variables.into_iter().collect());
        (monomial.degree() <= MAX_DEGREE).then_some(monomial)
    }
}

pub(crate) fn insert(options: &mut Vec<Polynomial>, polynomial: Polynomial) {
    if !options.contains(&polynomial) {
        options.push(polynomial);
    }
}

pub(crate) fn normalize(mut options: Vec<Polynomial>) -> Vec<Polynomial> {
    options.sort_by_key(Polynomial::cost);
    options.truncate(MAX_OPTIONS);
    options
}

pub(crate) fn combine(
    left: &[Polynomial],
    right: &[Polynomial],
    operation: fn(&Polynomial, &Polynomial) -> Option<Polynomial>,
) -> Vec<Polynomial> {
    let mut options = vec![];
    for left in left {
        for right in right {
            if let Some(polynomial) = operation(left, right) {
                insert(&mut options, polynomial);
            }
        }
    }
    normalize(options)
}

pub(crate) fn subtract(left: &Polynomial, right: &Polynomial) -> Option<Polynomial> {
    left.add(&right.neg())
}
