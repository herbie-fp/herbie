use rug::{
    Assign, Float,
    float::{OrdFloat, Round},
    ops::AssignRound,
};

use crate::mpfr::zero;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Endpoint {
    pub val: OrdFloat,
    pub immovable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ival {
    pub lo: Endpoint,
    pub hi: Endpoint,
    pub err: ErrorFlags,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ErrorFlags {
    pub partial: bool,
    pub total: bool,
}

impl Endpoint {
    pub fn new(val: OrdFloat, immovable: bool) -> Self {
        Endpoint { val, immovable }
    }

    #[inline]
    pub fn as_float(&self) -> &Float {
        self.val.as_float()
    }

    #[inline]
    pub fn as_float_mut(&mut self) -> &mut Float {
        self.val.as_float_mut()
    }

    pub fn endpoint_min2_assign(&mut self, b: Endpoint) {
        use std::cmp::Ordering;
        match self.val.cmp(&b.val) {
            Ordering::Less => (),
            Ordering::Greater => *self = b,
            Ordering::Equal => self.immovable |= b.immovable,
        }
    }

    pub fn endpoint_max2_assign(&mut self, b: Endpoint) {
        use std::cmp::Ordering;
        match self.val.cmp(&b.val) {
            Ordering::Greater => (),
            Ordering::Less => *self = b,
            Ordering::Equal => self.immovable |= b.immovable,
        }
    }
}

impl Ival {
    pub fn new(lo: Endpoint, hi: Endpoint, err: ErrorFlags) -> Self {
        assert!(lo.as_float().prec() == hi.as_float().prec());
        Ival { lo, hi, err }
    }

    #[inline]
    pub fn prec(&self) -> u32 {
        self.lo.as_float().prec()
    }

    #[inline]
    pub fn set_prec(&mut self, prec: u32) {
        self.lo.as_float_mut().set_prec(prec);
        self.hi.as_float_mut().set_prec(prec);
    }

    pub fn from_lo_hi(lo: Float, hi: Float) -> Self {
        let err = if lo.is_nan() || hi.is_nan() || (lo.eq(&hi) && lo.is_infinite()) {
            ErrorFlags::error()
        } else {
            ErrorFlags::none()
        };
        Ival {
            lo: Endpoint::new(OrdFloat::from(lo), false),
            hi: Endpoint::new(OrdFloat::from(hi), false),
            err,
        }
    }

    #[inline]
    pub fn bool_interval(lo_true: bool, hi_true: bool) -> Self {
        // 2-bit precision is sufficient for 0/1 endpoints
        let to_float = |b: bool| Float::with_val(2, if b { 1 } else { 0 });
        Self::from_lo_hi(to_float(lo_true), to_float(hi_true))
    }

    pub fn f64_assign(&mut self, value: f64) {
        self.lo.as_float_mut().assign_round(value, Round::Down);
        self.hi.as_float_mut().assign_round(value, Round::Up);
        self.err = ErrorFlags::none();
    }

    pub fn zero(prec: u32) -> Self {
        let lo = Float::with_val(prec, 0);
        let hi = Float::with_val(prec, 0);
        Ival::new(
            Endpoint::new(OrdFloat::from(lo), true),
            Endpoint::new(OrdFloat::from(hi), true),
            ErrorFlags::none(),
        )
    }

    pub fn assign_from(&mut self, src: &Ival) {
        self.lo.as_float_mut().assign(src.lo.as_float());
        self.lo.immovable = src.lo.immovable;
        self.hi.as_float_mut().assign(src.hi.as_float());
        self.hi.immovable = src.hi.immovable;
        self.err = src.err;
    }

    pub fn union_assign(&mut self, other: Ival) {
        if self.err.total {
            self.lo = other.lo;
            self.hi = other.hi;
            self.err = other.err;
            self.err.partial = true;
            return;
        }

        if other.err.total {
            self.err.partial = true;
            return;
        }

        self.lo.endpoint_min2_assign(other.lo);
        self.hi.endpoint_max2_assign(other.hi);
        self.err = self.err.union_disjoint(&other.err);
    }

    /// Return Some(false) if interval is exactly [0,0], Some(true) if [1,1], else None.
    /// Returns None whenever there are error flags present.
    pub fn known_bool(&self) -> Option<bool> {
        if self.err.partial || self.err.total {
            return None;
        }
        let lo = self.lo.as_float();
        let hi = self.hi.as_float();
        if lo.is_zero() && hi.is_zero() {
            Some(false)
        } else if *lo == 1 && *hi == 1 {
            Some(true)
        } else {
            None
        }
    }

    // The following helpers mirror previous clamp logic
    pub fn clamp(&mut self, lo: Float, hi: Float) {
        let x_lo = self.lo.as_float();
        let x_hi = self.hi.as_float();

        self.err = ErrorFlags::new(
            self.err.partial || x_lo < &lo || x_hi > &hi,
            self.err.total || x_hi < &lo || x_lo > &hi,
        );

        if lo.is_zero() && x_hi.is_zero() {
            self.lo.val = OrdFloat::from(zero(self.prec()));
            self.hi.val = OrdFloat::from(zero(self.prec()));
        } else {
            if x_lo < &lo {
                self.lo.val = OrdFloat::from(lo)
            }

            if x_hi > &hi {
                self.hi.val = OrdFloat::from(hi);
            }
        }
    }

    pub fn clamp_strict(&mut self, lo: Float, hi: Float) {
        let x_lo = self.lo.as_float();
        let x_hi = self.hi.as_float();

        self.err = ErrorFlags::new(
            self.err.partial || x_lo <= &lo || x_hi >= &hi,
            self.err.total || x_hi <= &lo || x_lo >= &hi,
        );

        if x_lo < &lo {
            self.lo.val = OrdFloat::from(lo)
        }

        if x_hi > &hi {
            self.hi.val = OrdFloat::from(hi);
        }
    }

    pub fn split_at(&self, val: &Float) -> (Ival, Ival) {
        let lower = Ival::new(
            self.lo.clone(),
            Endpoint::new(OrdFloat::from(val.clone()), self.hi.immovable),
            self.err,
        );
        let upper = Ival::new(
            Endpoint::new(OrdFloat::from(val.clone()), self.lo.immovable),
            self.hi.clone(),
            self.err,
        );
        (lower, upper)
    }
}

impl ErrorFlags {
    pub fn new(partial: bool, total: bool) -> Self {
        ErrorFlags { partial, total }
    }

    pub fn none() -> Self {
        ErrorFlags::new(false, false)
    }

    pub fn error() -> Self {
        ErrorFlags::new(true, true)
    }

    pub fn union(&self, other: &ErrorFlags) -> ErrorFlags {
        ErrorFlags::new(self.partial || other.partial, self.total || other.total)
    }

    pub fn union_disjoint(&self, other: &ErrorFlags) -> ErrorFlags {
        ErrorFlags::new(self.partial || other.partial, self.total && other.total)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IvalClass {
    Pos,
    Neg,
    Mix,
}

pub fn classify(ival: &Ival, strict: bool) -> IvalClass {
    let lo = ival.lo.as_float();
    let hi = ival.hi.as_float();
    if strict {
        if *lo > 0.0 {
            IvalClass::Pos
        } else if *hi < 0.0 {
            IvalClass::Neg
        } else {
            IvalClass::Mix
        }
    } else if *lo >= 0.0 {
        IvalClass::Pos
    } else if *hi <= 0.0 {
        IvalClass::Neg
    } else {
        IvalClass::Mix
    }
}
