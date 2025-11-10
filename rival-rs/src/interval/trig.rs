use super::value::{Ival, IvalClass, classify};
use crate::{
    interval::core::endpoint_unary,
    mpfr::{
        mpfr_cos, mpfr_cosu, mpfr_div, mpfr_floor_inplace, mpfr_get_exp, mpfr_pi,
        mpfr_round_inplace, mpfr_sin, mpfr_sinu, mpfr_tan, mpfr_tanu, zero,
    },
};
use rug::{Assign, Float, float::Round};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PeriodClass {
    TooWide,
    NearZero,
    RangeReduce,
}

impl Ival {
    pub fn cos_assign(&mut self, x: &Ival) {
        self.err = x.err;

        match classify_ival_periodic(x, 1) {
            PeriodClass::TooWide => {
                set_to_unit_bounds(self);
            }
            PeriodClass::NearZero => match classify(x, false) {
                IvalClass::Neg => self.monotonic_assign(&mpfr_cos, x),
                IvalClass::Pos => self.comonotonic_assign(&mpfr_cos, x),
                IvalClass::Mix => set_lower_extremum_upper_one(self, &mpfr_cos, x),
            },
            PeriodClass::RangeReduce => {
                let cap: u32 = 1 << 20;
                let p = range_reduce_precision(x.lo.as_float(), x.hi.as_float(), cap, x.prec());
                let (a, b) = compute_div_pi(x, p, mpfr_floor_inplace);

                match (a == b, diff_is_one(&a, &b), is_even(&a)) {
                    (true, _, true) => self.comonotonic_assign(&mpfr_cos, x),
                    (true, _, false) => self.monotonic_assign(&mpfr_cos, x),
                    (false, true, true) => set_lower_neg_one_upper_extremum(self, &mpfr_cos, x),
                    (false, true, false) => set_lower_extremum_upper_one(self, &mpfr_cos, x),
                    _ => set_to_unit_bounds(self),
                }
            }
        }
    }

    pub fn sin_assign(&mut self, x: &Ival) {
        self.err = x.err;

        match classify_ival_periodic(x, 1) {
            PeriodClass::TooWide => {
                set_to_unit_bounds(self);
            }
            PeriodClass::NearZero => {
                self.monotonic_assign(&mpfr_sin, x);
            }
            PeriodClass::RangeReduce => {
                let cap: u32 = 1 << 20;
                let p = range_reduce_precision(x.lo.as_float(), x.hi.as_float(), cap, x.prec());
                let (a, b) = compute_div_pi(x, p, mpfr_round_inplace);

                match (a == b, diff_is_one(&a, &b), is_even(&a)) {
                    (true, _, true) => self.monotonic_assign(&mpfr_sin, x),
                    (true, _, false) => self.comonotonic_assign(&mpfr_sin, x),
                    (false, true, false) => set_lower_neg_one_upper_extremum(self, &mpfr_sin, x),
                    (false, true, true) => set_lower_extremum_upper_one(self, &mpfr_sin, x),
                    _ => set_to_unit_bounds(self),
                }
            }
        }
    }

    pub fn tan_assign(&mut self, x: &Ival) {
        match classify_ival_periodic(x, 0) {
            PeriodClass::TooWide => {
                set_to_infinite_bounds(self, x.lo.immovable && x.hi.immovable);
                self.err.partial = true;
                self.err.total = x.err.total;
            }
            PeriodClass::NearZero => {
                self.monotonic_assign(&mpfr_tan, x);
                self.err = x.err;
            }
            PeriodClass::RangeReduce => {
                let cap: u32 = 1 << 20;
                let p = range_reduce_precision(x.lo.as_float(), x.hi.as_float(), cap, x.prec());
                let (a, b) = compute_div_pi(x, p, mpfr_round_inplace);
                if a == b {
                    self.monotonic_assign(&mpfr_tan, x);
                    self.err = x.err;
                } else {
                    set_to_infinite_bounds(self, x.lo.immovable && x.hi.immovable);
                    self.err.partial = true;
                    self.err.total = x.err.total;
                }
            }
        }
    }

    pub fn cosu_assign(&mut self, x: &Ival, n: u64) {
        self.err = x.err;
        let period_qtr = period_quarter_bitlen(n, 4);

        match classify_ival_periodic(x, period_qtr) {
            PeriodClass::TooWide => {
                set_to_unit_bounds(self);
            }
            PeriodClass::NearZero => match classify(x, false) {
                IvalClass::Neg => {
                    self.monotonic_assign(&|x, out, rnd| mpfr_cosu(x, n, out, rnd), x)
                }
                IvalClass::Pos => {
                    self.comonotonic_assign(&|x, out, rnd| mpfr_cosu(x, n, out, rnd), x)
                }
                IvalClass::Mix => {
                    set_lower_extremum_upper_one(self, &|x, out, rnd| mpfr_cosu(x, n, out, rnd), x)
                }
            },
            PeriodClass::RangeReduce => {
                let cap: u32 = 1 << 20;
                let p = range_reduce_precision(x.lo.as_float(), x.hi.as_float(), cap, x.prec());
                let (a, b) = compute_div_n_half(x, n, p, mpfr_floor_inplace);

                match (a == b, diff_is_one(&a, &b), is_even(&a)) {
                    (true, _, true) => {
                        self.comonotonic_assign(&|x, out, rnd| mpfr_cosu(x, n, out, rnd), x)
                    }
                    (true, _, false) => {
                        self.monotonic_assign(&|x, out, rnd| mpfr_cosu(x, n, out, rnd), x)
                    }
                    (false, true, true) => set_lower_neg_one_upper_extremum(
                        self,
                        &|x, out, rnd| mpfr_cosu(x, n, out, rnd),
                        x,
                    ),
                    (false, true, false) => set_lower_extremum_upper_one(
                        self,
                        &|x, out, rnd| mpfr_cosu(x, n, out, rnd),
                        x,
                    ),
                    _ => set_to_unit_bounds(self),
                }
            }
        }
    }

    pub fn sinu_assign(&mut self, x: &Ival, n: u64) {
        self.err = x.err;
        let period_qtr = period_quarter_bitlen(n, 4);

        match classify_ival_periodic(x, period_qtr) {
            PeriodClass::TooWide => {
                set_to_unit_bounds(self);
            }
            PeriodClass::NearZero => {
                self.monotonic_assign(&|x, out, rnd| mpfr_sinu(x, n, out, rnd), x);
            }
            PeriodClass::RangeReduce => {
                let cap: u32 = 1 << 20;
                let p = range_reduce_precision(x.lo.as_float(), x.hi.as_float(), cap, x.prec());
                let (a, b) = compute_div_n_half(x, n, p, mpfr_round_inplace);

                match (a == b, diff_is_one(&a, &b), is_even(&a)) {
                    (true, _, true) => {
                        self.monotonic_assign(&|x, out, rnd| mpfr_sinu(x, n, out, rnd), x)
                    }
                    (true, _, false) => {
                        self.comonotonic_assign(&|x, out, rnd| mpfr_sinu(x, n, out, rnd), x)
                    }
                    (false, true, false) => set_lower_neg_one_upper_extremum(
                        self,
                        &|x, out, rnd| mpfr_sinu(x, n, out, rnd),
                        x,
                    ),
                    (false, true, true) => set_lower_extremum_upper_one(
                        self,
                        &|x, out, rnd| mpfr_sinu(x, n, out, rnd),
                        x,
                    ),
                    _ => set_to_unit_bounds(self),
                }
            }
        }
    }

    pub fn tanu_assign(&mut self, x: &Ival, n: u64) {
        let period_qtr = period_quarter_bitlen(n, 8);

        match classify_ival_periodic(x, period_qtr) {
            PeriodClass::TooWide => {
                set_to_infinite_bounds(self, x.lo.immovable && x.hi.immovable);
                self.err.partial = true;
                self.err.total = x.err.total;
            }
            PeriodClass::NearZero => {
                self.monotonic_assign(&|x, out, rnd| mpfr_tanu(x, n, out, rnd), x);
                self.err = x.err;
            }
            PeriodClass::RangeReduce => {
                let cap: u32 = 1 << 20;
                let p = range_reduce_precision(x.lo.as_float(), x.hi.as_float(), cap, x.prec());
                let (a, b) = compute_div_n_half(x, n, p, mpfr_round_inplace);
                if a == b {
                    self.monotonic_assign(&|x, out, rnd| mpfr_tanu(x, n, out, rnd), x);
                    self.err = x.err;
                } else {
                    set_to_infinite_bounds(self, x.lo.immovable && x.hi.immovable);
                    self.err.partial = true;
                    self.err.total = x.err.total;
                }
            }
        }
    }
}

/// Apply unary function to two input endpoints and pick min or max for output
/// Returns the appropriate immovability flag based on which endpoint was chosen
fn endpoint_unary_pick_extremum<F>(
    f: &F,
    ep1: &super::value::Endpoint,
    ep2: &super::value::Endpoint,
    out: &mut Float,
    rnd: Round,
    prefer_min: bool,
) -> bool
where
    F: Fn(&Float, &mut Float, Round) -> bool,
{
    let prec = out.prec();
    let mut tmp = zero(prec);
    let imm1 = endpoint_unary(f, ep1, out, rnd);
    let imm2 = endpoint_unary(f, ep2, &mut tmp, rnd);

    let should_swap = if prefer_min { *out > tmp } else { *out < tmp };

    if should_swap {
        out.assign(&tmp);
        imm2
    } else if *out == tmp {
        imm1 || imm2
    } else {
        imm1
    }
}

fn classify_ival_periodic(x: &Ival, period_quarter_bitlen: i64) -> PeriodClass {
    let xlo = x.lo.as_float();
    let xhi = x.hi.as_float();

    if xlo.is_infinite() || xhi.is_infinite() {
        return PeriodClass::TooWide;
    }

    let (lo_exp, hi_exp) = (mpfr_get_exp(xlo), mpfr_get_exp(xhi));
    if lo_exp < period_quarter_bitlen && hi_exp < period_quarter_bitlen {
        return PeriodClass::NearZero;
    }

    let (lo_prec, hi_prec) = (xlo.prec() as i64, xhi.prec() as i64);
    let (lo_ulp, hi_ulp) = (
        lo_exp.saturating_sub(lo_prec),
        hi_exp.saturating_sub(hi_prec),
    );

    if lo_ulp > 0 || hi_ulp > 0 {
        if xlo == xhi {
            PeriodClass::RangeReduce
        } else {
            PeriodClass::TooWide
        }
    } else {
        PeriodClass::RangeReduce
    }
}

fn range_reduce_precision(xlo: &Float, xhi: &Float, cap: u32, curr: u32) -> u32 {
    let lo = (mpfr_get_exp(xlo) + 2 * (xlo.prec() as i64)).max(curr as i64) as u32;
    let hi = (mpfr_get_exp(xhi) + 2 * (xhi.prec() as i64)).max(curr as i64) as u32;
    lo.max(hi).min(cap).max(curr)
}

fn compute_div_pi(x: &Ival, prec: u32, round_fn: fn(&mut Float) -> bool) -> (Float, Float) {
    let (mut pi_lo, mut pi_hi) = (zero(prec), zero(prec));
    mpfr_pi(&mut pi_lo, Round::Down);
    mpfr_pi(&mut pi_hi, Round::Up);
    let (mut q_lo, mut q_hi) = (zero(prec), zero(prec));
    mpfr_div(x.lo.as_float(), &pi_hi, &mut q_lo, Round::Down);
    mpfr_div(x.hi.as_float(), &pi_lo, &mut q_hi, Round::Up);
    round_fn(&mut q_lo);
    round_fn(&mut q_hi);
    (q_lo, q_hi)
}

fn compute_div_n_half(
    x: &Ival,
    n: u64,
    prec: u32,
    round_fn: fn(&mut Float) -> bool,
) -> (Float, Float) {
    let n_half_lo = Float::with_val(prec, n) / 2u32;
    let n_half_hi = Float::with_val(prec, n) / 2u32;
    let (mut q_lo, mut q_hi) = (zero(prec), zero(prec));
    mpfr_div(x.lo.as_float(), &n_half_hi, &mut q_lo, Round::Down);
    mpfr_div(x.hi.as_float(), &n_half_lo, &mut q_hi, Round::Up);
    round_fn(&mut q_lo);
    round_fn(&mut q_hi);
    (q_lo, q_hi)
}

fn period_quarter_bitlen(n: u64, divisor: u64) -> i64 {
    let quarter = n / divisor;
    if quarter > 0 {
        (quarter.ilog2() + 1) as i64
    } else {
        0
    }
}

fn is_even(x: &Float) -> bool {
    let prec = x.prec();
    let mut t = Float::with_val(prec, x);
    t /= 2;
    mpfr_floor_inplace(&mut t);
    t *= 2;
    t == *x
}

fn diff_is_one(a: &Float, b: &Float) -> bool {
    let prec = a.prec().max(b.prec());
    let mut d = Float::with_val(prec, b);
    d -= a;
    d == 1
}

/// Set interval to [-1, 1] with both endpoints movable
fn set_to_unit_bounds(result: &mut Ival) {
    result.lo.as_float_mut().assign(-1);
    result.hi.as_float_mut().assign(1);
    result.lo.immovable = false;
    result.hi.immovable = false;
}

/// Set interval to [-inf, inf] with immovability based on input
fn set_to_infinite_bounds(result: &mut Ival, immovable: bool) {
    result.lo.as_float_mut().assign(f64::NEG_INFINITY);
    result.hi.as_float_mut().assign(f64::INFINITY);
    result.lo.immovable = immovable;
    result.hi.immovable = immovable;
}

/// Set lower bound to min of extrema, upper to 1
fn set_lower_extremum_upper_one<F>(result: &mut Ival, f: &F, x: &Ival)
where
    F: Fn(&Float, &mut Float, Round) -> bool,
{
    result.set_prec(x.prec());
    result.lo.immovable =
        endpoint_unary_pick_extremum(f, &x.lo, &x.hi, result.lo.as_float_mut(), Round::Down, true);
    result.hi.as_float_mut().assign(1);
    result.hi.immovable = false;
}

/// Set lower bound to -1, upper to max of extrema
fn set_lower_neg_one_upper_extremum<F>(result: &mut Ival, f: &F, x: &Ival)
where
    F: Fn(&Float, &mut Float, Round) -> bool,
{
    result.set_prec(x.prec());
    result.lo.as_float_mut().assign(-1);
    result.lo.immovable = false;
    result.hi.immovable =
        endpoint_unary_pick_extremum(f, &x.lo, &x.hi, result.hi.as_float_mut(), Round::Up, false);
}
