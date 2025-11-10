//! Core interval operations

use super::value::{Endpoint, ErrorFlags, Ival, IvalClass, classify};
use crate::mpfr::{
    acosh_overflow_threshold, exp_overflow_threshold, exp2_overflow_threshold, inf, mpfr_abs,
    mpfr_acos, mpfr_acosh, mpfr_asin, mpfr_asinh, mpfr_atan, mpfr_atan2, mpfr_atanh, mpfr_cbrt,
    mpfr_cmpabs, mpfr_cosh, mpfr_erf, mpfr_erfc, mpfr_exp, mpfr_exp2, mpfr_expm1, mpfr_log,
    mpfr_log1p, mpfr_log2, mpfr_log10, mpfr_max, mpfr_min, mpfr_neg, mpfr_pi, mpfr_rint, mpfr_sign,
    mpfr_sinh, mpfr_sqrt, mpfr_tanh, sinh_overflow_threshold, zero,
};
use crate::mpfr::{is_exact_operation, mpfr_get_exp};
use rug::{Assign, Float, float::Round, ops::NegAssign};

impl Ival {
    pub fn max_prec(&self) -> u32 {
        self.lo.as_float().prec().max(self.hi.as_float().prec())
    }

    pub fn monotonic_assign<F>(&mut self, mpfr_func: &F, a: &Ival)
    where
        F: Fn(&Float, &mut Float, Round) -> bool,
    {
        self.lo.immovable = endpoint_unary(mpfr_func, &a.lo, self.lo.as_float_mut(), Round::Down);
        self.hi.immovable = endpoint_unary(mpfr_func, &a.hi, self.hi.as_float_mut(), Round::Up);
    }

    pub fn comonotonic_assign<F>(&mut self, mpfr_func: &F, a: &Ival)
    where
        F: Fn(&Float, &mut Float, Round) -> bool,
    {
        self.lo.immovable = endpoint_unary(mpfr_func, &a.hi, self.lo.as_float_mut(), Round::Down);
        self.hi.immovable = endpoint_unary(mpfr_func, &a.lo, self.hi.as_float_mut(), Round::Up);
    }

    pub fn overflows_loose_at(&mut self, a: &Ival, lo: Float, hi: Float) {
        let x_lo = a.lo.as_float();
        let x_hi = a.hi.as_float();

        self.lo.immovable = self.lo.immovable || x_hi <= &lo || (x_lo <= &lo && a.lo.immovable);
        self.hi.immovable = self.hi.immovable || x_lo >= &hi || (x_hi >= &hi && a.hi.immovable);
        self.err = a.err;
    }

    pub fn neg_assign(&mut self, a: &Ival) {
        self.comonotonic_assign(&mpfr_neg, a);
    }

    pub fn exact_neg_assign(&mut self, a: &Ival) {
        let prec = a.max_prec();
        self.set_prec(prec);
        self.neg_assign(a);
    }

    pub fn fabs_assign(&mut self, x: &Ival) {
        match classify(x, false) {
            IvalClass::Neg => self.comonotonic_assign(&mpfr_abs, x),
            IvalClass::Pos => self.monotonic_assign(&mpfr_abs, x),
            IvalClass::Mix => {
                let prec = x.prec();
                let mut tmp1 = zero(prec);
                let mut tmp2 = zero(prec);

                let abs_lo_imm = endpoint_unary(mpfr_abs, &x.lo, &mut tmp1, Round::Up);
                let abs_hi_imm = endpoint_unary(mpfr_abs, &x.hi, &mut tmp2, Round::Up);

                self.lo.as_float_mut().assign(0);
                self.lo.immovable = x.lo.immovable && x.hi.immovable;

                if tmp1 > tmp2 {
                    self.hi.as_float_mut().assign(&tmp1);
                    self.hi.immovable = abs_lo_imm;
                } else {
                    self.hi.as_float_mut().assign(&tmp2);
                    self.hi.immovable = if tmp1 == tmp2 {
                        abs_lo_imm || abs_hi_imm
                    } else {
                        abs_hi_imm
                    };
                }

                self.err = x.err;
            }
        }
    }

    pub fn exact_fabs_assign(&mut self, a: &Ival) {
        let prec = a.max_prec();
        self.set_prec(prec);
        self.fabs_assign(a);
    }

    pub fn pre_fabs_assign(&mut self, x: &Ival) {
        match classify(x, false) {
            IvalClass::Pos => {
                self.assign_from(x);
            }
            IvalClass::Neg => {
                self.lo.as_float_mut().assign(x.hi.as_float());
                self.lo.immovable = x.hi.immovable;
                self.hi.as_float_mut().assign(x.lo.as_float());
                self.hi.immovable = x.lo.immovable;
                self.err = x.err;
            }
            IvalClass::Mix => {
                self.lo.as_float_mut().assign(0);
                self.lo.immovable = x.lo.immovable && x.hi.immovable;

                if mpfr_cmpabs(x.lo.as_float(), x.hi.as_float()) > 0 {
                    self.hi.as_float_mut().assign(x.lo.as_float());
                    self.hi.immovable = x.lo.immovable;
                } else {
                    self.hi.as_float_mut().assign(x.hi.as_float());
                    self.hi.immovable = x.hi.immovable;
                }
                self.err = x.err;
            }
        }
    }

    pub fn sqrt_assign(&mut self, a: &Ival) {
        // TODO: To get rid of all these clones when clamping, we can add inplace operators to mpfr.rs
        let mut clamped = a.clone();
        clamped.clamp(zero(self.prec()), inf(self.prec()));
        self.err = clamped.err;
        self.monotonic_assign(&mpfr_sqrt, &clamped);
    }

    pub fn cbrt_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_cbrt, a);
    }

    pub fn exp_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_exp, a);
        self.overflows_loose_at(
            a,
            -exp_overflow_threshold(self.prec()),
            exp_overflow_threshold(self.prec()),
        );
    }

    pub fn exp2_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_exp2, a);
        self.overflows_loose_at(
            a,
            -exp2_overflow_threshold(self.prec()),
            exp2_overflow_threshold(self.prec()),
        );
    }

    pub fn expm1_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_expm1, a);
        self.overflows_at(
            a,
            -exp_overflow_threshold(self.prec()),
            exp_overflow_threshold(self.prec()),
        );
    }

    pub fn log_assign(&mut self, a: &Ival) {
        let mut clamped = a.clone();
        clamped.clamp_strict(zero(self.prec()), inf(self.prec()));
        self.err = clamped.err;
        self.monotonic_assign(&mpfr_log, &clamped);
    }

    pub fn log2_assign(&mut self, a: &Ival) {
        let mut clamped = a.clone();
        clamped.clamp_strict(zero(self.prec()), inf(self.prec()));
        self.err = clamped.err;
        self.monotonic_assign(&mpfr_log2, &clamped);
    }

    pub fn log10_assign(&mut self, a: &Ival) {
        let mut clamped = a.clone();
        clamped.clamp_strict(zero(self.prec()), inf(self.prec()));
        self.err = clamped.err;
        self.monotonic_assign(&mpfr_log10, &clamped);
    }

    pub fn log1p_assign(&mut self, a: &Ival) {
        let mut clamped = a.clone();
        let neg_one = Float::with_val(self.prec(), -1);
        clamped.clamp_strict(neg_one, inf(self.prec()));
        self.err = clamped.err;
        self.monotonic_assign(&mpfr_log1p, &clamped);
    }

    pub fn logb_assign(&mut self, a: &Ival) {
        let mut abs_a = Ival::zero(a.max_prec());
        let mut tmp = Ival::zero(self.prec());
        abs_a.exact_fabs_assign(a);
        tmp.log2_assign(&abs_a);
        self.floor_assign(&tmp);
    }

    pub fn asin_assign(&mut self, a: &Ival) {
        let mut clamped = a.clone();
        let one = Float::with_val(self.prec(), 1);
        let neg_one = Float::with_val(self.prec(), -1);
        clamped.clamp(neg_one, one);
        self.err = clamped.err;
        self.monotonic_assign(&mpfr_asin, &clamped);
    }

    pub fn acos_assign(&mut self, a: &Ival) {
        let mut clamped = a.clone();
        let one = Float::with_val(self.prec(), 1);
        let neg_one = Float::with_val(self.prec(), -1);
        clamped.clamp(neg_one, one);
        self.err = clamped.err;
        self.comonotonic_assign(&mpfr_acos, &clamped);
    }

    pub fn atan_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_atan, a);
    }

    pub fn atan2_assign(&mut self, y: &Ival, x: &Ival) {
        let class_x = classify(x, true);
        let class_y = classify(y, true);

        let err = y.err.union(&x.err);
        self.err = err;

        let mut mkatan = |a: &Endpoint, b: &Endpoint, c: &Endpoint, d: &Endpoint| {
            let prec = self.prec();
            let mut lo_out = zero(prec);
            let mut hi_out = zero(prec);

            self.lo.immovable = endpoint_binary(mpfr_atan2, a, b, &mut lo_out, Round::Down);
            self.hi.immovable = endpoint_binary(mpfr_atan2, c, d, &mut hi_out, Round::Up);
            self.lo.as_float_mut().assign(&lo_out);
            self.hi.as_float_mut().assign(&hi_out);
            self.err = err;
        };

        match (class_x, class_y) {
            (IvalClass::Neg, IvalClass::Neg) => mkatan(&y.hi, &x.lo, &y.lo, &x.hi),
            (IvalClass::Mix, IvalClass::Neg) => mkatan(&y.hi, &x.lo, &y.hi, &x.hi),
            (IvalClass::Pos, IvalClass::Neg) => mkatan(&y.lo, &x.lo, &y.hi, &x.hi),
            (IvalClass::Pos, IvalClass::Mix) => mkatan(&y.lo, &x.lo, &y.hi, &x.lo),
            (IvalClass::Pos, IvalClass::Pos) => mkatan(&y.lo, &x.hi, &y.hi, &x.lo),
            (IvalClass::Mix, IvalClass::Pos) => mkatan(&y.lo, &x.hi, &y.lo, &x.lo),
            (IvalClass::Neg, IvalClass::Pos) => mkatan(&y.hi, &x.hi, &y.lo, &x.lo),
            (_, IvalClass::Mix) => {
                let prec = self.prec();
                let mut pi_lo = zero(prec);
                let mut pi_hi = zero(prec);
                mpfr_pi(&mut pi_lo, Round::Down);
                mpfr_pi(&mut pi_hi, Round::Up);

                let mut neg_pi = zero(prec);
                neg_pi.assign(&pi_hi);
                neg_pi.neg_assign();
                self.lo.as_float_mut().assign(&neg_pi);
                self.hi.as_float_mut().assign(&pi_hi);
                self.lo.immovable = false;
                self.hi.immovable = false;

                let x_lo = x.lo.as_float();
                let x_hi = x.hi.as_float();
                let y_lo = y.lo.as_float();
                let y_hi = y.hi.as_float();

                self.err.partial = err.partial || x_hi >= &zero(prec);
                self.err.total = err.total
                    || (x_lo.is_zero() && x_hi.is_zero() && y_lo.is_zero() && y_hi.is_zero());
            }
        }
    }

    pub fn sinh_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_sinh, a);
        self.overflows_at(
            a,
            -sinh_overflow_threshold(self.prec()),
            sinh_overflow_threshold(self.prec()),
        );
    }

    pub fn cosh_assign(&mut self, a: &Ival) {
        let mut abs_a = Ival::zero(a.max_prec());
        abs_a.exact_fabs_assign(a);
        self.monotonic_assign(&mpfr_cosh, &abs_a);
        self.overflows_at(
            &abs_a,
            -acosh_overflow_threshold(self.prec()),
            acosh_overflow_threshold(self.prec()),
        );
    }

    pub fn tanh_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_tanh, a);
    }

    pub fn asinh_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_asinh, a);
    }

    pub fn acosh_assign(&mut self, a: &Ival) {
        let mut clamped = a.clone();
        let one = Float::with_val(self.prec(), 1);
        clamped.clamp(one, inf(self.prec()));
        self.err = clamped.err;
        self.monotonic_assign(&mpfr_acosh, &clamped);
    }

    pub fn atanh_assign(&mut self, a: &Ival) {
        let mut clamped = a.clone();
        let one = Float::with_val(self.prec(), 1);
        let neg_one = Float::with_val(self.prec(), -1);
        clamped.clamp_strict(neg_one, one);
        self.err = clamped.err;
        self.monotonic_assign(&mpfr_atanh, &clamped);
    }

    pub fn erf_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_erf, a);
    }

    pub fn erfc_assign(&mut self, a: &Ival) {
        self.comonotonic_assign(&mpfr_erfc, a);
    }

    pub fn rint_assign(&mut self, a: &Ival) {
        self.monotonic_assign(&mpfr_rint, a);
    }

    pub fn round_assign(&mut self, a: &Ival) {
        self.monotonic_assign(
            &|input: &Float, out: &mut Float, rnd: Round| fix_rounding_round(input, out, rnd),
            a,
        );
    }

    pub fn ceil_assign(&mut self, a: &Ival) {
        self.monotonic_assign(
            &|input: &Float, out: &mut Float, rnd: Round| fix_rounding_ceil(input, out, rnd),
            a,
        );
    }

    pub fn floor_assign(&mut self, a: &Ival) {
        self.monotonic_assign(
            &|input: &Float, out: &mut Float, rnd: Round| fix_rounding_floor(input, out, rnd),
            a,
        );
    }

    pub fn trunc_assign(&mut self, a: &Ival) {
        self.monotonic_assign(
            &|input: &Float, out: &mut Float, rnd: Round| fix_rounding_trunc(input, out, rnd),
            a,
        );
    }

    pub fn fmin_assign(&mut self, a: &Ival, b: &Ival) {
        self.lo.immovable =
            endpoint_minmax(mpfr_min, &a.lo, &b.lo, self.lo.as_float_mut(), Round::Down);
        self.hi.immovable =
            endpoint_minmax(mpfr_min, &a.hi, &b.hi, self.hi.as_float_mut(), Round::Up);
        self.err = a.err.union(&b.err);
    }

    pub fn fmax_assign(&mut self, a: &Ival, b: &Ival) {
        self.lo.immovable =
            endpoint_minmax(mpfr_max, &a.lo, &b.lo, self.lo.as_float_mut(), Round::Down);
        self.hi.immovable =
            endpoint_minmax(mpfr_max, &a.hi, &b.hi, self.hi.as_float_mut(), Round::Up);
        self.err = a.err.union(&b.err);
    }

    pub fn copysign_assign(&mut self, x: &Ival, y: &Ival) {
        let mut abs_x = Ival::zero(self.prec());
        abs_x.fabs_assign(x);

        let y_lo = y.lo.as_float();
        let y_hi = y.hi.as_float();

        let can_zero = y_lo.is_zero() || y_hi.is_zero();
        let can_neg = mpfr_sign(y_lo) == -1 || can_zero;
        let can_pos = mpfr_sign(y_hi) == 1 || can_zero;

        let err = y.err.union(&abs_x.err);

        match (can_neg, can_pos) {
            (true, true) => {
                let prec = self.prec();
                let mut neg_hi = zero(prec);
                let imm_lo = endpoint_unary(mpfr_neg, &abs_x.hi, &mut neg_hi, Round::Down);

                self.lo.as_float_mut().assign(&neg_hi);
                self.lo.immovable = imm_lo;
                self.hi.as_float_mut().assign(abs_x.hi.as_float());
                self.hi.immovable = abs_x.hi.immovable;
                self.err = err;
            }
            (true, false) => {
                let prec = self.prec();
                let mut neg_hi = zero(prec);
                let mut neg_lo = zero(prec);
                let imm_lo = endpoint_unary(mpfr_neg, &abs_x.hi, &mut neg_hi, Round::Down);
                let imm_hi = endpoint_unary(mpfr_neg, &abs_x.lo, &mut neg_lo, Round::Up);

                self.lo.as_float_mut().assign(&neg_hi);
                self.lo.immovable = imm_lo;
                self.hi.as_float_mut().assign(&neg_lo);
                self.hi.immovable = imm_hi;
                self.err = err;
            }
            (false, true) => {
                self.assign_from(&abs_x);
                self.err = err;
            }
            (false, false) => {
                self.lo.as_float_mut().assign(f64::NAN);
                self.hi.as_float_mut().assign(f64::NAN);
                self.lo.immovable = true;
                self.hi.immovable = true;
                self.err = ErrorFlags::error();
            }
        }
    }

    fn overflows_at(&mut self, a: &Ival, lo: Float, hi: Float) {
        let x_lo = a.lo.as_float();
        let x_hi = a.hi.as_float();

        self.lo.immovable = self.lo.immovable || (x_hi <= &lo && a.lo.immovable);
        self.hi.immovable = self.hi.immovable || (x_lo >= &hi && a.hi.immovable);
    }
}

#[must_use]
pub fn endpoint_unary(
    f: impl FnOnce(&Float, &mut Float, Round) -> bool,
    ep: &Endpoint,
    out: &mut Float,
    rnd: Round,
) -> bool {
    let v = ep.as_float();
    let exact = f(v, out, rnd);
    ep.immovable && (v.is_infinite() || exact)
}

#[must_use]
pub fn endpoint_binary(
    f: impl FnOnce(&Float, &Float, &mut Float, Round) -> bool,
    ep1: &Endpoint,
    ep2: &Endpoint,
    out: &mut Float,
    rnd: Round,
) -> bool {
    let v1 = ep1.as_float();
    let v2 = ep2.as_float();
    let exact = f(v1, v2, out, rnd);
    (ep1.immovable && v1.is_infinite())
        || (ep2.immovable && v2.is_infinite())
        || (ep1.immovable && ep2.immovable && exact)
}

#[must_use]
pub fn endpoint_ternary(
    f: impl FnOnce(&Float, &Float, &Float, &mut Float, Round) -> bool,
    ep1: &Endpoint,
    ep2: &Endpoint,
    ep3: &Endpoint,
    out: &mut Float,
    rnd: Round,
) -> bool {
    let v1 = ep1.as_float();
    let v2 = ep2.as_float();
    let v3 = ep3.as_float();
    let exact = f(v1, v2, v3, out, rnd);
    (ep1.immovable && v1.is_infinite())
        || (ep2.immovable && v2.is_infinite())
        || (ep3.immovable && v3.is_infinite())
        || (ep1.immovable && ep2.immovable && ep3.immovable && exact)
}

#[must_use]
pub fn endpoint_minmax(
    f: impl FnOnce(&Float, &Float, &mut Float, Round) -> bool,
    ep1: &Endpoint,
    ep2: &Endpoint,
    out: &mut Float,
    rnd: Round,
) -> bool {
    let v1 = ep1.as_float();
    let v2 = ep2.as_float();
    f(v1, v2, out, rnd);
    (out == v1 && ep1.immovable) || (out == v2 && ep2.immovable)
}

/// Fix MPFR rounding bug where large values can round to infinity
/// If exponent >= 0, value is already an integer, so use rint
/// Otherwise use the specified rounding operation
fn fix_rounding_round(input: &Float, out: &mut Float, rnd: Round) -> bool {
    if mpfr_get_exp(input) >= 0 {
        mpfr_rint(input, out, rnd)
    } else {
        is_exact_operation(|| {
            let result = input.round_ref();
            out.assign(result);
        })
    }
}

fn fix_rounding_ceil(input: &Float, out: &mut Float, rnd: Round) -> bool {
    if mpfr_get_exp(input) >= 0 {
        mpfr_rint(input, out, rnd)
    } else {
        is_exact_operation(|| {
            let result = input.ceil_ref();
            out.assign(result);
        })
    }
}

fn fix_rounding_floor(input: &Float, out: &mut Float, rnd: Round) -> bool {
    if mpfr_get_exp(input) >= 0 {
        mpfr_rint(input, out, rnd)
    } else {
        is_exact_operation(|| {
            let result = input.floor_ref();
            out.assign(result);
        })
    }
}

fn fix_rounding_trunc(input: &Float, out: &mut Float, rnd: Round) -> bool {
    if mpfr_get_exp(input) >= 0 {
        mpfr_rint(input, out, rnd)
    } else {
        is_exact_operation(|| {
            let result = input.trunc_ref();
            out.assign(result);
        })
    }
}
