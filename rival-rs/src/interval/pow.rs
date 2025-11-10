//! Power interval operations

use super::value::{Endpoint, ErrorFlags, Ival, IvalClass, classify};
use crate::mpfr::{
    exp2_overflow_threshold, mpfr_get_exp, mpfr_integer, mpfr_log2, mpfr_odd, mpfr_pow, mpfr_pow2,
    mpfr_sign, zero,
};
use rug::{Assign, Float, float::Round};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PosIvalClass1 {
    GreaterOrEqual = 1,
    Less = -1,
    Straddles = 0,
}

impl Ival {
    pub fn pow_assign(&mut self, x: &Ival, y: &Ival) {
        let x_lo = x.lo.as_float();
        let x_hi = x.hi.as_float();

        let hi_is_neg = mpfr_sign(x_hi) == -1 && !x_hi.is_zero();
        let lo_is_pos = mpfr_sign(x_lo) == 1 || x_lo.is_zero();

        if hi_is_neg {
            self.pow_neg_assign(x, y);
        } else if lo_is_pos {
            self.pow_pos_assign(x, y);
        } else {
            let zero_val = zero(x.prec());
            let (neg, pos) = x.split_at(&zero_val);

            let mut neg_result = Ival::zero(self.prec());
            let mut pos_result = Ival::zero(self.prec());

            neg_result.pow_neg_assign(&neg, y);
            pos_result.pow_pos_assign(&pos, y);

            self.union_assign(neg_result);
            self.union_assign(pos_result);
        }
    }

    pub fn pow2_assign(&mut self, x: &Ival) {
        let mut abs_x = Ival::zero(x.max_prec());
        abs_x.pre_fabs_assign(x);
        self.monotonic_assign(&mpfr_pow2, &abs_x);
    }

    fn pow_pos_assign(&mut self, x: &Ival, y: &Ival) {
        let x_class = classify_pos_ival_1(x);
        let y_class = classify(y, false);

        let mk_pow =
            |out: &mut Ival, lo_a: &Endpoint, lo_b: &Endpoint, hi_a: &Endpoint, hi_b: &Endpoint| {
                let prec = out.prec();
                let mut lo_out = zero(prec);
                let mut hi_out = zero(prec);

                let lo_imm = eppow(lo_a, lo_b, x_class, y_class, &mut lo_out, Round::Down);
                let hi_imm = eppow(hi_a, hi_b, x_class, y_class, &mut hi_out, Round::Up);

                let (real_lo_imm, real_hi_imm) = if lo_out.is_zero() || hi_out.is_infinite() {
                    let threshold = exp2_overflow_threshold(prec);

                    let log2_sum_exceeds_threshold = |exp: &Float, base: &Float| -> bool {
                        let mut log2_base = zero(prec);
                        mpfr_log2(base, &mut log2_base, Round::Zero);
                        (mpfr_get_exp(exp) + mpfr_get_exp(&log2_base)) > mpfr_get_exp(&threshold)
                    };

                    let x_class_i = x_class as i32;
                    let y_class_i = y_class as i32;

                    let must_overflow = hi_out.is_infinite()
                        && x_class_i * y_class_i == 1
                        && log2_sum_exceeds_threshold(lo_b.as_float(), lo_a.as_float());

                    let must_underflow = lo_out.is_zero()
                        && x_class_i * y_class_i == -1
                        && log2_sum_exceeds_threshold(hi_b.as_float(), hi_a.as_float());

                    let new_lo_imm = lo_imm
                        || must_underflow
                        || (lo_out.is_zero() && lo_a.immovable && lo_b.immovable);

                    let new_hi_imm = hi_imm
                        || must_underflow
                        || must_overflow
                        || (hi_out.is_infinite() && hi_a.immovable && hi_b.immovable);

                    (new_lo_imm, new_hi_imm)
                } else {
                    (lo_imm, hi_imm)
                };

                out.lo.as_float_mut().assign(&lo_out);
                out.lo.immovable = real_lo_imm;
                out.hi.as_float_mut().assign(&hi_out);
                out.hi.immovable = real_hi_imm;

                let x_lo_zero = x.lo.as_float().is_zero();
                out.err = x.err.union(&y.err);
                if x_lo_zero && !matches!(y_class, IvalClass::Pos) {
                    out.err.partial = true;
                }
                if x.hi.as_float().is_zero()
                    && matches!(y_class, IvalClass::Neg)
                    && !y.hi.as_float().is_zero()
                {
                    out.err.total = true;
                }
            };

        let xlo = &x.lo;
        let xhi = &x.hi;
        let ylo = &y.lo;
        let yhi = &y.hi;

        match (x_class, y_class) {
            (PosIvalClass1::GreaterOrEqual, IvalClass::Pos) => mk_pow(self, xlo, ylo, xhi, yhi),
            (PosIvalClass1::GreaterOrEqual, IvalClass::Mix) => mk_pow(self, xhi, ylo, xhi, yhi),
            (PosIvalClass1::GreaterOrEqual, IvalClass::Neg) => mk_pow(self, xhi, ylo, xlo, yhi),
            (PosIvalClass1::Straddles, IvalClass::Pos) => mk_pow(self, xlo, yhi, xhi, yhi),
            (PosIvalClass1::Straddles, IvalClass::Neg) => mk_pow(self, xhi, ylo, xlo, ylo),
            (PosIvalClass1::Less, IvalClass::Pos) => mk_pow(self, xlo, yhi, xhi, ylo),
            (PosIvalClass1::Less, IvalClass::Mix) => mk_pow(self, xlo, yhi, xlo, ylo),
            (PosIvalClass1::Less, IvalClass::Neg) => mk_pow(self, xhi, yhi, xlo, ylo),
            (PosIvalClass1::Straddles, IvalClass::Mix) => {
                let mut tmp = self.clone();
                mk_pow(self, xlo, yhi, xhi, yhi);
                mk_pow(&mut tmp, xhi, ylo, xlo, ylo);
                self.union_assign(tmp);
            }
        }
    }

    fn pow_neg_assign(&mut self, x: &Ival, y: &Ival) {
        let y_lo = y.lo.as_float();
        let y_hi = y.hi.as_float();

        if y_lo == y_hi {
            if mpfr_integer(y_lo) {
                let mut abs_x = Ival::zero(x.max_prec());
                abs_x.exact_fabs_assign(x);

                if mpfr_odd(y_lo) {
                    self.pow_pos_assign(&abs_x, y);
                    let tmp = self.clone();
                    self.neg_assign(&tmp);
                } else {
                    self.pow_pos_assign(&abs_x, y);
                }
            } else {
                self.lo.as_float_mut().assign(f64::NAN);
                self.hi.as_float_mut().assign(f64::NAN);
                self.lo.immovable = true;
                self.hi.immovable = true;
                self.err = ErrorFlags::error();
            }
        } else {
            let mut abs_x = Ival::zero(x.max_prec());
            abs_x.exact_fabs_assign(x);

            let mut pos_pow = Ival::zero(self.prec());
            let mut neg_pow = Ival::zero(self.prec());

            pos_pow.pow_pos_assign(&abs_x, y);
            neg_pow.neg_assign(&pos_pow);

            self.union_assign(pos_pow);
            self.union_assign(neg_pow);
            self.err.partial = true;
        }
    }
}

fn classify_pos_ival_1(x: &Ival) -> PosIvalClass1 {
    let x_lo = x.lo.as_float();
    if mpfr_get_exp(x_lo) >= 1 {
        return PosIvalClass1::GreaterOrEqual;
    }

    let x_hi = x.hi.as_float();
    if mpfr_get_exp(x_hi) < 1 && !x_hi.is_infinite() {
        return PosIvalClass1::Less;
    }

    PosIvalClass1::Straddles
}

fn eppow(
    a: &Endpoint,
    b: &Endpoint,
    a_class: PosIvalClass1,
    b_class: IvalClass,
    out: &mut Float,
    rnd: Round,
) -> bool {
    let base_val = a.as_float();
    let exp_val = b.as_float();

    let base = if base_val.is_zero() {
        &zero(base_val.prec())
    } else {
        base_val
    };

    let exact = mpfr_pow(base, exp_val, out, rnd);

    let a_imm = a.immovable;
    let b_imm = b.immovable;

    (a_imm && b_imm && exact)
        || (a_imm && base == &Float::with_val(base.prec(), 1))
        || (a_imm && base.is_zero() && !matches!(b_class, IvalClass::Mix))
        || (a_imm && base.is_infinite() && !matches!(b_class, IvalClass::Mix))
        || (b_imm && exp_val.is_zero())
        || (b_imm && exp_val.is_infinite() && !matches!(a_class, PosIvalClass1::Straddles))
}
