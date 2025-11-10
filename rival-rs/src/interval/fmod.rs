use super::value::Ival;
use crate::mpfr::{
    mpfr_add, mpfr_div, mpfr_fmod, mpfr_max, mpfr_min, mpfr_mul, mpfr_neg, mpfr_remainder,
    mpfr_round, mpfr_sign, mpfr_sub, mpfr_trunc, zero,
};
use rug::{Assign, Float, float::Round};

impl Ival {
    pub fn fmod_assign(&mut self, x: &Ival, y: &Ival) {
        let y_lo = y.lo.as_float();
        let y_hi = y.hi.as_float();
        let y_straddles_zero = (y_lo.is_sign_negative() || y_lo.is_zero())
            && (y_hi.is_sign_positive() || y_hi.is_zero());

        self.err = x.err.union(&y.err);
        if y_straddles_zero {
            self.err.partial = true;
        }
        if y_lo.is_zero() && y_hi.is_zero() {
            self.err.total = true;
        }

        let mut y_abs = Ival::zero(y.max_prec());
        y_abs.exact_fabs_assign(y);

        let x_hi = x.hi.as_float();
        let x_lo = x.lo.as_float();
        let hi_is_neg = mpfr_sign(x_hi) == -1 && !x_hi.is_zero();
        let lo_is_pos = mpfr_sign(x_lo) == 1 || x_lo.is_zero();

        if hi_is_neg {
            let mut neg_x = Ival::zero(x.max_prec());
            neg_x.exact_neg_assign(x);
            self.fmod_pos_assign(&neg_x, &y_abs);
            let tmp = self.clone();
            self.neg_assign(&tmp);
        } else if lo_is_pos {
            self.fmod_pos_assign(x, &y_abs);
        } else {
            let zero_val = zero(x.prec());
            let (neg, pos) = x.split_at(&zero_val);

            let mut neg_x = Ival::zero(neg.max_prec());
            neg_x.exact_neg_assign(&neg);

            let mut neg_result = Ival::zero(self.prec());
            let mut pos_result = Ival::zero(self.prec());

            neg_result.fmod_pos_assign(&neg_x, &y_abs);
            pos_result.fmod_pos_assign(&pos, &y_abs);

            let tmp_neg = neg_result.clone();
            neg_result.neg_assign(&tmp_neg);

            self.union_assign(pos_result);
            self.union_assign(neg_result);
        }
    }

    pub fn remainder_assign(&mut self, x: &Ival, y: &Ival) {
        let y_lo = y.lo.as_float();
        let y_hi = y.hi.as_float();
        let y_straddles_zero = (y_lo.is_sign_negative() || y_lo.is_zero())
            && (y_hi.is_sign_positive() || y_hi.is_zero());

        self.err = x.err.union(&y.err);
        if y_straddles_zero {
            self.err.partial = true;
        }
        if y_lo.is_zero() && y_hi.is_zero() {
            self.err.total = true;
        }

        let mut y_abs = Ival::zero(y.max_prec());
        y_abs.exact_fabs_assign(y);

        let x_hi = x.hi.as_float();
        let x_lo = x.lo.as_float();
        let hi_is_neg = mpfr_sign(x_hi) == -1 && !x_hi.is_zero();
        let lo_is_pos = mpfr_sign(x_lo) == 1 || x_lo.is_zero();

        if hi_is_neg {
            let mut neg_x = Ival::zero(x.max_prec());
            neg_x.exact_neg_assign(x);
            self.remainder_pos_assign(&neg_x, &y_abs);
            let tmp = self.clone();
            self.neg_assign(&tmp);
        } else if lo_is_pos {
            self.remainder_pos_assign(x, &y_abs);
        } else {
            let zero_val = zero(x.prec());
            let (neg, pos) = x.split_at(&zero_val);

            let mut neg_x = Ival::zero(neg.max_prec());
            neg_x.exact_neg_assign(&neg);

            let mut neg_result = Ival::zero(self.prec());
            let mut pos_result = Ival::zero(self.prec());

            neg_result.remainder_pos_assign(&neg_x, &y_abs);
            pos_result.remainder_pos_assign(&pos, &y_abs);

            let tmp_neg = neg_result.clone();
            neg_result.neg_assign(&tmp_neg);

            self.union_assign(pos_result);
            self.union_assign(neg_result);
        }
    }

    fn fmod_pos_assign(&mut self, x: &Ival, y: &Ival) {
        let prec = self.prec();
        let x_lo = x.lo.as_float();
        let x_hi = x.hi.as_float();
        let y_lo = y.lo.as_float();
        let y_hi = y.hi.as_float();

        let mut tmp_div = zero(prec);
        mpfr_div(x_lo, y_hi, &mut tmp_div, Round::Down);
        let mut a = zero(prec);
        mpfr_trunc(&tmp_div, &mut a, Round::Down);

        mpfr_div(x_hi, y_hi, &mut tmp_div, Round::Up);
        let mut b = zero(prec);
        mpfr_trunc(&tmp_div, &mut b, Round::Up);

        if a == b {
            mpfr_div(x_hi, y_hi, &mut tmp_div, Round::Down);
            let mut c = zero(prec);
            mpfr_trunc(&tmp_div, &mut c, Round::Down);

            mpfr_div(x_hi, y_lo, &mut tmp_div, Round::Up);
            let mut d = zero(prec);
            mpfr_trunc(&tmp_div, &mut d, Round::Up);

            if c == d {
                let mut lo = zero(prec);
                let mut hi = zero(prec);

                mpfr_fmod(x_lo, y_hi, &mut lo, Round::Down);
                mpfr_fmod(x_hi, y_lo, &mut hi, Round::Up);

                self.lo.as_float_mut().assign(&lo);
                self.lo.immovable = false;
                self.hi.as_float_mut().assign(&hi);
                self.hi.immovable = false;
            } else {
                let mut c_plus_1 = zero(prec);
                let one = Float::with_val(prec, 1);
                mpfr_add(&c, &one, &mut c_plus_1, Round::Down);

                let mut hi = zero(prec);
                mpfr_div(x_hi, &c_plus_1, &mut hi, Round::Up);

                self.lo.as_float_mut().assign(0);
                self.lo.immovable = false;
                self.hi.as_float_mut().assign(&hi);
                self.hi.immovable = false;
            }
        } else {
            self.lo.as_float_mut().assign(0);
            self.lo.immovable = false;
            self.hi.as_float_mut().assign(y_hi);
            self.hi.immovable = false;
        }
    }

    fn remainder_pos_assign(&mut self, x: &Ival, y: &Ival) {
        let prec = self.prec();
        let x_lo = x.lo.as_float();
        let x_hi = x.hi.as_float();
        let y_lo = y.lo.as_float();
        let y_hi = y.hi.as_float();

        let mut tmp_div = zero(prec);
        mpfr_div(x_lo, y_hi, &mut tmp_div, Round::Down);
        let mut a = zero(prec);
        mpfr_round(&tmp_div, &mut a, Round::Down);

        mpfr_div(x_hi, y_hi, &mut tmp_div, Round::Up);
        let mut b = zero(prec);
        mpfr_round(&tmp_div, &mut b, Round::Up);

        if a == b {
            mpfr_div(x_hi, y_hi, &mut tmp_div, Round::Down);
            let mut c = zero(prec);
            mpfr_round(&tmp_div, &mut c, Round::Down);

            mpfr_div(x_hi, y_lo, &mut tmp_div, Round::Up);
            let mut d = zero(prec);
            mpfr_round(&tmp_div, &mut d, Round::Up);

            if c == d {
                let mut lo = zero(prec);
                let mut hi = zero(prec);

                mpfr_remainder(x_lo, y_hi, &mut lo, Round::Down);
                mpfr_remainder(x_hi, y_lo, &mut hi, Round::Up);

                self.lo.as_float_mut().assign(&lo);
                self.lo.immovable = false;
                self.hi.as_float_mut().assign(&hi);
                self.hi.immovable = false;
            } else {
                let half = Float::with_val(prec, 0.5);
                let mut c_plus_half = zero(prec);
                mpfr_add(&c, &half, &mut c_plus_half, Round::Down);

                let mut tmp1 = zero(prec);
                mpfr_div(x_hi, &c_plus_half, &mut tmp1, Round::Up);

                let two = Float::with_val(prec, 2);
                let mut y_star_hi = zero(prec);
                mpfr_div(&tmp1, &two, &mut y_star_hi, Round::Up);

                let mut c_times_yhi = zero(prec);
                mpfr_mul(&c, y_hi, &mut c_times_yhi, Round::Up);

                let mut x_lo_minus_c_yhi = zero(prec);
                mpfr_sub(x_lo, &c_times_yhi, &mut x_lo_minus_c_yhi, Round::Down);

                let mut yhi_half = zero(prec);
                mpfr_div(y_hi, &two, &mut yhi_half, Round::Up);
                let mut neg_yhi_half = zero(prec);
                mpfr_neg(&yhi_half, &mut neg_yhi_half, Round::Down);

                let mut y_star_lo = zero(prec);
                mpfr_max(
                    &x_lo_minus_c_yhi,
                    &neg_yhi_half,
                    &mut y_star_lo,
                    Round::Down,
                );

                let mut neg_y_star_hi = zero(prec);
                mpfr_neg(&y_star_hi, &mut neg_y_star_hi, Round::Down);

                let mut final_lo = zero(prec);
                mpfr_min(&y_star_lo, &neg_y_star_hi, &mut final_lo, Round::Down);

                self.lo.as_float_mut().assign(&final_lo);
                self.lo.immovable = false;
                self.hi.as_float_mut().assign(&y_star_hi);
                self.hi.immovable = false;
            }
        } else {
            let two = Float::with_val(prec, 2);
            let mut y_half = zero(prec);
            mpfr_div(y_hi, &two, &mut y_half, Round::Up);

            let mut neg_y_half = zero(prec);
            mpfr_neg(&y_half, &mut neg_y_half, Round::Down);

            self.lo.as_float_mut().assign(&neg_y_half);
            self.lo.immovable = false;
            self.hi.as_float_mut().assign(&y_half);
            self.hi.immovable = false;
        }
    }
}
