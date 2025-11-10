use super::value::{Endpoint, Ival, IvalClass, classify};
use crate::{
    interval::core::endpoint_binary,
    mpfr::{mpfr_add, mpfr_div, mpfr_hypot, mpfr_mul, mpfr_sub},
};
use rug::{Assign, Float, float::Round, ops::AssignRound};

impl Ival {
    pub fn add_assign(&mut self, a: &Ival, b: &Ival) {
        self.lo.immovable =
            endpoint_binary(mpfr_add, &a.lo, &b.lo, self.lo.as_float_mut(), Round::Down);
        self.hi.immovable =
            endpoint_binary(mpfr_add, &a.hi, &b.hi, self.hi.as_float_mut(), Round::Up);
        self.err = a.err.union(&b.err);
    }

    pub fn sub_assign(&mut self, a: &Ival, b: &Ival) {
        self.lo.immovable =
            endpoint_binary(mpfr_sub, &a.lo, &b.hi, self.lo.as_float_mut(), Round::Down);
        self.hi.immovable =
            endpoint_binary(mpfr_sub, &a.hi, &b.lo, self.hi.as_float_mut(), Round::Up);
        self.err = a.err.union(&b.err);
    }

    pub fn mul_assign(&mut self, a: &Ival, b: &Ival) {
        let class_a = classify(a, false);
        let class_b = classify(b, false);
        let err = a.err.union(&b.err);

        let mkmul =
            |out: &mut Ival, lo_a: &Endpoint, lo_b: &Endpoint, hi_a: &Endpoint, hi_b: &Endpoint| {
                out.lo.immovable = epmul(
                    lo_a,
                    lo_b,
                    class_a,
                    class_b,
                    out.lo.as_float_mut(),
                    Round::Down,
                );
                out.hi.immovable = epmul(
                    hi_a,
                    hi_b,
                    class_a,
                    class_b,
                    out.hi.as_float_mut(),
                    Round::Up,
                );
                out.err = err;
            };

        match (class_a, class_b) {
            (IvalClass::Pos, IvalClass::Pos) => mkmul(self, &a.lo, &b.lo, &a.hi, &b.hi),
            (IvalClass::Pos, IvalClass::Neg) => mkmul(self, &a.hi, &b.lo, &a.lo, &b.hi),
            (IvalClass::Pos, IvalClass::Mix) => mkmul(self, &a.hi, &b.lo, &a.hi, &b.hi),
            (IvalClass::Neg, IvalClass::Pos) => mkmul(self, &a.lo, &b.hi, &a.hi, &b.lo),
            (IvalClass::Neg, IvalClass::Neg) => mkmul(self, &a.hi, &b.hi, &a.lo, &b.lo),
            (IvalClass::Neg, IvalClass::Mix) => mkmul(self, &a.lo, &b.hi, &a.lo, &b.lo),
            (IvalClass::Mix, IvalClass::Pos) => mkmul(self, &a.lo, &b.hi, &a.hi, &b.hi),
            (IvalClass::Mix, IvalClass::Neg) => mkmul(self, &a.hi, &b.lo, &a.lo, &b.lo),
            (IvalClass::Mix, IvalClass::Mix) => {
                let mut tmp_ival = self.clone();
                mkmul(self, &a.hi, &b.lo, &a.lo, &b.lo);
                mkmul(&mut tmp_ival, &a.lo, &b.hi, &a.hi, &b.hi);
                self.union_assign(tmp_ival);
            }
        }
    }

    pub fn div_assign(&mut self, a: &Ival, b: &Ival) {
        let class_a = classify(a, true);
        let class_b = classify(b, true);

        let y_lo = b.lo.as_float();
        let y_hi = b.hi.as_float();

        self.err = a.err.union(&b.err);
        if (y_lo.is_zero() || y_lo.is_sign_negative())
            && (y_hi.is_zero() || y_hi.is_sign_positive())
        {
            self.err.partial = true;
        }
        if y_lo.is_zero() && y_hi.is_zero() {
            self.err.total = true;
            self.err.partial = true;
        }

        if matches!(class_b, IvalClass::Mix) {
            let immovable = (b.lo.immovable && y_lo.is_zero())
                || (b.hi.immovable && y_hi.is_zero())
                || (b.lo.immovable && b.hi.immovable);
            self.lo.as_float_mut().assign(f64::NEG_INFINITY);
            self.hi.as_float_mut().assign(f64::INFINITY);
            self.lo.immovable = immovable;
            self.hi.immovable = immovable;
            return;
        }

        let mut mkdiv = |num_lo: &Endpoint,
                         den_lo: &Endpoint,
                         num_hi: &Endpoint,
                         den_hi: &Endpoint| {
            self.lo.immovable = epdiv(num_lo, den_lo, class_a, self.lo.as_float_mut(), Round::Down);
            self.hi.immovable = epdiv(num_hi, den_hi, class_a, self.hi.as_float_mut(), Round::Up);
        };

        match (class_a, class_b) {
            (_, IvalClass::Mix) => unreachable!(),
            (IvalClass::Pos, IvalClass::Pos) => mkdiv(&a.lo, &b.hi, &a.hi, &b.lo),
            (IvalClass::Pos, IvalClass::Neg) => mkdiv(&a.hi, &b.hi, &a.lo, &b.lo),
            (IvalClass::Neg, IvalClass::Pos) => mkdiv(&a.lo, &b.lo, &a.hi, &b.hi),
            (IvalClass::Neg, IvalClass::Neg) => mkdiv(&a.hi, &b.lo, &a.lo, &b.hi),
            (IvalClass::Mix, IvalClass::Pos) => mkdiv(&a.lo, &b.lo, &a.hi, &b.lo),
            (IvalClass::Mix, IvalClass::Neg) => mkdiv(&a.hi, &b.hi, &a.lo, &b.hi),
        }
    }

    pub fn fma_assign(&mut self, a: &Ival, b: &Ival, c: &Ival) {
        let mut product = Ival::zero(self.prec());
        product.mul_assign(a, b);
        self.add_assign(&product, c);
    }

    pub fn fdim_assign(&mut self, x: &Ival, y: &Ival) {
        let mut diff = Ival::zero(self.prec());
        diff.sub_assign(x, y);
        let zero_ival = Ival::zero(self.prec());
        self.fmax_assign(&diff, &zero_ival);
    }

    pub fn hypot_assign(&mut self, x: &Ival, y: &Ival) {
        let mut abs_x = Ival::zero(self.prec());
        let mut abs_y = Ival::zero(self.prec());
        abs_x.pre_fabs_assign(x);
        abs_y.pre_fabs_assign(y);

        self.lo.immovable = endpoint_binary(
            mpfr_hypot,
            &abs_x.lo,
            &abs_y.lo,
            self.lo.as_float_mut(),
            Round::Down,
        );
        self.hi.immovable = endpoint_binary(
            mpfr_hypot,
            &abs_x.hi,
            &abs_y.hi,
            self.hi.as_float_mut(),
            Round::Up,
        );
        self.err = x.err.union(&y.err);
    }
}

fn epmul(
    ep1: &Endpoint,
    ep2: &Endpoint,
    class1: IvalClass,
    class2: IvalClass,
    out: &mut Float,
    rnd: Round,
) -> bool {
    let a = ep1.as_float();
    let b = ep2.as_float();

    if a.is_zero() || b.is_zero() {
        out.assign_round(0.0, Round::Nearest);
        return (ep1.immovable && a.is_zero())
            || (ep2.immovable && b.is_zero())
            || (ep1.immovable && ep2.immovable);
    }

    let exact = mpfr_mul(a, b, out, rnd);
    (ep1.immovable && ep2.immovable && exact)
        || (ep1.immovable && a.is_infinite() && !matches!(class2, IvalClass::Mix))
        || (ep2.immovable && b.is_infinite() && !matches!(class1, IvalClass::Mix))
}

fn epdiv(ep1: &Endpoint, ep2: &Endpoint, class: IvalClass, out: &mut Float, rnd: Round) -> bool {
    let a = ep1.as_float();
    let b = ep2.as_float();

    let exact = mpfr_div(a, b, out, rnd);

    (ep1.immovable && ep2.immovable && exact)
        || (ep1.immovable && (a.is_zero() || a.is_infinite()))
        || (ep2.immovable && (b.is_infinite() || (b.is_zero() && !matches!(class, IvalClass::Mix))))
}
