//! Wrappers around MPFR functions using the `rug` and `gmp_mpfr_sys` crates.
//! Uses unsafe functions for ease of implementation, and in cases such as
//! `exp_overflow_threshold`, for efficiency
use gmp_mpfr_sys::mpfr;
use rug::Float;
use rug::float::Round;

fn to_mpfr_round(rnd: Round) -> mpfr::rnd_t {
    match rnd {
        Round::Down => mpfr::rnd_t::RNDD,
        Round::Up => mpfr::rnd_t::RNDU,
        Round::Zero => mpfr::rnd_t::RNDZ,
        Round::Nearest => mpfr::rnd_t::RNDN,
        _ => mpfr::rnd_t::RNDN,
    }
}

/// Returns true if the MPFR operation was exact (no rounding error); not thread safe
pub fn is_exact_operation<F>(f: F) -> bool
where
    F: FnOnce(),
{
    unsafe {
        mpfr::clear_inexflag();
    }
    f();
    unsafe { mpfr::inexflag_p() == 0 }
}

macro_rules! mpfr_unary_op {
    ($name:ident, $func:path) => {
        pub fn $name(input: &Float, out: &mut Float, rnd: Round) -> bool {
            is_exact_operation(|| unsafe {
                $func(out.as_raw_mut(), input.as_raw(), to_mpfr_round(rnd));
            })
        }
    };
}

macro_rules! mpfr_binary_op {
    ($name:ident, $func:path) => {
        pub fn $name(lhs: &Float, rhs: &Float, out: &mut Float, rnd: Round) -> bool {
            is_exact_operation(|| unsafe {
                $func(
                    out.as_raw_mut(),
                    lhs.as_raw(),
                    rhs.as_raw(),
                    to_mpfr_round(rnd),
                );
            })
        }
    };
}

macro_rules! mpfr_ternary_op {
    ($name:ident, $func:path) => {
        pub fn $name(a: &Float, b: &Float, c: &Float, out: &mut Float, rnd: Round) -> bool {
            is_exact_operation(|| unsafe {
                $func(
                    out.as_raw_mut(),
                    a.as_raw(),
                    b.as_raw(),
                    c.as_raw(),
                    to_mpfr_round(rnd),
                );
            })
        }
    };
}

// Mutating operations

// Basic operations
mpfr_unary_op!(mpfr_neg, mpfr::neg);
mpfr_unary_op!(mpfr_abs, mpfr::abs);

// Roots and powers
mpfr_unary_op!(mpfr_sqrt, mpfr::sqrt);
mpfr_unary_op!(mpfr_cbrt, mpfr::cbrt);
mpfr_unary_op!(mpfr_pow2, mpfr::sqr);

// Exponential functions
mpfr_unary_op!(mpfr_exp, mpfr::exp);
mpfr_unary_op!(mpfr_exp2, mpfr::exp2);
mpfr_unary_op!(mpfr_expm1, mpfr::expm1);

// Logarithmic functions
mpfr_unary_op!(mpfr_log, mpfr::log);
mpfr_unary_op!(mpfr_log2, mpfr::log2);
mpfr_unary_op!(mpfr_log10, mpfr::log10);
mpfr_unary_op!(mpfr_log1p, mpfr::log1p);

// Trigonometric functions
mpfr_unary_op!(mpfr_sin, mpfr::sin);
mpfr_unary_op!(mpfr_cos, mpfr::cos);
mpfr_unary_op!(mpfr_tan, mpfr::tan);
mpfr_unary_op!(mpfr_asin, mpfr::asin);
mpfr_unary_op!(mpfr_acos, mpfr::acos);
mpfr_unary_op!(mpfr_atan, mpfr::atan);

// Hyperbolic functions
mpfr_unary_op!(mpfr_sinh, mpfr::sinh);
mpfr_unary_op!(mpfr_cosh, mpfr::cosh);
mpfr_unary_op!(mpfr_tanh, mpfr::tanh);
mpfr_unary_op!(mpfr_asinh, mpfr::asinh);
mpfr_unary_op!(mpfr_acosh, mpfr::acosh);
mpfr_unary_op!(mpfr_atanh, mpfr::atanh);

// Error functions
mpfr_unary_op!(mpfr_erf, mpfr::erf);
mpfr_unary_op!(mpfr_erfc, mpfr::erfc);

// Rounding functions
mpfr_unary_op!(mpfr_rint, mpfr::rint);

// Binary operations
mpfr_binary_op!(mpfr_add, mpfr::add);
mpfr_binary_op!(mpfr_sub, mpfr::sub);
mpfr_binary_op!(mpfr_mul, mpfr::mul);
mpfr_binary_op!(mpfr_div, mpfr::div);
mpfr_binary_op!(mpfr_min, mpfr::min);
mpfr_binary_op!(mpfr_max, mpfr::max);
mpfr_binary_op!(mpfr_atan2, mpfr::atan2);
mpfr_binary_op!(mpfr_copysign, mpfr::copysign);
mpfr_binary_op!(mpfr_pow, mpfr::pow);
mpfr_binary_op!(mpfr_hypot, mpfr::hypot);
mpfr_binary_op!(mpfr_fmod, mpfr::fmod);
mpfr_binary_op!(mpfr_remainder, mpfr::remainder);

// Ternary operations
mpfr_ternary_op!(mpfr_fma, mpfr::fma);

pub fn mpfr_cosu(x: &Float, n: u64, out: &mut Float, rnd: Round) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::cosu(out.as_raw_mut(), x.as_raw(), n, to_mpfr_round(rnd));
    })
}

pub fn mpfr_sinu(x: &Float, n: u64, out: &mut Float, rnd: Round) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::sinu(out.as_raw_mut(), x.as_raw(), n, to_mpfr_round(rnd));
    })
}

pub fn mpfr_tanu(x: &Float, n: u64, out: &mut Float, rnd: Round) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::tanu(out.as_raw_mut(), x.as_raw(), n, to_mpfr_round(rnd));
    })
}

pub fn mpfr_pi(out: &mut Float, rnd: Round) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::const_pi(out.as_raw_mut(), to_mpfr_round(rnd));
    })
}

pub fn mpfr_integer(x: &Float) -> bool {
    unsafe { mpfr::integer_p(x.as_raw()) != 0 }
}

pub fn mpfr_even(x: &Float) -> bool {
    if !mpfr_integer(x) {
        return false;
    }
    let mut half = Float::with_val(x.prec(), x);
    unsafe {
        mpfr::mul_2si(
            half.as_raw_mut(),
            half.as_raw(),
            -1,
            to_mpfr_round(Round::Nearest),
        );
    }
    mpfr_integer(&half)
}

pub fn mpfr_odd(x: &Float) -> bool {
    mpfr_integer(x) && !mpfr_even(x)
}

pub fn mpfr_e(out: &mut Float, rnd: Round) -> bool {
    mpfr_exp(&Float::with_val(out.prec(), 1), out, rnd)
}

pub fn mpfr_get_exp(x: &Float) -> i64 {
    unsafe { mpfr::get_exp(x.as_raw()) as i64 }
}

pub fn mpfr_floor_inplace(x: &mut Float) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::floor(x.as_raw_mut(), x.as_raw());
    })
}

pub fn mpfr_floor(input: &Float, out: &mut Float, _rnd: Round) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::floor(out.as_raw_mut(), input.as_raw());
    })
}

pub fn mpfr_ceil_inplace(x: &mut Float) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::ceil(x.as_raw_mut(), x.as_raw());
    })
}

pub fn mpfr_ceil(input: &Float, out: &mut Float, _rnd: Round) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::ceil(out.as_raw_mut(), input.as_raw());
    })
}

pub fn mpfr_round_inplace(x: &mut Float) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::round(x.as_raw_mut(), x.as_raw());
    })
}

pub fn mpfr_round(input: &Float, out: &mut Float, _rnd: Round) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::round(out.as_raw_mut(), input.as_raw());
    })
}

pub fn mpfr_trunc_inplace(x: &mut Float) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::trunc(x.as_raw_mut(), x.as_raw());
    })
}

pub fn mpfr_trunc(input: &Float, out: &mut Float, _rnd: Round) -> bool {
    is_exact_operation(|| unsafe {
        mpfr::trunc(out.as_raw_mut(), input.as_raw());
    })
}

pub fn mpfr_cmpabs(x: &Float, y: &Float) -> i32 {
    unsafe { mpfr::cmpabs(x.as_raw(), y.as_raw()) }
}

pub fn mpfr_sign(x: &Float) -> i32 {
    unsafe { mpfr::sgn(x.as_raw()) }
}

pub fn mpfr_nextbelow(out: &mut Float) {
    unsafe {
        mpfr::nextbelow(out.as_raw_mut());
    }
}

pub fn mpfr_nextabove(out: &mut Float) {
    unsafe {
        mpfr::nextabove(out.as_raw_mut());
    }
}

// Non-mutating functions
// TODO: Consider some kind of caching for these

pub fn zero(prec: u32) -> Float {
    Float::with_val(prec, 0)
}

pub fn inf(prec: u32) -> Float {
    Float::with_val(prec, f64::INFINITY)
}

pub fn exp_overflow_threshold(prec: u32) -> Float {
    let mut threshold = Float::with_val(prec, f64::INFINITY);
    unsafe {
        mpfr::nextbelow(threshold.as_raw_mut());
        mpfr::log(
            threshold.as_raw_mut(),
            threshold.as_raw(),
            to_mpfr_round(Round::Nearest),
        );
        mpfr::add(
            threshold.as_raw_mut(),
            threshold.as_raw(),
            Float::with_val(prec, 1.0).as_raw(),
            to_mpfr_round(Round::Nearest),
        );
    }
    threshold
}

pub fn exp2_overflow_threshold(prec: u32) -> Float {
    let mut threshold = Float::with_val(prec, f64::INFINITY);
    unsafe {
        mpfr::nextbelow(threshold.as_raw_mut());
        mpfr::log2(
            threshold.as_raw_mut(),
            threshold.as_raw(),
            to_mpfr_round(Round::Nearest),
        );
        mpfr::add(
            threshold.as_raw_mut(),
            threshold.as_raw(),
            Float::with_val(prec, 1.0).as_raw(),
            to_mpfr_round(Round::Nearest),
        );
    }
    threshold
}

pub fn sinh_overflow_threshold(prec: u32) -> Float {
    let mut threshold = Float::with_val(prec, f64::INFINITY);
    unsafe {
        mpfr::nextbelow(threshold.as_raw_mut());
        mpfr::asinh(
            threshold.as_raw_mut(),
            threshold.as_raw(),
            to_mpfr_round(Round::Nearest),
        );
        mpfr::add(
            threshold.as_raw_mut(),
            threshold.as_raw(),
            Float::with_val(prec, 1.0).as_raw(),
            to_mpfr_round(Round::Nearest),
        );
    }
    threshold
}

pub fn acosh_overflow_threshold(prec: u32) -> Float {
    let mut threshold = Float::with_val(prec, f64::INFINITY);
    unsafe {
        mpfr::nextbelow(threshold.as_raw_mut());
        mpfr::acosh(
            threshold.as_raw_mut(),
            threshold.as_raw(),
            to_mpfr_round(Round::Nearest),
        );
        mpfr::add(
            threshold.as_raw_mut(),
            threshold.as_raw(),
            Float::with_val(prec, 1.0).as_raw(),
            to_mpfr_round(Round::Nearest),
        );
    }
    threshold
}
