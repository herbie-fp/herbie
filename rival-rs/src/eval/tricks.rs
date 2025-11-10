//! Precision bound calculations for adaptive tuning

use crate::{
    eval::{
        instructions::{BinaryOp, TernaryOp, UnaryOp, UnaryParamOp},
        ops,
    },
    interval::Ival,
    mpfr::mpfr_get_exp,
};
use rug::Float;

/// Compute slack for iteration-dependent precision growth
/// Slack doubles each iteration to push results away from discretization boundaries
pub fn get_slack(iteration: usize, slack_unit: i64) -> i64 {
    if iteration == 0 || slack_unit <= 0 {
        0
    } else {
        let shift = iteration.saturating_sub(1) as u32;
        slack_unit.checked_shl(shift).unwrap_or(i64::MAX)
    }
}

/// Convert slack into a precision bit count
pub fn slack_bits(iteration: usize, slack_unit: i64) -> u32 {
    clamp_to_bits(get_slack(iteration, slack_unit))
}

/// Bounds on how many bits of precision are needed for a child instruction based on amplification analysis
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AmplBounds {
    pub upper: i64,
    pub lower: i64,
}

impl AmplBounds {
    /// Create a bounds value with explicit upper and lower growth
    pub const fn new(upper: i64, lower: i64) -> Self {
        Self { upper, lower }
    }

    /// Create a bounds value indicating no required amplification
    pub const fn zero() -> Self {
        Self::new(0, 0)
    }
}

impl Default for AmplBounds {
    fn default() -> Self {
        AmplBounds::zero()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TrickContext {
    pub iteration: usize,
    pub lower_bound_early_stopping: bool,
    pub bumps_enabled: bool,
    pub slack_unit: i64,
}

impl TrickContext {
    /// Create a context capturing tuning settings for one iteration
    pub fn new(
        iteration: usize,
        lower_bound_early_stopping: bool,
        bumps_enabled: bool,
        slack_unit: i64,
    ) -> Self {
        Self {
            iteration,
            lower_bound_early_stopping,
            bumps_enabled,
            slack_unit,
        }
    }

    /// Look up unary amplification bounds via the operation registry
    pub fn bounds_for_unary(&self, op: UnaryOp, output: &Ival, input: &Ival) -> AmplBounds {
        ops::bounds_for_unary(self, op, output, input)
    }

    /// Look up binary amplification bounds via the operation registry
    pub fn bounds_for_binary(
        &self,
        op: BinaryOp,
        output: &Ival,
        lhs: &Ival,
        rhs: &Ival,
    ) -> (AmplBounds, AmplBounds) {
        ops::bounds_for_binary(self, op, output, lhs, rhs)
    }

    /// Look up ternary amplification bounds via the operation registry
    pub fn bounds_for_ternary(
        &self,
        op: TernaryOp,
        output: &Ival,
        arg1: &Ival,
        arg2: &Ival,
        arg3: &Ival,
    ) -> (AmplBounds, AmplBounds, AmplBounds) {
        ops::bounds_for_ternary(self, op, output, arg1, arg2, arg3)
    }

    /// Look up unary parameterized amplification bounds via the operation registry
    pub fn bounds_for_unary_param(
        &self,
        op: UnaryParamOp,
        param: u64,
        output: &Ival,
        input: &Ival,
    ) -> AmplBounds {
        ops::bounds_for_unary_param(self, op, param, output, input)
    }

    /// Estimate exponent span when bump mode is enabled
    pub fn logspan(&self, value: &Ival) -> i64 {
        if !self.bumps_enabled {
            return 0;
        }
        let lo = value.lo.as_float();
        let hi = value.hi.as_float();
        if lo.is_zero() || hi.is_zero() || lo.is_infinite() || hi.is_infinite() {
            return get_slack(self.iteration, self.slack_unit);
        }
        let lo_exp = exponent(lo);
        let hi_exp = exponent(hi);
        (lo_exp - hi_exp).abs() + 1
    }

    /// Compute an upper exponent bound with optional slack reduction
    pub fn maxlog(&self, value: &Ival, less_slack: bool) -> i64 {
        let (lo, hi) = (value.lo.as_float(), value.hi.as_float());
        let slack = get_slack(self.iter_for(less_slack), self.slack_unit);
        let (lo_inf, hi_inf) = (lo.is_infinite(), hi.is_infinite());
        if lo_inf && hi_inf {
            slack
        } else if hi_inf {
            exponent(lo).max(0) + slack
        } else if lo_inf {
            exponent(hi).max(0) + slack
        } else {
            exponent(lo).max(exponent(hi)) + 1
        }
    }

    /// Compute a lower exponent bound with optional slack reduction
    pub fn minlog(&self, value: &Ival, less_slack: bool) -> i64 {
        let (lo, hi) = (value.lo.as_float(), value.hi.as_float());
        let slack = get_slack(self.iter_for(less_slack), self.slack_unit);
        let (lo_zero, hi_zero) = (lo.is_zero(), hi.is_zero());
        let (lo_inf, hi_inf) = (lo.is_infinite(), hi.is_infinite());
        if lo_zero && hi_zero {
            slack
        } else if lo_zero {
            if hi_inf {
                -slack
            } else {
                exponent(hi).min(0) - slack
            }
        } else if hi_zero {
            if lo_inf {
                -slack
            } else {
                exponent(lo).min(0) - slack
            }
        } else if crosses_zero(value) {
            if hi_inf && lo_inf {
                -slack
            } else if hi_inf {
                exponent(lo).min(0) - slack
            } else if lo_inf {
                exponent(hi).min(0) - slack
            } else {
                exponent(lo).min(exponent(hi)).min(0) - slack
            }
        } else if lo_inf {
            exponent(hi)
        } else if hi_inf {
            exponent(lo)
        } else {
            exponent(lo).min(exponent(hi))
        }
    }

    fn iter_for(&self, less_slack: bool) -> usize {
        if less_slack {
            self.iteration.saturating_sub(1)
        } else {
            self.iteration
        }
    }
}

/// Extract the MPFR exponent or return zero for zero values
pub fn exponent(value: &Float) -> i64 {
    if value.is_zero() {
        // A zero endpoint should not dominate the other non-zero endpoint.
        // Returning a conservative negative value here (instead of 0) ensures
        // maxlog ignores zero endpoints when taking a max across endpoints.
        i64::MIN / 4
    } else {
        mpfr_get_exp(value)
    }
}

/// Detect whether an interval spans zero
pub fn crosses_zero(value: &Ival) -> bool {
    let lo = value.lo.as_float();
    let hi = value.hi.as_float();
    let lo_sign = if lo.is_zero() {
        0
    } else if lo.is_sign_positive() {
        1
    } else {
        -1
    };
    let hi_sign = if hi.is_zero() {
        0
    } else if hi.is_sign_positive() {
        1
    } else {
        -1
    };
    lo_sign != hi_sign
}

/// Clamp signed growth requirements to a u32 bit count
pub fn clamp_to_bits(value: i64) -> u32 {
    if value <= 0 {
        0
    } else if value >= u32::MAX as i64 {
        u32::MAX
    } else {
        value as u32
    }
}
