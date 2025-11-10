//! Evaluator for individual instructions

use crate::eval::instructions::{Instruction, InstructionData::*};
use crate::eval::ops;
use crate::interval::Ival;
use rug::{Assign, Float, Rational as RugRational, float::Round};

/// Evaluate an instruction into its output register using the provided precision
pub fn evaluate_instruction(instruction: &Instruction, registers: &mut [Ival], precision: u32) {
    let out = instruction.out;

    // Split registers so we can mutate out_reg while reading inputs
    let (before, rest) = registers.split_at_mut(out);
    let (out_reg, after) = rest.split_first_mut().expect("Invalid register index");
    // Set the register's working precision before evaluating the instruction because all
    // registers are by default allocated to max precision
    out_reg.set_prec(precision);

    // Helper to access any register despite the split
    let get_reg = |idx: usize| -> &Ival {
        if idx < out {
            &before[idx]
        } else {
            &after[idx - out - 1]
        }
    };

    match &instruction.data {
        Literal { value } => out_reg.f64_assign(f64::from(*value)),
        // TODO: Precompute point rationals?
        Rational { num, den, neg } => {
            let prec = out_reg.prec();
            let rat = RugRational::from(((if *neg { -1 } else { 1 }) * (*num as i64), *den));
            let lo = Float::with_val_round(prec, &rat, Round::Down).0;
            let hi = Float::with_val_round(prec, &rat, Round::Up).0;
            out_reg.lo.as_float_mut().assign(&lo);
            out_reg.hi.as_float_mut().assign(&hi);
            out_reg.err = crate::interval::ErrorFlags::none();
        }
        Constant { op } => ops::execute_constant(*op, out_reg),
        Unary { op, arg } => ops::execute_unary(*op, out_reg, get_reg(*arg)),
        UnaryParam { op, param, arg } => {
            ops::execute_unary_param(*op, *param, out_reg, get_reg(*arg))
        }
        Binary { op, lhs, rhs } => ops::execute_binary(*op, out_reg, get_reg(*lhs), get_reg(*rhs)),
        Ternary {
            op,
            arg1,
            arg2,
            arg3,
        } => ops::execute_ternary(*op, out_reg, get_reg(*arg1), get_reg(*arg2), get_reg(*arg3)),
    }
}
