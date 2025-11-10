//! Precision tuning logic using amplification bounds

use crate::eval::{
    instructions::InstructionData,
    machine::{Discretization, Hint, Machine},
    tricks::{AmplBounds, TrickContext, clamp_to_bits},
};
use itertools::{enumerate, izip};

/// Compute precision targets for instructions based on amplification bounds and hints
pub(super) fn precision_tuning<D: Discretization>(
    machine: &Machine<D>,
    hints: &[Hint],
    repeats: &[bool],
    new_precisions: &mut [u32],
    min_bounds: &mut [u32],
) {
    let ctx = TrickContext::new(
        machine.state.iteration,
        machine.state.lower_bound_early_stopping,
        machine.state.bumps_activated,
        machine.state.slack_unit,
    );

    for idx in (0..machine.state.instructions.len()).rev() {
        if repeats[idx] || matches!(hints[idx], Hint::Skip) {
            continue;
        }

        let instruction = &machine.state.instructions[idx];
        let output = &machine.state.registers[machine.instruction_register(idx)];
        let parent_upper = new_precisions[idx];
        let parent_lower = min_bounds[idx];
        let base_precision = machine.state.initial_precisions[idx];

        new_precisions[idx] = base_precision
            .saturating_add(parent_upper)
            .clamp(machine.state.min_precision, machine.state.max_precision);

        // Propagate precision requirements to children based on error amplification
        match &instruction.data {
            InstructionData::Literal { .. }
            | InstructionData::Rational { .. }
            | InstructionData::Constant { .. } => {}
            InstructionData::Unary { op, arg } => {
                let bounds = ctx.bounds_for_unary(*op, output, &machine.state.registers[*arg]);
                propagate_child(
                    machine,
                    *arg,
                    bounds,
                    parent_upper,
                    parent_lower,
                    new_precisions,
                    min_bounds,
                );
            }
            InstructionData::UnaryParam { op, param, arg } => {
                let bounds =
                    ctx.bounds_for_unary_param(*op, *param, output, &machine.state.registers[*arg]);
                propagate_child(
                    machine,
                    *arg,
                    bounds,
                    parent_upper,
                    parent_lower,
                    new_precisions,
                    min_bounds,
                );
            }
            InstructionData::Binary { op, lhs, rhs } => {
                let (lhs_bounds, rhs_bounds) = ctx.bounds_for_binary(
                    *op,
                    output,
                    &machine.state.registers[*lhs],
                    &machine.state.registers[*rhs],
                );
                propagate_child(
                    machine,
                    *lhs,
                    lhs_bounds,
                    parent_upper,
                    parent_lower,
                    new_precisions,
                    min_bounds,
                );
                propagate_child(
                    machine,
                    *rhs,
                    rhs_bounds,
                    parent_upper,
                    parent_lower,
                    new_precisions,
                    min_bounds,
                );
            }
            InstructionData::Ternary {
                op,
                arg1,
                arg2,
                arg3,
            } => {
                let (bounds1, bounds2, bounds3) = ctx.bounds_for_ternary(
                    *op,
                    output,
                    &machine.state.registers[*arg1],
                    &machine.state.registers[*arg2],
                    &machine.state.registers[*arg3],
                );
                propagate_child(
                    machine,
                    *arg1,
                    bounds1,
                    parent_upper,
                    parent_lower,
                    new_precisions,
                    min_bounds,
                );
                propagate_child(
                    machine,
                    *arg2,
                    bounds2,
                    parent_upper,
                    parent_lower,
                    new_precisions,
                    min_bounds,
                );
                propagate_child(
                    machine,
                    *arg3,
                    bounds3,
                    parent_upper,
                    parent_lower,
                    new_precisions,
                    min_bounds,
                );
            }
        }
    }
}

/// Update a child register with propagated precision requirements
fn propagate_child<D: Discretization>(
    machine: &Machine<D>,
    child_reg: usize,
    bounds: AmplBounds,
    parent_upper: u32,
    parent_lower: u32,
    new_precisions: &mut [u32],
    min_bounds: &mut [u32],
) {
    if let Some(child_idx) = machine.register_to_instruction(child_reg) {
        // TODO: We actually don't need clamp_to_bits here-- we can simply cast as u32 because we
        // assume that all stored precisions are positive and a cast won't overflow; see if there's
        // any noticeable performance difference if we don't use clamp_to_bits (likely not significant)
        new_precisions[child_idx] = clamp_to_bits(
            (new_precisions[child_idx] as i64)
                .max((parent_upper as i64).saturating_add(bounds.upper)),
        );
        min_bounds[child_idx] = clamp_to_bits(
            (min_bounds[child_idx] as i64).max((parent_lower as i64).saturating_add(bounds.upper)),
        );
    }
}

/// Mark instructions that can skip reevaluation and report whether any work remains
pub(super) fn update_repeats<D: Discretization>(
    machine: &mut Machine<D>,
    repeats: &mut [bool],
    new_precisions: &[u32],
    first_tuning_pass: bool,
) -> bool {
    let mut any_reevaluation = false;

    let old_precisions: &[u32] = if first_tuning_pass {
        &machine.state.initial_precisions
    } else {
        &machine.state.precisions
    };

    for (idx, (instr, &new_precision, &constant)) in enumerate(izip!(
        &machine.state.instructions,
        new_precisions,
        &machine.state.initial_repeats
    )) {
        if repeats[idx] {
            continue;
        }

        let old_precision = old_precisions[idx];
        let reference = if constant {
            machine.state.best_known_precisions[idx]
        } else {
            old_precision
        };

        // Recompute if precision increases or if any child recomputes
        let self_reg = machine.instruction_register(idx);
        let mut children_repeat = true;
        instr.data.for_each_input(|reg| {
            if reg != self_reg
                && let Some(child_idx) = machine.register_to_instruction(reg)
                && !repeats[child_idx]
            {
                children_repeat = false;
            }
        });

        let precision_has_increased = new_precision > reference;
        if precision_has_increased || !children_repeat {
            any_reevaluation = true;
            if constant && precision_has_increased {
                machine.state.best_known_precisions[idx] = new_precision;
            }
        } else {
            repeats[idx] = true;
        }
    }

    any_reevaluation
}
