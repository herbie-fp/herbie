//! Adaptive precision tuning and path reduction

pub(crate) mod path_reduction;
mod precision;

use crate::eval::{
    machine::{Discretization, Hint, Machine},
    tricks::slack_bits,
};
use path_reduction::{path_reduction, schedule_child};
use precision::{precision_tuning, update_repeats};

impl<D: Discretization> Machine<D> {
    /// Adjust precision and repeats using the backward tuning pass
    pub fn adjust(&mut self, hints: &[Hint]) {
        assert_eq!(
            hints.len(),
            self.state.instructions.len(),
            "hint length mismatch"
        );
        if self.state.iteration == 0 {
            return;
        }
        backward_pass(self, hints);
    }

    /// Compute hints indicating which instructions should be executed, skipped, or aliased
    pub fn make_hint(&self, old_hint: &[Hint]) -> (Vec<Hint>, bool) {
        let len = self.state.instructions.len();
        let mut hints = vec![Hint::Skip; len];
        let mut converged = old_hint.len() == len;

        // Roots should always be executed
        for &root in &self.state.outputs {
            if let Some(idx) = self.register_to_instruction(root) {
                hints[idx] = Hint::Execute;
            }
        }

        for idx in (0..len).rev() {
            if matches!(hints[idx], Hint::Skip) {
                continue;
            }

            if let Some(previous) = old_hint.get(idx) {
                match previous {
                    Hint::KnownBool(val) => {
                        if !matches!(&hints[idx], Hint::KnownBool(existing) if existing == val) {
                            converged = false;
                        }
                        hints[idx] = Hint::KnownBool(*val);
                        continue;
                    }
                    Hint::Alias(pos) => {
                        if let Some(reg) = self.state.instructions[idx].data.input_at(*pos as usize)
                        {
                            schedule_child(&mut hints, self, reg);
                        }
                        if !matches!(&hints[idx], Hint::Alias(p) if p == pos) {
                            converged = false;
                        }
                        hints[idx] = Hint::Alias(*pos);
                        continue;
                    }
                    Hint::Execute | Hint::Skip => {}
                }
            } else {
                converged = false;
            }

            let mut schedule = |reg: usize| schedule_child(&mut hints, self, reg);
            let outcome = path_reduction(self, idx, &mut schedule);

            converged = converged && hints[idx] == outcome.hint && outcome.converged;
            hints[idx] = outcome.hint;
        }

        (hints, converged)
    }
}

/// Compute required precision for each instruction by propagating from outputs to inputs
fn backward_pass<D: Discretization>(machine: &mut Machine<D>, hints: &[Hint]) {
    let instruction_count = machine.state.instructions.len();
    let first_tuning_pass = machine.state.iteration == 1;

    let mut new_precisions = vec![0u32; instruction_count];
    let mut work_repeats = vec![true; instruction_count];

    // Step 1: Add slack bits to outputs that hit discretization boundaries
    // Slack grows exponentially with iteration to push intervals away from boundaries
    let slack = slack_bits(machine.state.iteration, machine.state.slack_unit);
    for (&root, &boundary_issue) in machine
        .state
        .outputs
        .iter()
        .zip(machine.state.output_distance.iter())
    {
        if boundary_issue && let Some(idx) = machine.register_to_instruction(root) {
            new_precisions[idx] = new_precisions[idx].max(slack);
        }
    }

    // Step 1b: Check if reevaluation is needed

    // Mark all outputs for reevaluation
    for &root in &machine.state.outputs {
        if let Some(idx) = machine.register_to_instruction(root) {
            work_repeats[idx] = false;
        }
    }

    // Traverse instructions from outputs to inputs to mark necessary reevaluations
    for idx in (0..instruction_count).rev() {
        if work_repeats[idx] {
            continue;
        }
        let reg = &machine.state.registers[machine.instruction_register(idx)];
        if reg.lo.immovable && reg.hi.immovable {
            work_repeats[idx] = true;
            continue;
        }

        let var_count = machine.state.arguments.len();
        let mut mark = |reg: usize| {
            if reg >= var_count {
                work_repeats[reg - var_count] = false;
            }
        };
        path_reduction(machine, idx, &mut mark);
    }

    // Step 2: Precision tuning
    let mut min_bounds = vec![0u32; instruction_count];
    precision_tuning(
        machine,
        hints,
        &work_repeats,
        &mut new_precisions,
        &mut min_bounds,
    );

    // Step 3: Update repeats based on new precisions
    let mut any_reevaluation = update_repeats(
        machine,
        &mut work_repeats,
        &new_precisions,
        first_tuning_pass,
    );

    // Step 4: If no precision increase, try logspan bumps
    if !any_reevaluation {
        // Bumps mode adds precision based on interval width (logspan) rather than slack
        machine.state.bumps = machine.state.bumps.saturating_add(1);
        machine.state.bumps_activated = true;
        // Reset and recalculate precisions for bumps mode
        new_precisions.fill(0);
        work_repeats.fill(false);
        precision_tuning(
            machine,
            hints,
            &work_repeats,
            &mut new_precisions,
            &mut min_bounds,
        );
        any_reevaluation = update_repeats(
            machine,
            &mut work_repeats,
            &new_precisions,
            first_tuning_pass,
        );
        if !any_reevaluation {
            work_repeats.fill(true);
        }
    }

    // Step 5: Update machine state
    machine.state.repeats.copy_from_slice(&work_repeats);
    machine.state.precisions.copy_from_slice(&new_precisions);
}
