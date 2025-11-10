//! Adaptive register machine evaluator

use std::collections::HashMap;

use super::{
    ast::Expr,
    execute,
    instructions::{Instruction, InstructionData},
};
use crate::{eval::ops, interval::Ival};
use indexmap::IndexMap;
use rug::Float;

/// Convert floating-point values to a discrete representation
pub trait Discretization: Clone {
    fn target(&self) -> u32;
    fn convert(&self, idx: usize, v: &Float) -> Float;
    fn distance(&self, idx: usize, lo: &Float, hi: &Float) -> usize;
}

/// Interval evaluation machine with persistent state and discretization
pub struct Machine<D: Discretization> {
    pub state: MachineState,
    pub disc: D,
}

/// Mutable state for machine evaluation
pub struct MachineState {
    // Program structure
    pub arguments: Vec<String>,
    pub instructions: Vec<Instruction>,
    pub outputs: Vec<usize>,

    // Initial state computed during compilation
    pub initial_repeats: Vec<bool>,
    pub initial_precisions: Vec<u32>,
    pub best_known_precisions: Vec<u32>,
    pub default_hint: Vec<Hint>,

    // Runtime state
    pub registers: Vec<Ival>,
    pub precisions: Vec<u32>,
    pub repeats: Vec<bool>,         // true = skip execution (no change needed)
    pub output_distance: Vec<bool>, // true = output near discretization boundary

    pub iteration: usize,
    pub bumps: usize, // Number of times bumps mode has been activated

    // Configuration parameters
    pub max_precision: u32,
    pub min_precision: u32,
    pub lower_bound_early_stopping: bool,
    pub slack_unit: i64,
    pub bumps_activated: bool,
    pub ampl_tuning_bits: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Hint {
    /// Instruction executes normally
    Execute,
    /// Instruction is not needed for the outputs being computed
    /// Examples include dead code or an untaken branch
    Skip,
    /// Skip execution and copy the chosen input
    /// Examples include if (true) x else y => just use x
    Alias(u8),
    /// Skip execution because the result is known exactly
    /// Examples include if (x > 0) where x = [5, 10] => result is [true, true]
    KnownBool(bool),
}

#[derive(Clone, Debug)]
pub struct PathOutcome {
    pub hint: Hint,
    pub converged: bool,
}

/// Builder for constructing a machine with custom precision parameters
pub struct MachineBuilder<D: Discretization> {
    disc: D,
    min_precision: u32,
    max_precision: u32,
    slack_unit: i64,
    base_tuning_precision: u32,
    ampl_tuning_bits: u32,
}

impl<D: Discretization> MachineBuilder<D> {
    /// Create a builder with default precision parameters
    pub fn new(disc: D) -> Self {
        Self {
            disc,
            min_precision: 20,
            max_precision: 10_000,
            slack_unit: 512,
            base_tuning_precision: 5,
            ampl_tuning_bits: 2,
        }
    }

    /// Set the minimum working precision in bits
    pub fn min_precision(mut self, v: u32) -> Self {
        self.min_precision = v;
        self
    }

    /// Set the maximum working precision in bits
    pub fn max_precision(mut self, v: u32) -> Self {
        self.max_precision = v;
        self
    }

    /// Set the slack unit used when computing slack bits
    pub fn slack_unit(mut self, v: i64) -> Self {
        self.slack_unit = v;
        self
    }

    /// Set the base tuning precision added to discretization targets
    pub fn base_tuning_precision(mut self, v: u32) -> Self {
        self.base_tuning_precision = v;
        self
    }

    /// Set the amplification tuning bits added during propagation
    pub fn ampl_tuning_bits(mut self, v: u32) -> Self {
        self.ampl_tuning_bits = v;
        self
    }

    /// Build a machine by lowering expressions into instructions and allocating state
    pub fn build(self, exprs: Vec<Expr>, vars: Vec<String>) -> Machine<D> {
        // Optimize and lower expressions to instructions
        let optimized_exprs = exprs.into_iter().map(ops::optimize_expr).collect();
        let (instructions_map, roots) = lower(optimized_exprs, &vars);
        let var_count = vars.len();
        let instruction_count = instructions_map.len();
        let register_count = var_count + instruction_count;

        let mut registers = vec![Ival::zero(self.max_precision); register_count];

        let instructions: Vec<Instruction> = instructions_map
            .into_iter()
            .map(|(data, register)| Instruction {
                out: register,
                data,
            })
            .collect();

        let mut best_known_precisions = vec![0u32; instruction_count];
        let initial_precisions = make_initial_precisions(
            &instructions,
            var_count,
            &roots,
            &self.disc,
            self.base_tuning_precision,
            self.ampl_tuning_bits,
        );

        let initial_repeats = make_initial_repeats(
            &instructions,
            var_count,
            &mut registers,
            &initial_precisions,
            &mut best_known_precisions,
        );

        let default_hint = vec![Hint::Execute; instruction_count];
        let precisions = vec![0u32; instruction_count];
        let repeats = vec![false; instruction_count];
        let output_distance = vec![false; roots.len()];

        let mut state = MachineState {
            arguments: vars,
            instructions,
            outputs: roots,
            initial_repeats,
            initial_precisions,
            best_known_precisions,
            default_hint,
            registers,
            precisions,
            repeats,
            output_distance,
            iteration: 0,
            bumps: 0,
            max_precision: self.max_precision,
            min_precision: self.min_precision,
            lower_bound_early_stopping: false,
            slack_unit: self.slack_unit,
            bumps_activated: false,
            ampl_tuning_bits: self.ampl_tuning_bits,
        };

        state.output_distance.fill(false);

        Machine {
            state,
            disc: self.disc,
        }
    }
}

impl<D: Discretization> Machine<D> {
    /// Return the instruction index that writes to the given register when applicable
    #[inline]
    pub fn register_to_instruction(&self, register: usize) -> Option<usize> {
        let var_count = self.state.arguments.len();
        if register >= var_count {
            Some(register - var_count)
        } else {
            None
        }
    }

    /// Return the register index corresponding to an instruction index
    #[inline]
    pub fn instruction_register(&self, index: usize) -> usize {
        self.state.arguments.len() + index
    }

    /// Return the root output register indices
    #[inline]
    pub fn outputs(&self) -> &[usize] {
        &self.state.outputs
    }

    /// Return the default evaluation hints
    #[inline]
    pub fn default_hint(&self) -> &[Hint] {
        &self.state.default_hint
    }

    /// Return the initial precision plan
    #[inline]
    pub fn initial_precisions(&self) -> &[u32] {
        &self.state.initial_precisions
    }

    /// Return the current precision assignments
    #[inline]
    pub fn precisions(&self) -> &[u32] {
        &self.state.precisions
    }

    /// Return how many times bump mode has run
    #[inline]
    pub fn bumps(&self) -> usize {
        self.state.bumps
    }
}

impl PathOutcome {
    /// Create an execute outcome with the given convergence status
    #[inline]
    pub fn execute(converged: bool) -> PathOutcome {
        PathOutcome {
            hint: Hint::Execute,
            converged,
        }
    }

    /// Create an alias outcome for the provided input position
    #[inline]
    pub fn alias(idx: u8) -> PathOutcome {
        PathOutcome {
            hint: Hint::Alias(idx),
            converged: true,
        }
    }

    /// Create a known boolean outcome pinned to the provided value
    #[inline]
    pub fn known_bool(value: bool) -> PathOutcome {
        PathOutcome {
            hint: Hint::KnownBool(value),
            converged: true,
        }
    }
}

/// Lower optimized expressions into instructions with common subexpression elimination
pub fn lower(exprs: Vec<Expr>, vars: &[String]) -> (IndexMap<InstructionData, usize>, Vec<usize>) {
    let mut current_reg = vars.len();
    let mut nodes: IndexMap<InstructionData, usize> = IndexMap::new();
    let var_lookup: HashMap<&str, usize> = vars
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.as_str(), idx))
        .collect();

    let roots: Vec<usize> = exprs
        .iter()
        .map(|expr| ops::lower_expr(expr, &var_lookup, &mut nodes, &mut current_reg))
        .collect();

    (nodes, roots)
}

/// Determine initial precision targets for each instruction
fn make_initial_precisions<D: Discretization>(
    instructions: &[Instruction],
    var_count: usize,
    roots: &[usize],
    disc: &D,
    base_tuning_precision: u32,
    ampl_tuning_bits: u32,
) -> Vec<u32> {
    let mut precisions = vec![0u32; instructions.len()];

    // Initialize output nodes to target + base precision
    for &root in roots.iter() {
        if root >= var_count {
            precisions[root - var_count] = disc.target() + base_tuning_precision;
        }
    }

    // Propagate precisions backward through the computation graph
    for idx in (0..instructions.len()).rev() {
        let current_prec = precisions[idx];
        instructions[idx].for_each_input(|reg| {
            if reg >= var_count {
                let input_idx = reg - var_count;
                if input_idx != idx {
                    precisions[input_idx] =
                        precisions[input_idx].max(current_prec + ampl_tuning_bits);
                }
            }
        });
    }

    precisions
}

/// Evaluate and mark constant-only nodes that can skip future execution
fn make_initial_repeats(
    instructions: &[Instruction],
    var_count: usize,
    registers: &mut [Ival],
    initial_precisions: &[u32],
    best_known_precisions: &mut [u32],
) -> Vec<bool> {
    let mut initial_repeats = vec![true; instructions.len()];

    for (idx, (instr, &prec)) in instructions.iter().zip(initial_precisions).enumerate() {
        let mut depends = false;
        instr.data.for_each_input(|reg| {
            let child = reg as isize - var_count as isize;
            if child == idx as isize {
                return;
            }
            if child < 0 || !initial_repeats[child as usize] {
                depends = true;
            }
        });

        if depends {
            initial_repeats[idx] = false;
        } else {
            execute::evaluate_instruction(instr, registers, prec);
            best_known_precisions[idx] = prec;
        }
    }

    initial_repeats
}
