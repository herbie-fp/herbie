//! Path reduction optimization logic for certain operations

use crate::{
    eval::{
        instructions::InstructionData,
        machine::{Discretization, Hint, Machine, PathOutcome},
        ops,
    },
    interval::Ival,
};

/// Mark child registers except for the output register
#[inline]
fn mark_inputs<F: FnMut(usize)>(
    out_reg: usize,
    mark: &mut F,
    regs: impl IntoIterator<Item = usize>,
) {
    for r in regs {
        if r != out_reg {
            mark(r);
        }
    }
}

/// Apply path reduction for a machine instruction and schedule dependencies
pub(super) fn path_reduction<D: Discretization, F>(
    machine: &Machine<D>,
    idx: usize,
    mut mark_child: F,
) -> PathOutcome
where
    F: FnMut(usize),
{
    let out_reg = machine.instruction_register(idx);
    let mark = |reg: usize| {
        if reg != out_reg {
            mark_child(reg);
        }
    };

    match &machine.state.instructions[idx].data {
        InstructionData::Unary { op, .. } => ops::path_reduce_unary(*op, machine, idx, mark),
        InstructionData::UnaryParam { op, .. } => {
            ops::path_reduce_unary_param(*op, machine, idx, mark)
        }
        InstructionData::Binary { op, .. } => ops::path_reduce_binary(*op, machine, idx, mark),
        InstructionData::Ternary { op, .. } => ops::path_reduce_ternary(*op, machine, idx, mark),
        InstructionData::Literal { .. }
        | InstructionData::Rational { .. }
        | InstructionData::Constant { .. } => PathOutcome::execute(true),
    }
}

/// Schedule the instruction that feeds the provided register for execution
pub(super) fn schedule_child<D: Discretization>(
    hints: &mut [Hint],
    machine: &Machine<D>,
    reg: usize,
) {
    if let Some(idx) = machine.register_to_instruction(reg)
        && !matches!(hints[idx], Hint::KnownBool(_) | Hint::Alias(_))
    {
        hints[idx] = Hint::Execute;
    }
}

/// Handle boolean operations by returning known results or marking inputs
pub fn bool_op_path_reduce<D: Discretization, F>(
    machine: &Machine<D>,
    idx: usize,
    mut mark: F,
) -> PathOutcome
where
    F: FnMut(usize),
{
    let out_reg = machine.instruction_register(idx);
    if let Some(value) = machine.state.registers[out_reg].known_bool() {
        return PathOutcome::known_bool(value);
    }

    match &machine.state.instructions[idx].data {
        InstructionData::Binary { lhs, rhs, .. } => {
            mark_inputs(out_reg, &mut mark, [*lhs, *rhs]);
            PathOutcome::execute(false)
        }
        _ => PathOutcome::execute(false),
    }
}

/// Handle not operations by reusing known outputs or marking the child
pub fn not_op_path_reduce<D: Discretization, F>(
    machine: &Machine<D>,
    idx: usize,
    mut mark: F,
) -> PathOutcome
where
    F: FnMut(usize),
{
    let out_reg = machine.instruction_register(idx);
    if let Some(value) = machine.state.registers[out_reg].known_bool() {
        return PathOutcome::known_bool(value);
    }

    match &machine.state.instructions[idx].data {
        InstructionData::Unary { arg, .. } => {
            mark_inputs(out_reg, &mut mark, [*arg]);
            PathOutcome::execute(false)
        }
        _ => PathOutcome::execute(true),
    }
}

/// Handle assert operations by aliasing known inputs or scheduling reevaluation
pub fn assert_op_path_reduce<D: Discretization, F>(
    machine: &Machine<D>,
    idx: usize,
    mut mark: F,
) -> PathOutcome
where
    F: FnMut(usize),
{
    let out_reg = machine.instruction_register(idx);
    let instr = &machine.state.instructions[idx].data;

    if let InstructionData::Unary { arg, .. } = instr {
        if let Some(value) = machine.state.registers[*arg].known_bool() {
            return PathOutcome::known_bool(value);
        }
        mark_inputs(out_reg, &mut mark, [*arg]);
        return PathOutcome::execute(false);
    }

    // Non-unary assert: treat as already converged execute
    PathOutcome::execute(true)
}

/// Handle conditional operations by aliasing known branches or marking all inputs
pub fn if_op_path_reduce<D: Discretization, F>(
    machine: &Machine<D>,
    idx: usize,
    mut mark: F,
) -> PathOutcome
where
    F: FnMut(usize),
{
    let out_reg = machine.instruction_register(idx);
    let instr = &machine.state.instructions[idx].data;

    if let InstructionData::Ternary {
        arg1, arg2, arg3, ..
    } = instr
    {
        if let Some(value) = machine.state.registers[*arg1].known_bool() {
            let (pos, reg) = if value { (1, *arg2) } else { (2, *arg3) };
            mark_inputs(out_reg, &mut mark, [reg]);
            return PathOutcome::alias(pos);
        }
        mark_inputs(out_reg, &mut mark, [*arg1, *arg2, *arg3]);
        return PathOutcome::execute(false);
    }

    PathOutcome::execute(true)
}

/// Handle min and max operations by aliasing definite comparisons or marking both inputs
pub fn minmax_path_reduce<D: Discretization, F>(
    machine: &Machine<D>,
    idx: usize,
    mut mark: F,
    prefer_lhs_when_greater: bool,
) -> PathOutcome
where
    F: FnMut(usize),
{
    let instr = &machine.state.instructions[idx].data;
    if let InstructionData::Binary { lhs, rhs, .. } = instr {
        let out_reg = machine.instruction_register(idx);
        let lhs_val = &machine.state.registers[*lhs];
        let rhs_val = &machine.state.registers[*rhs];

        let mut cmp = Ival::zero(lhs_val.prec());
        cmp.gt_assign(lhs_val, rhs_val);

        match cmp.known_bool() {
            Some(true) => {
                // lhs > rhs
                let (alias_idx, child) = if prefer_lhs_when_greater {
                    (0, *lhs)
                } else {
                    (1, *rhs)
                };
                mark_inputs(out_reg, &mut mark, [child]);
                return PathOutcome::alias(alias_idx);
            }
            Some(false) => {
                // lhs <= rhs
                let (alias_idx, child) = if prefer_lhs_when_greater {
                    (1, *rhs)
                } else {
                    (0, *lhs)
                };
                mark_inputs(out_reg, &mut mark, [child]);
                return PathOutcome::alias(alias_idx);
            }
            None => {
                // Uncertain
                mark_inputs(out_reg, &mut mark, [*lhs, *rhs]);
                return PathOutcome::execute(false);
            }
        }
    }

    PathOutcome::execute(true)
}
