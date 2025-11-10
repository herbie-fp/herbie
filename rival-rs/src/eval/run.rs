//! Main evaluation loop with adaptive precision tuning

use itertools::{enumerate, izip};

use crate::eval::{
    execute,
    machine::{Discretization, Hint, Machine},
};
use crate::interval::Ival;

impl<D: Discretization> Machine<D> {
    /// Evaluate the machine with given inputs until convergence or the iteration limit
    pub fn apply(
        &mut self,
        args: &[Ival],
        hint: Option<&[Hint]>,
        max_iterations: usize,
    ) -> Result<Vec<Ival>, RivalError> {
        self.load_arguments(args);
        let hint_storage;
        let hint_slice: &[Hint] = if let Some(h) = hint {
            h
        } else {
            hint_storage = self.state.default_hint.clone();
            &hint_storage
        };

        for iteration in 0..=max_iterations {
            match self.run_iteration(iteration, hint_slice)? {
                Some(results) => return Ok(results),
                None if iteration == max_iterations => return Err(RivalError::Unsamplable),
                None => continue,
            }
        }

        Err(RivalError::Unsamplable)
    }

    /// Run a single iteration with precision tuning and hint-guided evaluation
    pub fn run_iteration(
        &mut self,
        iteration: usize,
        hints: &[Hint],
    ) -> Result<Option<Vec<Ival>>, RivalError> {
        assert_eq!(
            hints.len(),
            self.state.instructions.len(),
            "hint length mismatch"
        );
        self.state.iteration = iteration;
        self.adjust(hints);
        self.run_with_hint(hints);
        self.collect_outputs()
    }

    /// Analyze an input rectangle and return status summary, next hints, and convergence flag
    pub fn analyze_with_hints(
        &mut self,
        rect: Vec<Ival>,
        hint: Option<&[Hint]>,
    ) -> (Ival, Vec<Hint>, bool) {
        self.load_arguments(&rect);

        // Use provided hint or default
        let tmp;
        let hint_slice = if let Some(h) = hint {
            h
        } else {
            tmp = self.state.default_hint.clone();
            &tmp
        };

        // One analysis iteration at sampling iteration 0
        self.state.iteration = 0;
        self.adjust(hint_slice);
        self.run_with_hint(hint_slice);

        let (good, _done, bad, stuck) = self.return_flags();
        let (next_hint, converged) = self.make_hint(hint_slice);

        let status = Ival::bool_interval(bad || stuck, !good);
        (status, next_hint, converged)
    }

    /// Analyze a hyper-rectangle and return only the boolean interval status
    pub fn analyze(&mut self, rect: Vec<Ival>) -> Ival {
        let (status, _hint, _conv) = self.analyze_with_hints(rect, None);
        status
    }

    /// Load argument intervals into the front of the register file
    pub fn load_arguments(&mut self, args: &[Ival]) {
        assert_eq!(
            args.len(),
            self.state.arguments.len(),
            "Argument count mismatch"
        );
        for (i, arg) in args.iter().cloned().enumerate() {
            self.state.registers[i] = arg;
        }
        self.state.bumps = 0;
        self.state.bumps_activated = false;
        self.state.iteration = 0;
        self.state.precisions.fill(0);
        self.state.repeats.fill(false);
        self.state.output_distance.fill(false);
    }

    /// Execute instructions once using the supplied precision and hint plan
    fn run_with_hint(&mut self, hints: &[Hint]) {
        // On the first iteration use the initial plan; subsequent iterations use tuned state
        let (precisions, repeats) = if self.state.iteration == 0 {
            (
                &self.state.initial_precisions[..],
                &self.state.initial_repeats[..],
            )
        } else {
            (&self.state.precisions[..], &self.state.repeats[..])
        };

        for (idx, (instruction, &repeat, &precision, hint)) in
            enumerate(izip!(&self.state.instructions, repeats, precisions, hints))
        {
            if repeat {
                continue;
            }
            let out_reg = self.instruction_register(idx);

            // Hints can override execution
            match hint {
                Hint::Skip => {}
                Hint::Execute => {
                    execute::evaluate_instruction(instruction, &mut self.state.registers, precision)
                }
                // Path reduction aliasing the output of an instruction to one of its inputs
                Hint::Alias(pos) => {
                    if let Some(src_reg) = instruction.data.input_at(*pos as usize)
                        && src_reg != out_reg
                    {
                        let (src, dst) = if src_reg < out_reg {
                            let (left, right) = self.state.registers.split_at_mut(out_reg);
                            (&left[src_reg], &mut right[0])
                        } else {
                            let (left, right) = self.state.registers.split_at_mut(src_reg);
                            (&right[0], &mut left[out_reg])
                        };
                        dst.assign_from(src);
                    }
                }
                // Use pre-computed boolean value
                Hint::KnownBool(value) => {
                    self.state.registers[out_reg] = Ival::bool_interval(*value, *value);
                }
            }
        }
    }

    /// Gather outputs and translate evaluation state into convergence results
    fn collect_outputs(&mut self) -> Result<Option<Vec<Ival>>, RivalError> {
        let (good, done, bad, stuck) = self.return_flags();
        let mut outputs = Vec::with_capacity(self.state.outputs.len());

        for &root in &self.state.outputs {
            outputs.push(self.state.registers[root].clone());
        }

        if bad {
            return Err(RivalError::InvalidInput);
        }
        if done && good {
            return Ok(Some(outputs));
        }
        if stuck {
            return Err(RivalError::Unsamplable);
        }

        Ok(None)
    }

    /// Compute (good, done, bad, stuck) flags and update output_distance like Racket's rival-machine-return
    fn return_flags(&mut self) -> (bool, bool, bool, bool) {
        let mut good = true;
        let mut done = true;
        let mut bad = false;
        let mut stuck = false;

        for (idx, &root) in self.state.outputs.iter().enumerate() {
            let value = &self.state.registers[root];
            if value.err.total {
                bad = true;
            }
            if value.err.partial {
                good = false;
            }
            let lo = self.disc.convert(idx, value.lo.as_float());
            let hi = self.disc.convert(idx, value.hi.as_float());
            let dist = self.disc.distance(idx, &lo, &hi);
            self.state.output_distance[idx] = dist == 1;
            if dist != 0 {
                done = false;
                if value.lo.immovable && value.hi.immovable {
                    stuck = true;
                }
            }
        }

        (good, done, bad, stuck)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum RivalError {
    #[error("Invalid input for rival machine")]
    InvalidInput,
    #[error("Unsamplable input for rival machine")]
    Unsamplable,
}
