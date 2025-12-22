use gmp_mpfr_sys::mpfr::{self, mpfr_t};
use rival::eval::machine::Hint;
use rival::{Discretization, ErrorFlags, Ival, Machine, MachineBuilder, RivalError};
use rug::Float;
use std::collections::HashMap;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::panic;
use std::ptr;
use std::slice;

mod discretization;
mod parser;
use discretization::{DiscretizationType, RustDiscretization};
use parser::{parse_expr, parse_sexpr, SExpr};

pub struct MachineWrapper {
    machine: Machine<RustDiscretization>,
    arg_buf: Vec<Ival>,
    rect_buf: Vec<Ival>,
    /// Cached execution records for FFI transfer
    execution_cache: Vec<ExecutionRecord>,
    /// Cached null-separated instruction names for FFI transfer
    instruction_names_cache: Vec<u8>,
    /// Cached aggregated profile entries
    aggregated_profile_cache: Vec<AggregatedProfileEntry>,
    /// Precision bucket size for aggregation (max_precision / 25)
    precision_bucket_size: u32,
}

pub struct HintsHandle {
    hints: Vec<Hint>,
}

#[repr(C)]
pub struct ExecutionRecord {
    pub name_ptr: *const u8,
    pub name_len: usize,
    pub number: i32,
    pub precision: u32,
    pub time_ms: f64,
    pub iteration: usize,
}

#[repr(C)]
pub struct AggregatedProfileEntry {
    /// Instruction index (>= 0) or -1 for adjust
    pub instruction_index: i32,
    /// Precision rounded to bucket
    pub precision_bucket: u32,
    /// Sum of time_ms for all executions in this bucket
    pub total_time_ms: f64,
    /// Sum of memory for all executions in this bucket
    pub total_memory: usize,
}

/// Result of rival_apply with all profiling data bundled
#[repr(C)]
pub struct ApplyResult {
    /// 0 = success, -1 = invalid, -2 = unsamplable, -99 = panic
    pub status_code: i32,
    /// Number of precision bumps during evaluation
    pub bumps: usize,
    /// Number of iterations used
    pub iterations: usize,
    /// Pointer to aggregated profile entries
    pub profile_ptr: *const AggregatedProfileEntry,
    /// Number of aggregated profile entries
    pub profile_len: usize,
}

#[repr(C)]
pub struct ProfileData {
    pub instructions_len: usize,
    pub iterations: usize,
    pub bumps: usize,
    pub executions_ptr: *const ExecutionRecord,
    pub executions_len: usize,
}

#[repr(C)]
pub struct AnalyzeResult {
    /// 0 = success, -1 = panic
    pub status_code: i32,
    pub is_error: bool,
    pub maybe_error: bool,
    pub converged: bool,
    /// Caller must free with rival_hints_destroy
    pub hints: *mut HintsHandle,
}

#[repr(u32)]
pub enum ProfileField {
    Instructions = 0,
    Iterations = 1,
    Bumps = 2,
    Executions = 3,
}

#[no_mangle]
pub extern "C" fn rival_compile(
    exprs_str: *const c_char,
    vars_str: *const c_char,
    target: u32,
    types_ptr: *const u32,
    num_types: u32,
    max_precision: u32,
    profile_capacity: usize,
) -> *mut MachineWrapper {
    let result = panic::catch_unwind(|| {
        let exprs_str = unsafe { CStr::from_ptr(exprs_str).to_string_lossy() };
        let vars_str = unsafe { CStr::from_ptr(vars_str).to_string_lossy() };

        let vars: Vec<String> = vars_str.split_whitespace().map(|s| s.to_string()).collect();

        let sexpr = parse_sexpr(&exprs_str).expect("Failed to parse exprs sexpr");
        let exprs = match sexpr {
            SExpr::List(list) => list
                .into_iter()
                .map(|s| parse_expr(&s).expect("Failed to parse expr"))
                .collect(),
            _ => panic!("Expected list of expressions"),
        };

        let types_slice = unsafe { slice::from_raw_parts(types_ptr, num_types as usize) };
        let types = types_slice
            .iter()
            .map(|&t| match t {
                0 => DiscretizationType::Bool,
                1 => DiscretizationType::F32,
                2 => DiscretizationType::F64,
                _ => panic!("Unknown discretization type: {}", t),
            })
            .collect();

        let disc = RustDiscretization { target, types };
        let num_args = vars.len();
        let machine = MachineBuilder::new(disc)
            .profile_capacity(profile_capacity)
            .max_precision(max_precision)
            .build(exprs, vars);

        let arg_prec = machine.disc.target().max(machine.min_precision);

        let arg_buf = (0..num_args)
            .map(|_| Ival::from_lo_hi(Float::new(arg_prec), Float::new(arg_prec)))
            .collect();
        let rect_buf = (0..num_args)
            .map(|_| Ival::from_lo_hi(Float::new(arg_prec), Float::new(arg_prec)))
            .collect();

        let precision_bucket_size = max_precision / 25;
        let wrapper = MachineWrapper {
            machine,
            arg_buf,
            rect_buf,
            execution_cache: Vec::new(),
            instruction_names_cache: Vec::new(),
            aggregated_profile_cache: Vec::new(),
            precision_bucket_size: if precision_bucket_size > 0 { precision_bucket_size } else { 1 },
        };
        Box::into_raw(Box::new(wrapper))
    });

    match result {
        Ok(ptr) => ptr,
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_apply(
    ptr: *mut MachineWrapper,
    args_ptrs: *const *const mpfr_t,
    args_len: usize,
    out_ptrs: *const *mut mpfr_t,
    out_len: usize,
    hints_ptr: *const HintsHandle,
    max_iterations: usize,
    max_precision: u32,
) -> ApplyResult {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let arg_buf = &mut wrapper.arg_buf;
        let precision_bucket_size = wrapper.precision_bucket_size;

        machine.max_precision = max_precision;

        let arg_ptrs_slice = slice::from_raw_parts(args_ptrs, args_len);

        for (i, &raw_ptr) in arg_ptrs_slice.iter().enumerate() {
            let val = &mut arg_buf[i];

            mpfr::set(
                val.lo.as_float_mut().as_raw_mut(),
                raw_ptr,
                mpfr::rnd_t::RNDN,
            );
            mpfr::set(
                val.hi.as_float_mut().as_raw_mut(),
                raw_ptr,
                mpfr::rnd_t::RNDN,
            );

            // Might be redundant, but let's stay safe for now cus why not
            val.lo.immovable = true;
            val.hi.immovable = true;
            // Error if not rational
            val.err = if val.lo.as_float().is_finite() {
                ErrorFlags::none()
            } else {
                ErrorFlags::error()
            };
        }

        let hints = if hints_ptr.is_null() {
            None
        } else {
            Some((&*hints_ptr).hints.as_slice())
        };

        let res = machine.apply(arg_buf, hints, max_iterations);
        let status_code = match &res {
            Ok(vals) => {
                if vals.len() != out_len {
                    // This should not happen if caller is correct
                    2
                } else {
                    let out_ptrs_slice = slice::from_raw_parts(out_ptrs, out_len);
                    for (i, val) in vals.iter().enumerate() {
                        let out_ptr = out_ptrs_slice[i];
                        mpfr::set(out_ptr, val.lo.as_float().as_raw(), mpfr::rnd_t::RNDN);
                    }
                    0 // Success
                }
            }
            Err(RivalError::InvalidInput) => -1,
            Err(RivalError::Unsamplable) => -2,
        };

        // Aggregate profiler records in rust so that we dont have to pay the cost in racket
        let mut aggregation: HashMap<(i32, u32), (f64, usize)> = HashMap::new();
        for exec in machine.profiler.records().iter() {
            let precision_bucket = exec.precision - (exec.precision % precision_bucket_size);
            let key = (exec.number, precision_bucket);
            let entry = aggregation.entry(key).or_insert((0.0, 0));
            entry.0 += exec.time_ms;
            entry.1 += 0;
        }

        // Store aggregated results in cache
        wrapper.aggregated_profile_cache.clear();
        for ((instruction_index, precision_bucket), (total_time_ms, total_memory)) in aggregation {
            wrapper.aggregated_profile_cache.push(AggregatedProfileEntry {
                instruction_index,
                precision_bucket,
                total_time_ms,
                total_memory,
            });
        }

        // Get bumps and iterations before reset
        let bumps = machine.bumps;
        let iterations = machine.iteration;

        // Reset profiler for next call
        machine.profiler.reset();

        ApplyResult {
            status_code,
            bumps,
            iterations,
            profile_ptr: wrapper.aggregated_profile_cache.as_ptr(),
            profile_len: wrapper.aggregated_profile_cache.len(),
        }
    });

    match result {
        Ok(apply_result) => apply_result,
        Err(_) => ApplyResult {
            status_code: -99, // Panic
            bumps: 0,
            iterations: 0,
            profile_ptr: ptr::null(),
            profile_len: 0,
        },
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_analyze_with_hints(
    ptr: *mut MachineWrapper,
    rect_ptrs: *const *const mpfr_t,
    rect_len: usize,
    hints_ptr: *const HintsHandle,
) -> AnalyzeResult {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let rect_buf = &mut wrapper.rect_buf;

        let rect_ptrs_slice = slice::from_raw_parts(rect_ptrs, rect_len * 2);

        for i in 0..rect_len {
            let lo_ptr = rect_ptrs_slice[2 * i];
            let hi_ptr = rect_ptrs_slice[2 * i + 1];

            let ival = &mut rect_buf[i];

            mpfr::set(
                ival.lo.as_float_mut().as_raw_mut(),
                lo_ptr,
                mpfr::rnd_t::RNDN,
            );
            mpfr::set(
                ival.hi.as_float_mut().as_raw_mut(),
                hi_ptr,
                mpfr::rnd_t::RNDN,
            );
        }

        let hints = if hints_ptr.is_null() {
            None
        } else {
            Some((&*hints_ptr).hints.as_slice())
        };

        let (status, next_hints, converged) = machine.analyze_with_hints(rect_buf, hints);

        let is_error = !status.lo.as_float().is_zero();
        let maybe_error = !status.hi.as_float().is_zero();
        let hints_handle = Box::into_raw(Box::new(HintsHandle { hints: next_hints }));

        AnalyzeResult {
            status_code: 0,
            is_error,
            maybe_error,
            converged,
            hints: hints_handle,
        }
    });

    match result {
        Ok(res) => res,
        Err(_) => AnalyzeResult {
            status_code: -1,
            is_error: true,
            maybe_error: true,
            converged: false,
            hints: ptr::null_mut(),
        },
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_profile(ptr: *mut MachineWrapper, field: u32) -> ProfileData {
    let wrapper = &mut *ptr;
    let machine = &mut wrapper.machine;

    // Build execution cache from profiler records
    if field == ProfileField::Executions as u32 {
        wrapper.execution_cache.clear();
        for exec in machine.profiler.records().iter() {
            wrapper.execution_cache.push(ExecutionRecord {
                name_ptr: exec.name.as_ptr(),
                name_len: exec.name.len(),
                number: exec.number,
                precision: exec.precision,
                time_ms: exec.time_ms,
                iteration: exec.iteration,
            });
        }
    }

    if field == ProfileField::Executions as u32 {
        machine.profiler.reset();
    }

    ProfileData {
        instructions_len: machine.instructions.len(),
        iterations: machine.iteration,
        bumps: machine.bumps,
        executions_ptr: wrapper.execution_cache.as_ptr(),
        executions_len: wrapper.execution_cache.len(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_instruction_names(
    ptr: *mut MachineWrapper,
    out_len: *mut usize,
) -> *const u8 {
    let wrapper = &mut *ptr;

    // Build cache if empty (only on first call)
    if wrapper.instruction_names_cache.is_empty() {
        let names: Vec<&str> = wrapper
            .machine
            .instructions
            .iter()
            .map(|instr| instr.data.name_static())
            .collect();
        // Null-separated for easy parsing in Racket
        wrapper.instruction_names_cache = names.join("\0").into_bytes();
    }

    *out_len = wrapper.instruction_names_cache.len();
    wrapper.instruction_names_cache.as_ptr()
}

#[no_mangle]
pub extern "C" fn rival_hints_destroy(ptr: *mut HintsHandle) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(ptr));
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_destroy(ptr: *mut MachineWrapper) {
    if ptr.is_null() {
        return;
    }
    drop(Box::from_raw(ptr));
}
