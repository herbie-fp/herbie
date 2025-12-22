use gmp_mpfr_sys::mpfr::{self, mpfr_t};
use rival::eval::machine::Hint;
use rival::{Discretization, ErrorFlags, Ival, Machine, MachineBuilder, RivalError};
use rug::Float;
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

        let wrapper = MachineWrapper {
            machine,
            arg_buf,
            rect_buf,
            execution_cache: Vec::new(),
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
) -> i32 {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let arg_buf = &mut wrapper.arg_buf;

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
        match res {
            Ok(vals) => {
                if vals.len() != out_len {
                    // This should not happen if caller is correct
                    return 2;
                }
                let out_ptrs_slice = slice::from_raw_parts(out_ptrs, out_len);

                for (i, val) in vals.iter().enumerate() {
                    let out_ptr = out_ptrs_slice[i];
                    // Copy result from Rust's Float to Racket's mpfr_t
                    mpfr::set(out_ptr, val.lo.as_float().as_raw(), mpfr::rnd_t::RNDN);
                }
                0 // Success
            }
            Err(RivalError::InvalidInput) => -1,
            Err(RivalError::Unsamplable) => -2,
        }
    });

    match result {
        Ok(code) => code,
        Err(_) => -99, // Panic
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
