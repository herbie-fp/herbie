use gmp_mpfr_sys::mpfr::{self, mpfr_t};
use rival::eval::machine::Hint;
use rival::{Discretization, Ival, Machine, MachineBuilder, RivalError};
use rug::{Assign, Float};
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::panic;
use std::ptr;
use std::slice;

mod discretization;
mod parser;
use discretization::{DiscretizationType, RustDiscretization};
use parser::{parse_expr, parse_sexpr, SExpr};

// Status code constants (must match Racket side)
const STATUS_SUCCESS: i32 = 0;
const STATUS_INVALID_INPUT: i32 = -1;
const STATUS_UNSAMPLABLE: i32 = -2;
const STATUS_LENGTH_MISMATCH: i32 = 2;
const STATUS_PANIC: i32 = -99;

pub struct MachineWrapper {
    machine: Machine<RustDiscretization>,
    arg_buf: Vec<Ival>,
}

/// Ensures arg_buf has the correct size and precision for the current machine state.
/// Returns the precision being used.
fn ensure_arg_buf(
    arg_buf: &mut Vec<Ival>,
    required_len: usize,
    target_prec: u32,
    min_prec: u32,
) -> u32 {
    let arg_prec = target_prec.max(min_prec);

    // Resize buffer if needed
    if arg_buf.len() != required_len {
        arg_buf.clear();
        arg_buf.reserve(required_len);
        for _ in 0..required_len {
            arg_buf.push(Ival::from_lo_hi(Float::new(arg_prec), Float::new(arg_prec)));
        }
    }

    arg_prec
}

/// Updates the precision of an Ival if it doesn't match the required precision.
fn update_ival_precision(val: &mut Ival, required_prec: u32) {
    if val.lo.as_float().prec() != required_prec {
        val.lo.as_float_mut().set_prec(required_prec);
        val.hi.as_float_mut().set_prec(required_prec);
    }
}

fn parse_hints_binary(data: &[u8]) -> Option<Vec<Hint>> {
    if data.is_empty() {
        return None;
    }
    let mut hints = Vec::with_capacity(data.len());
    let mut i = 0;
    while i < data.len() {
        match data[i] {
            0 => {
                hints.push(Hint::Execute);
                i += 1;
            }
            1 => {
                hints.push(Hint::Skip);
                i += 1;
            }
            2 => {
                if i + 1 >= data.len() {
                    return None;
                }
                hints.push(Hint::Alias(data[i + 1]));
                i += 2;
            }
            3 => {
                if i + 1 >= data.len() {
                    return None;
                }
                hints.push(Hint::KnownBool(data[i + 1] != 0));
                i += 2;
            }
            _ => return None,
        }
    }
    Some(hints)
}

#[no_mangle]
pub extern "C" fn rival_compile(
    exprs_str: *const c_char,
    vars_str: *const c_char,
    target: u32,
    types_ptr: *const u32,
    num_types: u32,
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
        let machine = MachineBuilder::new(disc).slack_unit(512).build(exprs, vars);
        let wrapper = MachineWrapper {
            machine,
            arg_buf: Vec::new(),
        };
        Box::into_raw(Box::new(wrapper))
    });

    match result {
        Ok(ptr) => ptr,
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_destroy(ptr: *mut MachineWrapper) {
    if ptr.is_null() {
        return;
    }
    drop(Box::from_raw(ptr));
}

#[no_mangle]
pub unsafe extern "C" fn rival_apply(
    ptr: *mut MachineWrapper,
    args_ptrs: *const *const mpfr_t,
    args_len: usize,
    out_ptrs: *const *mut mpfr_t,
    out_len: usize,
    hints_ptr: *const u8,
    hints_len: usize,
    max_iterations: usize,
) -> i32 {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let arg_buf = &mut wrapper.arg_buf;

        let arg_prec = ensure_arg_buf(
            arg_buf,
            args_len,
            machine.disc.target(),
            machine.state.min_precision,
        );

        let arg_ptrs_slice = slice::from_raw_parts(args_ptrs, args_len);

        for (i, &raw_ptr) in arg_ptrs_slice.iter().enumerate() {
            let val = &mut arg_buf[i];
            update_ival_precision(val, arg_prec);

            // Copy value
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
        }

        let hints = parse_hints_binary(slice::from_raw_parts(hints_ptr, hints_len));

        let res = machine.apply(arg_buf, hints.as_deref(), max_iterations);
        match res {
            Ok(vals) => {
                if vals.len() != out_len {
                    return STATUS_LENGTH_MISMATCH;
                }
                let out_ptrs_slice = slice::from_raw_parts(out_ptrs, out_len);

                for (i, val) in vals.iter().enumerate() {
                    let out_ptr = out_ptrs_slice[i];
                    mpfr::set(out_ptr, val.lo.as_float().as_raw(), mpfr::rnd_t::RNDN);
                }
                STATUS_SUCCESS
            }
            Err(RivalError::InvalidInput) => STATUS_INVALID_INPUT,
            Err(RivalError::Unsamplable) => STATUS_UNSAMPLABLE,
        }
    });

    match result {
        Ok(code) => code,
        Err(_) => STATUS_PANIC,
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_apply_batch(
    ptr: *mut MachineWrapper,
    batch_size: usize,
    args_data: *const f64,
    args_len: usize,
    out_data: *mut f64,
    out_len: usize,
    hints_data: *const u8,
    hints_offsets: *const usize,
    hints_lens: *const usize,
    result_codes: *mut i32,
    max_iterations: usize,
) -> i32 {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let arg_buf = &mut wrapper.arg_buf;

        let arg_prec = ensure_arg_buf(
            arg_buf,
            args_len,
            machine.disc.target(),
            machine.state.min_precision,
        );

        let args_slice = slice::from_raw_parts(args_data, batch_size * args_len);
        let hints_offsets_slice = slice::from_raw_parts(hints_offsets, batch_size);
        let hints_lens_slice = slice::from_raw_parts(hints_lens, batch_size);
        let result_codes_slice = slice::from_raw_parts_mut(result_codes, batch_size);
        let out_slice = slice::from_raw_parts_mut(out_data, batch_size * out_len);

        // Process each point in the batch
        for batch_idx in 0..batch_size {
            // Copy args for this point into arg_buf
            let base_arg_idx = batch_idx * args_len;
            for i in 0..args_len {
                let val = &mut arg_buf[i];
                let f64_val = args_slice[base_arg_idx + i];

                update_ival_precision(val, arg_prec);

                // Set value from f64 (lossless for 53-bit precision)
                val.lo.as_float_mut().assign(f64_val);
                val.hi.as_float_mut().assign(f64_val);
            }

            // Parse hints for this point
            let len = hints_lens_slice[batch_idx];
            let hint_data = if len > 0 {
                let offset = hints_offsets_slice[batch_idx];
                let hint_bytes = slice::from_raw_parts(hints_data.add(offset), len);
                parse_hints_binary(hint_bytes)
            } else {
                None
            };

            // Apply machine
            let res = machine.apply(arg_buf, hint_data.as_deref(), max_iterations);

            match res {
                Ok(vals) => {
                    if vals.len() != out_len {
                        result_codes_slice[batch_idx] = STATUS_LENGTH_MISMATCH;
                        continue;
                    }

                    // Copy outputs as f64
                    let base_out_idx = batch_idx * out_len;
                    for (i, val) in vals.iter().enumerate() {
                        out_slice[base_out_idx + i] = val.lo.as_float().to_f64();
                    }
                    result_codes_slice[batch_idx] = STATUS_SUCCESS;
                }
                Err(RivalError::InvalidInput) => {
                    result_codes_slice[batch_idx] = STATUS_INVALID_INPUT
                }
                Err(RivalError::Unsamplable) => result_codes_slice[batch_idx] = STATUS_UNSAMPLABLE,
            }
        }

        STATUS_SUCCESS // Overall success
    });

    match result {
        Ok(code) => code,
        Err(_) => STATUS_PANIC,
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_analyze_with_hints(
    ptr: *mut MachineWrapper,
    rect_ptrs: *const *const mpfr_t,
    rect_len: usize,
    hints_ptr: *const u8,
    hints_len: usize,
    out_lo: *mut mpfr_t,
    out_hi: *mut mpfr_t,
    out_converged: *mut i32,
    out_hints_len: *mut usize,
) -> *mut u8 {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;

        let arg_prec = machine.disc.target().max(machine.state.min_precision);
        let rect_ptrs_slice = slice::from_raw_parts(rect_ptrs, rect_len * 2);

        let mut args = Vec::with_capacity(rect_len);
        for chunk in rect_ptrs_slice.chunks(2) {
            let lo_ptr = chunk[0];
            let hi_ptr = chunk[1];

            let mut lo = Float::new(arg_prec);
            let mut hi = Float::new(arg_prec);

            mpfr::set(lo.as_raw_mut(), lo_ptr, mpfr::rnd_t::RNDN);
            mpfr::set(hi.as_raw_mut(), hi_ptr, mpfr::rnd_t::RNDN);

            args.push(Ival::from_lo_hi(lo, hi));
        }

        let hints = parse_hints_binary(slice::from_raw_parts(hints_ptr, hints_len));
        let (status, next_hints, converged) = machine.analyze_with_hints(args, hints.as_deref());

        mpfr::set(out_lo, status.lo.as_float().as_raw(), mpfr::rnd_t::RNDN);
        mpfr::set(out_hi, status.hi.as_float().as_raw(), mpfr::rnd_t::RNDN);
        *out_converged = if converged { 1 } else { 0 };

        let mut hints_bytes = Vec::new();
        for hint in next_hints {
            match hint {
                Hint::Execute => hints_bytes.push(0),
                Hint::Skip => hints_bytes.push(1),
                Hint::Alias(n) => {
                    hints_bytes.push(2);
                    hints_bytes.push(n);
                }
                Hint::KnownBool(b) => {
                    hints_bytes.push(3);
                    hints_bytes.push(if b { 1 } else { 0 });
                }
            }
        }

        *out_hints_len = hints_bytes.len();

        let mut buf = hints_bytes.into_boxed_slice();
        let ptr = buf.as_mut_ptr();
        std::mem::forget(buf);
        ptr
    });

    match result {
        Ok(ptr) => ptr,
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_free_bytes(ptr: *mut u8, len: usize) {
    if ptr.is_null() || len == 0 {
        return;
    }
    let _ = Box::from_raw(slice::from_raw_parts_mut(ptr, len));
}

#[no_mangle]
pub unsafe extern "C" fn rival_profile(
    ptr: *mut MachineWrapper,
    param_str: *const c_char,
) -> *mut c_char {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let param = CStr::from_ptr(param_str).to_string_lossy();

        match param.as_ref() {
            "instructions" => machine.state.instructions.len().to_string(),
            "iterations" => machine.state.iteration.to_string(),
            "bumps" => machine.state.bumps.to_string(),
            "executions" => {
                let exec_strings: Vec<String> = machine
                    .state
                    .profiler
                    .iter()
                    .map(|exec| {
                        format!(
                            "({} {} {} {} 0 {})",
                            exec.name, exec.number, exec.precision, exec.time_ms, exec.iteration
                        )
                    })
                    .collect();
                machine.state.profiler.reset();
                format!("({})", exec_strings.join(" "))
            }
            _ => String::from(""),
        }
    });

    match result {
        Ok(s) => CString::new(s).unwrap().into_raw(),
        Err(_) => CString::new(String::from("(error panic)"))
            .unwrap()
            .into_raw(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_free_string(ptr: *mut c_char) {
    if ptr.is_null() {
        return;
    }
    drop(CString::from_raw(ptr));
}
