use gmp_mpfr_sys::mpfr::{self, mpfr_t};
use rival::eval::machine::Hint;
use rival::{Discretization, Ival, Machine, MachineBuilder, RivalError};
use rug::Float;
use std::ffi::{CStr, CString};
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
    hint_buf: Vec<u8>,
}

#[repr(C)]
pub struct ProfileRecord {
    pub name_ptr: *const u8,
    pub name_len: usize,
    pub number: i32,
    pub precision: u32,
    pub time_ms: f64,
    pub iteration: usize,
}

fn return_string(s: String) -> *mut c_char {
    CString::new(s).unwrap().into_raw()
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

fn serialize_hints_binary(hints: &[Hint], out: &mut Vec<u8>) {
    out.clear();
    out.reserve(hints.len() * 2);
    for hint in hints {
        match hint {
            Hint::Execute => out.push(0),
            Hint::Skip => out.push(1),
            Hint::Alias(n) => {
                out.push(2);
                out.push(*n);
            }
            Hint::KnownBool(b) => {
                out.push(3);
                out.push(if *b { 1 } else { 0 });
            }
        }
    }
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
        let machine = MachineBuilder::new(disc)
            .profile_capacity(1000)
            .max_precision(10000)
            .build(exprs, vars);
        let wrapper = MachineWrapper {
            machine,
            arg_buf: Vec::new(),
            hint_buf: Vec::new(),
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

        let arg_prec = machine.disc.target().max(machine.min_precision);

        // Resize buffer if needed
        if arg_buf.len() != args_len {
            arg_buf.clear();
            arg_buf.reserve(args_len);
            for _ in 0..args_len {
                arg_buf.push(Ival::from_lo_hi(Float::new(arg_prec), Float::new(arg_prec)));
            }
        }

        let arg_ptrs_slice = slice::from_raw_parts(args_ptrs, args_len);

        for (i, &raw_ptr) in arg_ptrs_slice.iter().enumerate() {
            let val = &mut arg_buf[i];
            // Update precision if needed
            if val.lo.as_float().prec() != arg_prec {
                val.lo.as_float_mut().set_prec(arg_prec);
                val.hi.as_float_mut().set_prec(arg_prec);
            }

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
pub unsafe extern "C" fn rival_apply_batch(
    ptr: *mut MachineWrapper,
    batch_size: usize,
    args_ptrs: *const *const mpfr_t,
    args_len: usize,
    out_ptrs: *const *mut mpfr_t,
    out_len: usize,
    hints_ptrs: *const *const u8,
    hints_lens: *const usize,
    max_iterations: usize,
    status_out: *mut i32,
) -> i32 {
    // Single catch_unwind for the whole batch - fast path when no panics
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let arg_buf = &mut wrapper.arg_buf;
        let arg_prec = machine.disc.target().max(machine.min_precision);

        // Resize argument buffer if needed
        if arg_buf.len() != args_len {
            arg_buf.clear();
            arg_buf.reserve(args_len);
            for _ in 0..args_len {
                arg_buf.push(Ival::from_lo_hi(Float::new(arg_prec), Float::new(arg_prec)));
            }
        }

        let all_args = slice::from_raw_parts(args_ptrs, batch_size * args_len);
        let all_outs = slice::from_raw_parts(out_ptrs, batch_size * out_len);
        let hints_ptrs_slice = slice::from_raw_parts(hints_ptrs, batch_size);
        let hints_lens_slice = slice::from_raw_parts(hints_lens, batch_size);
        let status_slice = slice::from_raw_parts_mut(status_out, batch_size);

        for batch_idx in 0..batch_size {
            // Load arguments for this point
            let args_offset = batch_idx * args_len;
            for i in 0..args_len {
                let raw_ptr = all_args[args_offset + i];
                let val = &mut arg_buf[i];

                // Update precision if needed
                if val.lo.as_float().prec() != arg_prec {
                    val.lo.as_float_mut().set_prec(arg_prec);
                    val.hi.as_float_mut().set_prec(arg_prec);
                }

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

            // Parse hints for this point
            let hints = if hints_lens_slice[batch_idx] > 0 {
                parse_hints_binary(slice::from_raw_parts(
                    hints_ptrs_slice[batch_idx],
                    hints_lens_slice[batch_idx],
                ))
            } else {
                None
            };

            // Apply the machine
            let res = machine.apply(arg_buf, hints.as_deref(), max_iterations);

            // Handle result
            match res {
                Ok(vals) => {
                    if vals.len() != out_len {
                        status_slice[batch_idx] = 2;
                        continue;
                    }
                    let outs_offset = batch_idx * out_len;
                    for (i, val) in vals.iter().enumerate() {
                        let out_ptr = all_outs[outs_offset + i];
                        mpfr::set(out_ptr, val.lo.as_float().as_raw(), mpfr::rnd_t::RNDN);
                    }
                    status_slice[batch_idx] = 0;
                }
                Err(RivalError::InvalidInput) => {
                    status_slice[batch_idx] = -1;
                }
                Err(RivalError::Unsamplable) => {
                    status_slice[batch_idx] = -2;
                }
            }
        }
    });

    match result {
        Ok(()) => 0,
        Err(_) => -99, // Panic occurred - caller should fall back to per-point evaluation
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_analyze_with_hints(
    ptr: *mut MachineWrapper,
    lo_ptrs: *const *const mpfr_t,
    hi_ptrs: *const *const mpfr_t,
    rect_len: usize,
    hints_ptr: *const u8,
    hints_len: usize,
    status_lo_out: *mut mpfr_t,
    status_hi_out: *mut mpfr_t,
    converged_out: *mut u8,
    next_hints_out: *mut u8,
    next_hints_capacity: usize,
    next_hints_len_out: *mut usize,
) -> i32 {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let arg_prec = machine.disc.target().max(machine.min_precision);
        if wrapper.arg_buf.len() != rect_len {
            wrapper.arg_buf.clear();
            wrapper.arg_buf.reserve(rect_len);
            for _ in 0..rect_len {
                wrapper
                    .arg_buf
                    .push(Ival::from_lo_hi(Float::new(arg_prec), Float::new(arg_prec)));
            }
        }

        let lo_slice = slice::from_raw_parts(lo_ptrs, rect_len);
        let hi_slice = slice::from_raw_parts(hi_ptrs, rect_len);

        for i in 0..rect_len {
            let val = &mut wrapper.arg_buf[i];
            if val.lo.as_float().prec() != arg_prec {
                val.lo.as_float_mut().set_prec(arg_prec);
                val.hi.as_float_mut().set_prec(arg_prec);
            }

            mpfr::set(
                val.lo.as_float_mut().as_raw_mut(),
                lo_slice[i],
                mpfr::rnd_t::RNDN,
            );
            mpfr::set(
                val.hi.as_float_mut().as_raw_mut(),
                hi_slice[i],
                mpfr::rnd_t::RNDN,
            );
        }

        let hints = parse_hints_binary(slice::from_raw_parts(hints_ptr, hints_len));
        let (status, next_hints, converged) =
            machine.analyze_with_hints(&wrapper.arg_buf[..rect_len], hints.as_deref());

        mpfr::set(
            status_lo_out,
            status.lo.as_float().as_raw(),
            mpfr::rnd_t::RNDN,
        );
        mpfr::set(
            status_hi_out,
            status.hi.as_float().as_raw(),
            mpfr::rnd_t::RNDN,
        );
        ptr::write(converged_out, if converged { 1 } else { 0 });

        serialize_hints_binary(&next_hints, &mut wrapper.hint_buf);
        let needed = wrapper.hint_buf.len();
        ptr::write(next_hints_len_out, needed);
        if needed > next_hints_capacity {
            return 1;
        }
        if needed > 0 {
            ptr::copy_nonoverlapping(wrapper.hint_buf.as_ptr(), next_hints_out, needed);
        }
        0
    });

    match result {
        Ok(code) => code,
        Err(_) => -99,
    }
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
            "instructions" => machine.instructions.len().to_string(),
            "iterations" => machine.iteration.to_string(),
            "bumps" => machine.bumps.to_string(),
            "executions" => {
                let mut s = String::from("(");
                for exec in machine.profiler.records().iter() {
                    s.push_str("(");
                    s.push_str(exec.name);
                    s.push_str(" ");
                    s.push_str(&exec.number.to_string());
                    s.push_str(" ");
                    s.push_str(&exec.precision.to_string());
                    s.push_str(" ");
                    s.push_str(&exec.time_ms.to_string());
                    s.push_str(" ");
                    s.push_str("0"); // memory
                    s.push_str(" ");
                    s.push_str(&exec.iteration.to_string());
                    s.push_str(") ");
                }
                machine.profiler.reset();

                if s.ends_with(" ") {
                    s.pop();
                }
                s.push_str(")");
                s
            }
            _ => String::from(""),
        }
    });

    match result {
        Ok(s) => return_string(s),
        Err(_) => return_string(String::from("(error panic)")),
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_free_string(ptr: *mut c_char) {
    if ptr.is_null() {
        return;
    }
    drop(CString::from_raw(ptr));
}

#[no_mangle]
pub unsafe extern "C" fn rival_profile_get_len(ptr: *mut MachineWrapper) -> usize {
    let wrapper = &*ptr;
    wrapper.machine.profiler.len()
}

#[no_mangle]
pub unsafe extern "C" fn rival_profile_get_records(
    ptr: *mut MachineWrapper,
    out_ptr: *mut ProfileRecord,
    capacity: usize,
) -> usize {
    let wrapper = &mut *ptr;
    let records = wrapper.machine.profiler.records();
    let count = records.len().min(capacity);

    let out_slice = slice::from_raw_parts_mut(out_ptr, count);
    for (i, rec) in records.iter().take(count).enumerate() {
        out_slice[i] = ProfileRecord {
            name_ptr: rec.name.as_ptr(),
            name_len: rec.name.len(),
            number: rec.number,
            precision: rec.precision,
            time_ms: rec.time_ms,
            iteration: rec.iteration,
        };
    }

    wrapper.machine.profiler.reset();
    count
}

#[no_mangle]
pub unsafe extern "C" fn rival_get_metric(ptr: *mut MachineWrapper, metric: u32) -> usize {
    let wrapper = &*ptr;
    match metric {
        0 => wrapper.machine.instructions.len(),
        1 => wrapper.machine.iteration,
        2 => wrapper.machine.bumps,
        _ => 0,
    }
}
