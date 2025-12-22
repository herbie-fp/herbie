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
        let machine = MachineBuilder::new(disc)
            .profile_capacity(profile_capacity)
            .max_precision(max_precision)
            .build(exprs, vars);
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
    max_precision: u32,
) -> i32 {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let arg_buf = &mut wrapper.arg_buf;

        machine.max_precision = max_precision;

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
pub unsafe extern "C" fn rival_analyze_with_hints(
    ptr: *mut MachineWrapper,
    rect_str: *const c_char,
    hints_ptr: *const u8,
    hints_len: usize,
) -> *mut c_char {
    let result = panic::catch_unwind(|| {
        let wrapper = &mut *ptr;
        let machine = &mut wrapper.machine;
        let rect_str = CStr::from_ptr(rect_str).to_string_lossy();

        // Parse args: "lo1 hi1 lo2 hi2 ..."
        let parts: Vec<&str> = rect_str.split_whitespace().collect();
        // This makes things work for benchmarks like piecewise defined which is kinda odd
        let arg_prec = 1024;

        let mut args = Vec::new();
        for chunk in parts.chunks(2) {
            if chunk.len() == 2 {
                let lo_val = Float::parse(chunk[0]).expect("Failed to parse float");
                let hi_val = Float::parse(chunk[1]).expect("Failed to parse float");
                let lo = Float::with_val(arg_prec, lo_val);
                let hi = Float::with_val(arg_prec, hi_val);
                args.push(Ival::from_lo_hi(lo, hi));
            }
        }

        let hints = parse_hints_binary(slice::from_raw_parts(hints_ptr, hints_len));
        let (status, next_hints, converged) = machine.analyze_with_hints(&args, hints.as_deref());

        // Serialize output
        // (status_lo status_hi converged (hint ...))
        let mut s = String::from("(");
        s.push_str(&status.lo.as_float().to_string_radix(10, None));
        s.push_str(" ");
        s.push_str(&status.hi.as_float().to_string_radix(10, None));
        s.push_str(" ");
        s.push_str(if converged { "true" } else { "false" });
        s.push_str(" (");
        // Serialize hints
        // Hint::Execute -> "execute"
        // Hint::Skip -> "skip"
        // Hint::Alias(u8) -> "(alias n)"
        // Hint::KnownBool(bool) -> "(known-bool b)"
        for hint in next_hints {
            match hint {
                Hint::Execute => s.push_str(" execute"),
                Hint::Skip => s.push_str(" skip"),
                Hint::Alias(n) => {
                    s.push_str(" (alias ");
                    s.push_str(&n.to_string());
                    s.push_str(")");
                }
                Hint::KnownBool(b) => {
                    s.push_str(" (known-bool ");
                    s.push_str(if b { "true" } else { "false" });
                    s.push_str(")");
                }
            }
        }
        s.push_str("))");
        s
    });

    match result {
        Ok(s) => return_string(s),
        Err(_) => return_string(String::from("(error panic)")),
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
