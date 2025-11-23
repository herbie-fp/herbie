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
use discretization::{
    CallbackDiscretization, ConvertCallback, DistanceCallback, FreeStringCallback,
};
use parser::{parse_expr, parse_sexpr, SExpr};

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
    convert_cb: ConvertCallback,
    distance_cb: DistanceCallback,
    free_cb: FreeStringCallback,
) -> *mut Machine<CallbackDiscretization> {
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

        let disc = CallbackDiscretization {
            target,
            convert_cb,
            distance_cb,
            free_cb,
        };
        let machine = MachineBuilder::new(disc).build(exprs, vars);
        Box::into_raw(Box::new(machine))
    });

    match result {
        Ok(ptr) => ptr,
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_destroy(ptr: *mut Machine<CallbackDiscretization>) {
    if ptr.is_null() {
        return;
    }
    drop(Box::from_raw(ptr));
}

#[no_mangle]
pub unsafe extern "C" fn rival_apply(
    ptr: *mut Machine<CallbackDiscretization>,
    args_str: *const c_char,
    hints_ptr: *const u8,
    hints_len: usize,
    max_iterations: usize,
) -> *mut c_char {
    let result = panic::catch_unwind(|| {
        let machine = &mut *ptr;
        let args_str = CStr::from_ptr(args_str).to_string_lossy();

        let arg_prec = machine.disc.target().max(machine.state.min_precision);
        let args: Vec<Ival> = args_str
            .split_whitespace()
            .map(|s| {
                let f_val = Float::parse(s).expect("Failed to parse float");
                let f = Float::with_val(arg_prec, f_val);
                Ival::from_lo_hi(f.clone(), f)
            })
            .collect();

        let hints = parse_hints_binary(slice::from_raw_parts(hints_ptr, hints_len));
        let res = machine.apply(&args, hints.as_deref(), max_iterations);

        match res {
            Ok(vals) => {
                let mut s = String::from("(valid");
                for val in vals {
                    let f = val.lo.as_float();
                    s.push_str(" ");
                    if f.is_infinite() {
                        if f.is_sign_negative() {
                            s.push_str("-inf.0");
                        } else {
                            s.push_str("+inf.0");
                        }
                    } else if f.is_nan() {
                        s.push_str("+nan.0");
                    } else {
                        s.push_str(&f.to_string_radix(10, None));
                    }
                }
                s.push_str(")");
                s
            }
            Err(RivalError::InvalidInput) => String::from("(invalid)"),
            Err(RivalError::Unsamplable) => String::from("(unsamplable)"),
        }
    });

    match result {
        Ok(s) => return_string(s),
        Err(_) => return_string(String::from("(error panic)")),
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_analyze_with_hints(
    ptr: *mut Machine<CallbackDiscretization>,
    rect_str: *const c_char,
    hints_ptr: *const u8,
    hints_len: usize,
) -> *mut c_char {
    let result = panic::catch_unwind(|| {
        let machine = &mut *ptr;
        let rect_str = CStr::from_ptr(rect_str).to_string_lossy();

        // Parse args: "lo1 hi1 lo2 hi2 ..."
        let parts: Vec<&str> = rect_str.split_whitespace().collect();
        let arg_prec = machine.disc.target().max(machine.state.min_precision);

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

        let (status, next_hints, converged) = machine.analyze_with_hints(args, hints.as_deref());

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
    ptr: *mut Machine<CallbackDiscretization>,
    param_str: *const c_char,
) -> *mut c_char {
    let result = panic::catch_unwind(|| {
        let machine = &mut *ptr;
        let param = CStr::from_ptr(param_str).to_string_lossy();

        match param.as_ref() {
            "instructions" => machine.state.instructions.len().to_string(),
            "iterations" => machine.state.iteration.to_string(),
            "bumps" => machine.state.bumps.to_string(),
            "executions" => {
                let mut s = String::from("(");
                for exec in machine.state.profiler.iter() {
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
                machine.state.profiler.reset();

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
