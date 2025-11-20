use rival::eval::machine::Hint;
use rival::{Ival, Machine, MachineBuilder, RivalError};
use rug::Float;
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_void};
use std::panic;
use std::ptr;

mod discretization;
mod parser;
use discretization::{
    CallbackDiscretization, ConvertCallback, DistanceCallback, Fp64Discretization,
    FreeStringCallback,
};
use parser::{parse_expr, parse_sexpr, SExpr};

enum RivalMachine {
    Fp64(Machine<Fp64Discretization>),
    Callback(Machine<CallbackDiscretization>),
}

fn return_string(s: String) -> *mut c_char {
    CString::new(s).unwrap().into_raw()
}

#[no_mangle]
pub extern "C" fn rival_make_context(
    exprs_str: *const c_char,
    vars_str: *const c_char,
    use_callback: bool,
    target_prec: u32,
    convert_cb: ConvertCallback,
    distance_cb: DistanceCallback,
    free_cb: FreeStringCallback,
) -> *mut c_void {
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

        if use_callback {
            let disc = CallbackDiscretization {
                target_prec,
                convert_cb,
                distance_cb,
                free_cb,
            };
            let machine = MachineBuilder::new(disc).build(exprs, vars);
            Box::into_raw(Box::new(RivalMachine::Callback(machine))) as *mut c_void
        } else {
            let disc = Fp64Discretization;
            let machine = MachineBuilder::new(disc).build(exprs, vars);
            Box::into_raw(Box::new(RivalMachine::Fp64(machine))) as *mut c_void
        }
    });

    match result {
        Ok(ptr) => ptr,
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_destroy(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    drop(Box::from_raw(ptr as *mut RivalMachine));
}

#[no_mangle]
pub unsafe extern "C" fn rival_eval(
    ptr: *mut c_void,
    args_str: *const c_char,
    hints_str: *const c_char,
) -> *mut c_char {
    let result = panic::catch_unwind(|| {
        let machine_enum = &mut *(ptr as *mut RivalMachine);
        let args_str = CStr::from_ptr(args_str).to_string_lossy();
        let hints_str = CStr::from_ptr(hints_str).to_string_lossy();

        let args: Vec<Ival> = args_str
            .split_whitespace()
            .map(|s| {
                let f_val = Float::parse(s).expect("Failed to parse float");
                let f = Float::with_val(256, f_val);
                Ival::from_lo_hi(f.clone(), f)
            })
            .collect();

        let hints = parse_hints(&hints_str);

        let res = match machine_enum {
            RivalMachine::Fp64(m) => m.apply(&args, hints.as_deref(), 5),
            RivalMachine::Callback(m) => m.apply(&args, hints.as_deref(), 5),
        };

        match res {
            Ok(vals) => {
                if !vals.is_empty() {
                    let pre = &vals[0];
                    if pre.hi.as_float().is_zero() {
                        return String::from("(invalid)");
                    }
                }

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
            Err(RivalError::InvalidInput) => String::from("(invalid)"), // RivalError::InvalidInput
            Err(RivalError::Unsamplable) => String::from("(unsamplable)"),
        }
    });

    match result {
        Ok(s) => return_string(s),
        Err(_) => return_string(String::from("(error panic)")),
    }
}

fn parse_hints(s: &str) -> Option<Vec<Hint>> {
    if s == "false" || s == "()" {
        return None;
    }
    let sexpr = parse_sexpr(s).ok()?;
    match sexpr {
        SExpr::List(list) => {
            let mut hints = Vec::new();
            for item in list {
                match item {
                    SExpr::Atom(s) => match s.as_str() {
                        "execute" => hints.push(Hint::Execute),
                        "skip" => hints.push(Hint::Skip),
                        _ => return None,
                    },
                    SExpr::List(sublist) => {
                        if sublist.len() == 2 {
                            if let SExpr::Atom(op) = &sublist[0] {
                                match op.as_str() {
                                    "alias" => {
                                        if let SExpr::Atom(n_str) = &sublist[1] {
                                            let n = n_str.parse::<u8>().ok()?;
                                            hints.push(Hint::Alias(n));
                                        } else {
                                            return None;
                                        }
                                    }
                                    "known-bool" => {
                                        if let SExpr::Atom(b_str) = &sublist[1] {
                                            let b = match b_str.as_str() {
                                                "true" => true,
                                                "false" => false,
                                                _ => return None,
                                            };
                                            hints.push(Hint::KnownBool(b));
                                        } else {
                                            return None;
                                        }
                                    }
                                    _ => return None,
                                }
                            } else {
                                return None;
                            }
                        } else {
                            return None;
                        }
                    }
                }
            }
            Some(hints)
        }
        _ => None,
    }
}

#[no_mangle]
pub unsafe extern "C" fn rival_analyze(
    ptr: *mut c_void,
    args_str: *const c_char,
    hints_str: *const c_char,
) -> *mut c_char {
    let result = panic::catch_unwind(|| {
        let machine_enum = &mut *(ptr as *mut RivalMachine);
        let args_str = CStr::from_ptr(args_str).to_string_lossy();
        let hints_str = CStr::from_ptr(hints_str).to_string_lossy();

        // Parse args: "lo1 hi1 lo2 hi2 ..."
        let parts: Vec<&str> = args_str.split_whitespace().collect();
        let mut args = Vec::new();
        for chunk in parts.chunks(2) {
            if chunk.len() == 2 {
                let lo_val = Float::parse(chunk[0]).expect("Failed to parse float");
                let hi_val = Float::parse(chunk[1]).expect("Failed to parse float");
                let lo = Float::with_val(256, lo_val);
                let hi = Float::with_val(256, hi_val);
                args.push(Ival::from_lo_hi(lo, hi));
            }
        }

        let hints = parse_hints(&hints_str);

        let (status, next_hints, converged) = match machine_enum {
            RivalMachine::Fp64(m) => m.analyze_with_hints(args, hints.as_deref()),
            RivalMachine::Callback(m) => m.analyze_with_hints(args, hints.as_deref()),
        };

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
        // We need a format for hints.
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
pub unsafe extern "C" fn rival_free_string(ptr: *mut c_char) {
    if ptr.is_null() {
        return;
    }
    drop(CString::from_raw(ptr));
}
