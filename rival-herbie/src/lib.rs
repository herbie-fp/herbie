#![allow(clippy::missing_safety_doc)]

use libc::strlen;
use rival::eval::machine::{Discretization, Machine, MachineBuilder};
use rival::eval::{self};
use rival::interval::Ival;
use rug::{float::Round, Float};
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;

// ---------- Shared C-string helpers (mirrors egg-herbie) ----------

#[no_mangle]
pub unsafe extern "C" fn destroy_string(ptr: *mut c_char) {
    drop(CString::from_raw(ptr))
}

#[no_mangle]
pub unsafe extern "C" fn string_length(ptr: *const c_char) -> u32 {
    strlen(ptr) as u32
}

fn to_c_string(s: &str) -> *mut c_char {
    CString::new(s).unwrap().into_raw()
}

unsafe fn from_c_str_lossy(p: *const c_char) -> String {
    CStr::from_ptr(p).to_string_lossy().into_owned()
}

// ---------- S-expression parsing lifted from rival-rs/bin/main.rs ----------

#[derive(Debug, Clone, PartialEq)]
enum SExpr {
    Atom(String),
    List(Vec<SExpr>),
}

fn parse_sexpr(s: &str) -> Result<SExpr, String> {
    let mut chars = s.trim().chars().peekable();
    parse_sexpr_inner(&mut chars)
}

fn parse_sexpr_inner(
    chars: &mut std::iter::Peekable<std::str::Chars>,
) -> Result<SExpr, String> {
    skip_whitespace(chars);
    match chars.peek() {
        Some('(') => {
            chars.next();
            let mut list = Vec::new();
            loop {
                skip_whitespace(chars);
                if chars.peek() == Some(&')') {
                    chars.next();
                    break;
                }
                if chars.peek().is_none() {
                    return Err("Unclosed list".to_string());
                }
                list.push(parse_sexpr_inner(chars)?);
            }
            Ok(SExpr::List(list))
        }
        Some(_) => {
            let mut atom = String::new();
            while let Some(&ch) = chars.peek() {
                if ch.is_whitespace() || ch == '(' || ch == ')' {
                    break;
                }
                atom.push(ch);
                chars.next();
            }
            Ok(SExpr::Atom(atom))
        }
        None => Err("Unexpected end of input".to_string()),
    }
}

fn skip_whitespace(chars: &mut std::iter::Peekable<std::str::Chars>) {
    while let Some(&ch) = chars.peek() {
        if !ch.is_whitespace() {
            break;
        }
        chars.next();
    }
}

// Convert S-expr to Rival Expr (subset needed by Herbie)
fn sexpr_to_expr(sexpr: &SExpr, vars: &[String]) -> Result<eval::ast::Expr, String> {
    sexpr_to_expr_env(sexpr, vars, &[])
}

fn sexpr_to_expr_env(
    sexpr: &SExpr,
    vars: &[String],
    env: &[(String, eval::ast::Expr)],
) -> Result<eval::ast::Expr, String> {
    use eval::ops::Expr as E;
    match sexpr {
        SExpr::Atom(s) => {
            // Look up let-bound names first
            if let Some((_, bound)) = env.iter().rev().find(|(name, _)| name == s) {
                return Ok(bound.clone());
            }
            if let Ok(f) = s.parse::<f64>() {
                return Ok(E::Literal(f));
            }
            if let Some(slash) = s.find('/') {
                if s.rfind('/') == Some(slash) {
                    let (nstr, dstr) = s.split_at(slash);
                    let dstr = &dstr[1..];
                    let n = nstr.parse::<i128>().map_err(|_| "bad num".to_string())?;
                    let d = dstr.parse::<i128>().map_err(|_| "bad den".to_string())?;
                    if d == 0 {
                        return Err("denominator is zero".to_string());
                    }
                    let neg = (n < 0) ^ (d < 0);
                    let na = if n < 0 { (-n) as u128 } else { n as u128 };
                    let da = if d < 0 { (-d) as u128 } else { d as u128 };
                    return Ok(E::Rational {
                        num: na as u64,
                        den: da as u64,
                        neg,
                    });
                }
                return Err("invalid rational".to_string());
            }
            if vars.iter().any(|v| v == s) {
                Ok(E::Var(s.clone()))
            } else {
                match s.as_str() {
                    "PI" | "pi" => Ok(E::Pi),
                    "E" | "e" => Ok(E::E),
                    "TRUE" => Ok(E::Literal(1.0)),
                    "FALSE" => Ok(E::Literal(0.0)),
                    _ => Err(format!("Unknown atom: {}", s)),
                }
            }
        }
        SExpr::List(list) => {
            if list.is_empty() {
                return Err("Empty list".to_string());
            }
            let head = &list[0];
            let args = &list[1..];
            let sym = match head {
                SExpr::Atom(s) => s.as_str(),
                _ => return Err("Invalid operator".to_string()),
            };
            use eval::ops::{TernaryOp as T, UnaryOp as U, UnaryParamOp as U1};
            // Helper folds for variadic ops
            let mut fold_left = |mut acc: eval::ast::Expr, rest: &[SExpr], bop: &str| -> Result<E, String> {
                for a in rest {
                    let rhs = sexpr_to_expr_env(a, vars, env)?;
                    acc = match bop {
                        "+" => E::Add(Box::new(acc), Box::new(rhs)),
                        "-" => E::Sub(Box::new(acc), Box::new(rhs)),
                        "*" => E::Mul(Box::new(acc), Box::new(rhs)),
                        "/" => E::Div(Box::new(acc), Box::new(rhs)),
                        _ => return Err(format!("unsupported fold op: {}", bop)),
                    };
                }
                Ok(acc)
            };
            let e = match sym {
                // Let and Let*
                "let" | "let*" => {
                    // (let ((v1 e1) (v2 e2) ...) body)
                    if args.is_empty() { return Err("let without args".to_string()); }
                    let bindings = &args[0];
                    let body = args.get(1).ok_or_else(|| "let missing body".to_string())?;
                    let mut new_env: Vec<(String, E)> = env.to_vec();
                    let parallel = sym == "let";
                    match bindings {
                        SExpr::List(pairs) => {
                            if parallel {
                                // Evaluate all in old env, then extend
                                let mut evaluated: Vec<(String, E)> = Vec::new();
                                for p in pairs {
                                    match p {
                                        SExpr::List(kv) if kv.len() == 2 => {
                                            let name = match &kv[0] { SExpr::Atom(s) => s.clone(), _ => return Err("let binding name".to_string()) };
                                            let val = sexpr_to_expr_env(&kv[1], vars, env)?;
                                            evaluated.push((name, val));
                                        }
                                        _ => return Err("let binding pair".to_string()),
                                    }
                                }
                                new_env.extend(evaluated);
                            } else {
                                // let*: sequential extend
                                for p in pairs {
                                    match p {
                                        SExpr::List(kv) if kv.len() == 2 => {
                                            let name = match &kv[0] { SExpr::Atom(s) => s.clone(), _ => return Err("let* binding name".to_string()) };
                                            let val = sexpr_to_expr_env(&kv[1], vars, &new_env)?;
                                            new_env.push((name, val));
                                        }
                                        _ => return Err("let* binding pair".to_string()),
                                    }
                                }
                            }
                        }
                        _ => return Err("let bindings must be a list".to_string()),
                    }
                    return sexpr_to_expr_env(body, vars, &new_env);
                }
                // Zero-arity operators/constants sometimes appear as calls
                // e.g., (TRUE), (FALSE), (PI), (E), (INFINITY), (NAN)
                "TRUE" if args.is_empty() => E::Literal(1.0),
                "FALSE" if args.is_empty() => E::Literal(0.0),
                "PI" if args.is_empty() => E::Pi,
                "E" if args.is_empty() => E::E,
                // Treat INFINITY/NAN as literals; Rival interval engine will clamp appropriately
                "INFINITY" if args.is_empty() => E::Literal(f64::INFINITY),
                "NAN" if args.is_empty() => E::Literal(f64::NAN),
                "+" => {
                    match args.len() {
                        0 => E::Literal(0.0),
                        1 => sexpr_to_expr_env(&args[0], vars, env)?,
                        _ => {
                            let first = sexpr_to_expr_env(&args[0], vars, env)?;
                            fold_left(first, &args[1..], "+")?
                        }
                    }
                }
                "-" => {
                    match args.len() {
                        1 => E::Neg(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                        2 => E::Sub(
                            Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                            Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                        ),
                        _ => {
                            let first = sexpr_to_expr_env(&args[0], vars, env)?;
                            fold_left(first, &args[1..], "-")?
                        }
                    }
                }
                "*" => {
                    match args.len() {
                        0 => E::Literal(1.0),
                        1 => sexpr_to_expr_env(&args[0], vars, env)?,
                        _ => {
                            let first = sexpr_to_expr_env(&args[0], vars, env)?;
                            fold_left(first, &args[1..], "*")?
                        }
                    }
                }
                "/" => {
                    match args.len() {
                        1 => E::Div(Box::new(E::Literal(1.0)), Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                        2 => E::Div(
                            Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                            Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                        ),
                        _ => {
                            let first = sexpr_to_expr_env(&args[0], vars, env)?;
                            fold_left(first, &args[1..], "/")?
                        }
                    }
                }
                "neg" => E::Neg(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "fabs" => E::Fabs(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "sqrt" => E::Sqrt(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "cbrt" => E::Cbrt(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "exp" => E::Exp(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "exp2" => E::Exp2(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "expm1" => E::Expm1(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "log" => E::Log(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "log2" => E::Log2(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "log10" => E::Log10(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "log1p" => E::Log1p(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "logb" => E::Logb(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "sin" => E::Sin(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "cos" => E::Cos(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "tan" => E::Tan(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "asin" => E::Asin(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "acos" => E::Acos(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "atan" => E::Atan(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "sinh" => E::Sinh(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "cosh" => E::Cosh(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "tanh" => E::Tanh(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "asinh" => E::Asinh(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "acosh" => E::Acosh(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "atanh" => E::Atanh(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "erf" => E::Erf(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "erfc" => E::Erfc(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "rint" => E::Rint(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "round" => E::Round(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "ceil" => E::Ceil(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "floor" => E::Floor(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "trunc" => E::Trunc(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "and" => E::And(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "or" => E::Or(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "==" | "eq" => E::Eq(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "!=" | "ne" => E::Ne(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "<" | "lt" => E::Lt(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "<=" | "le" => E::Le(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                ">" | "gt" => E::Gt(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                ">=" | "ge" => E::Ge(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "pow" => E::Pow(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "hypot" => E::Hypot(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "atan2" => E::Atan2(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "copysign" => E::Copysign(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "fdim" => E::Fdim(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "fmax" => E::Fmax(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "fmin" => E::Fmin(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "fmod" => E::Fmod(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "remainder" => E::Remainder(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                ),
                "if" => E::If(
                    Box::new(sexpr_to_expr_env(&args[0], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[1], vars, env)?),
                    Box::new(sexpr_to_expr_env(&args[2], vars, env)?),
                ),
                "not" => E::Not(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                "assert" => E::Assert(Box::new(sexpr_to_expr_env(&args[0], vars, env)?)),
                _ => return Err(format!("Unknown operator: {}", sym)),
            };
            Ok(e)
        }
    }
}

fn extract_vars(sexpr: &SExpr) -> Result<Vec<String>, String> {
    match sexpr {
        SExpr::List(vs) => vs
            .iter()
            .map(|v| match v {
                SExpr::Atom(s) => Ok(s.clone()),
                _ => Err("vars must be symbols".to_string()),
            })
            .collect(),
        _ => Err("vars must be a list".to_string()),
    }
}

fn extract_exprs(sexpr: &SExpr, vars: &[String]) -> Result<Vec<eval::ast::Expr>, String> {
    match sexpr {
        SExpr::List(es) => es.iter().map(|e| sexpr_to_expr(e, vars)).collect(),
        _ => Err("exprs must be a list".to_string()),
    }
}

// ---------- Discretizations lifted from rival-rs/bin/main.rs ----------

#[derive(Clone)]
struct Fp64Discretization;

impl Discretization for Fp64Discretization {
    fn target(&self) -> u32 {
        53
    }
    fn convert(&self, _idx: usize, v: &Float) -> Float {
        Float::with_val_round(53, v, Round::Nearest).0
    }
    fn distance(&self, _idx: usize, lo: &Float, hi: &Float) -> usize {
        let x = lo.to_f64();
        let y = hi.to_f64();
        #[inline]
        fn ordinal64(v: f64) -> i128 {
            if v < 0.0 { -((-v).abs().to_bits() as i128) } else { v.abs().to_bits() as i128 }
        }
        let ox = ordinal64(x);
        let oy = ordinal64(y);
        usize::try_from(oy.abs_diff(ox)).unwrap_or(usize::MAX)
    }
}

#[derive(Clone)]
struct BfDiscretization {
    precision: u32,
}

impl Discretization for BfDiscretization {
    fn target(&self) -> u32 {
        self.precision
    }
    fn convert(&self, _idx: usize, v: &Float) -> Float {
        let (f, _) = Float::with_val_round(self.precision, v, Round::Nearest);
        f
    }
    fn distance(&self, _idx: usize, lo: &Float, hi: &Float) -> usize {
        if lo == hi {
            0
        } else {
            let mut count = 0usize;
            let mut current = self.convert(_idx, lo);
            let target = self.convert(_idx, hi);
            while current < target && count < 1000 {
                rival::mpfr::mpfr_nextabove(&mut current);
                count += 1;
            }
            if current == target { count } else { usize::MAX }
        }
    }
}

enum MachineBox {
    Fp64(Machine<Fp64Discretization>),
    Bf(Machine<BfDiscretization>),
}

impl MachineBox {
    fn bumps(&self) -> usize {
        match self { MachineBox::Fp64(m) => m.bumps(), MachineBox::Bf(m) => m.bumps() }
    }
    fn load_args_and_run(&mut self, point: &[f64], max_iter: usize) -> (u32, Vec<Ival>) {
        match self {
            MachineBox::Fp64(m) => run_machine(m, point, max_iter),
            MachineBox::Bf(m) => run_machine(m, point, max_iter),
        }
    }
    fn instruction_count(&self) -> usize {
        match self {
            MachineBox::Fp64(m) => m.state.instructions.len(),
            MachineBox::Bf(m) => m.state.instructions.len(),
        }
    }
}

fn run_machine<D: Discretization>(machine: &mut Machine<D>, point: &[f64], max_iter: usize) -> (u32, Vec<Ival>) {
    let prec = machine.disc.target().max(53);
    let input_ivals: Vec<Ival> = point
        .iter()
        .map(|&val| {
            let mut ival = Ival::zero(prec);
            ival.f64_assign(val);
            ival
        })
        .collect();
    machine.load_arguments(&input_ivals);
    let hints = machine.default_hint().to_vec();
    let mut last_iter = 0u32;
    for iter in 0..=max_iter {
        last_iter = iter as u32;
        if let Ok(Some(final_vals)) = machine.run_iteration(iter, &hints) {
            return (last_iter, final_vals);
        }
    }
    (last_iter, machine.outputs().iter().map(|_| Ival::zero(prec)).collect())
}

#[repr(C)]
pub struct RivalContext {
    machine: MachineBox,
    last_iterations: u32,
}

#[no_mangle]
pub unsafe extern "C" fn rival_compile(
    vars: *const c_char,
    exprs: *const c_char,
    precision: u32,
) -> *mut RivalContext {
    let vars_str = from_c_str_lossy(vars);
    let exprs_str = from_c_str_lossy(exprs);
    let vars_sexpr = parse_sexpr(&vars_str).unwrap_or(SExpr::List(vec![]));
    let vars = extract_vars(&vars_sexpr).unwrap_or_else(|_| Vec::new());
    let exprs_sexpr = parse_sexpr(&exprs_str).unwrap_or(SExpr::List(vec![]));
    let exprs = extract_exprs(&exprs_sexpr, &vars).unwrap_or_else(|_| vec![eval::ops::Expr::Literal(0.0)]);

    let machine = if precision == 53 {
        let disc = Fp64Discretization;
        MachineBox::Fp64(MachineBuilder::new(disc).build(exprs, vars))
    } else {
        let disc = BfDiscretization { precision };
        MachineBox::Bf(MachineBuilder::new(disc).build(exprs, vars))
    };
    Box::into_raw(Box::new(RivalContext { machine, last_iterations: 0 }))
}

#[no_mangle]
pub unsafe extern "C" fn rival_destroy(ctx: *mut RivalContext) {
    drop(Box::from_raw(ctx))
}

#[no_mangle]
pub unsafe extern "C" fn rival_instruction_count(ctx: *mut RivalContext) -> u32 {
    let ctx = &*ctx;
    ctx.machine.instruction_count() as u32
}

#[no_mangle]
pub unsafe extern "C" fn rival_bumps(ctx: *mut RivalContext) -> u32 {
    let ctx = &*ctx;
    ctx.machine.bumps() as u32
}

#[no_mangle]
pub unsafe extern "C" fn rival_apply(
    ctx: *mut RivalContext,
    point_ptr: *const f64,
    point_len: u32,
    max_iterations: u32,
    out_len_ptr: *mut u32,
) -> *mut f64 {
    let ctx = &mut *ctx;
    let n = point_len as usize;
    let point = std::slice::from_raw_parts(point_ptr, n);
    let (iters, ivals) = ctx.machine.load_args_and_run(point, max_iterations as usize);
    ctx.last_iterations = iters;
    let mut out: Vec<f64> = Vec::with_capacity(ivals.len());
    for iv in ivals.iter() {
        // Best-effort: take midpoint as a scalar representative
        let lo = iv.lo.as_float().to_f64();
        let hi = iv.hi.as_float().to_f64();
        let mid = if lo == hi { lo } else { 0.5f64 * (lo + hi) };
        out.push(mid);
    }
    ptr::write(out_len_ptr, out.len() as u32);
    let ptr = out.as_mut_ptr();
    std::mem::forget(out);
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn destroy_f64_array(ptr: *mut f64, len: u32) {
    drop(Vec::from_raw_parts(ptr, len as usize, len as usize))
}

#[no_mangle]
pub unsafe extern "C" fn rival_last_iterations(ctx: *mut RivalContext) -> u32 {
    let ctx = &*ctx;
    ctx.last_iterations
}
