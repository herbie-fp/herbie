pub mod math;
pub mod rules;

use egg::{Extractor, Id, Iteration, Language};
use indexmap::IndexMap;
use math::*;

use std::cmp::min;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::time::Duration;
use std::{slice, sync::atomic::Ordering};

unsafe fn cstring_to_recexpr(c_string: *const c_char) -> Option<RecExpr> {
    match CStr::from_ptr(c_string).to_str() {
        Ok(expr_string) => match expr_string.parse() {
            Ok(expr) => Some(expr),
            Err(err) => {
                eprintln!("{}", err);
                None
            }
        },
        Err(_error) => None,
    }
}

pub struct Context {
    iteration: usize,
    runner: Option<Runner>,
}

// I had to add $(rustc --print sysroot)/lib to LD_LIBRARY_PATH to get linking to work after installing rust with rustup
#[no_mangle]
pub unsafe extern "C" fn egraph_create() -> *mut Context {
    Box::into_raw(Box::new(Context {
        iteration: 0,
        runner: Some(Runner::new(Default::default())),
    }))
}

#[no_mangle]
pub unsafe extern "C" fn egraph_destroy(ptr: *mut Context) {
    std::mem::drop(Box::from_raw(ptr))
}

#[no_mangle]
pub unsafe extern "C" fn egraph_addresult_destroy(ptr: *mut EGraphAddResult) {
    std::mem::drop(Box::from_raw(ptr))
}

#[no_mangle]
pub unsafe extern "C" fn destroy_egraphiters(size: u32, ptr: *mut EGraphIter) {
    let _array: &[EGraphIter] = slice::from_raw_parts(ptr, size as usize);
}

#[no_mangle]
pub unsafe extern "C" fn destroy_string(ptr: *mut c_char) {
    let _str = CString::from_raw(ptr);
}

// a struct to report failure if the add fails
#[repr(C)]
pub struct EGraphAddResult {
    id: u32,
    successp: bool,
}

#[repr(C)]
pub struct EGraphIter {
    numnodes: u32,
    numclasses: u32,
    time: f64,
}

// a struct for loading rules from external source
#[repr(C)]
pub struct FFIRule {
    name: *const c_char,
    left: *const c_char,
    right: *const c_char,
}

fn ffirun<F, T>(f: F) -> T
where
    F: FnOnce() -> T,
{
    let f = std::panic::AssertUnwindSafe(f);
    match std::panic::catch_unwind(f) {
        Ok(t) => t,
        Err(_) => {
            eprintln!("Caught a panic, aborting!");
            std::process::abort()
        }
    }
}

fn convert_iter(iter: &Iteration<IterData>) -> EGraphIter {
    EGraphIter {
        numnodes: iter.egraph_nodes as u32,
        numclasses: iter.egraph_classes as u32,
        time: iter.total_time,
    }
}

unsafe fn runner_egraphiters(runner: &Runner) -> *mut EGraphIter {
    let mut result: Vec<EGraphIter> = runner.iterations.iter().map(convert_iter).collect();
    let ptr = result.as_mut_ptr();
    std::mem::forget(result);
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn egraph_add_expr(
    ptr: *mut Context,
    expr: *const c_char,
) -> *mut EGraphAddResult {
    ffirun(|| {
        let _ = env_logger::try_init();
        let ctx = &mut *ptr;
        let mut runner = ctx
            .runner
            .take()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        assert_eq!(ctx.iteration, 0);

        let result = match cstring_to_recexpr(expr) {
            None => EGraphAddResult {
                id: 0,
                successp: false,
            },
            Some(rec_expr) => {
                runner = runner.with_expr(&rec_expr);
                let id = *runner.roots.last().unwrap();
                let id = usize::from(id) as u32;
                EGraphAddResult { id, successp: true }
            }
        };

        ctx.runner = Some(runner);
        Box::into_raw(Box::new(result))
    })
}

unsafe fn ptr_to_string(ptr: *const i8) -> String {
    let bytes = CStr::from_ptr(ptr).to_bytes();
    String::from_utf8(bytes.to_vec()).unwrap()
}

// todo don't just unwrap, also make sure the rules are validly parsed
unsafe fn ffirule_to_tuple(rule_ptr: *mut FFIRule) -> (String, String, String) {
    let rule = &mut *rule_ptr;
    (
        ptr_to_string(rule.name),
        ptr_to_string(rule.left),
        ptr_to_string(rule.right),
    )
}

#[no_mangle]
pub unsafe extern "C" fn egraph_run_with_iter_limit(
    ptr: *mut Context,
    output_size: *mut u32,
    iter_limit: u32,
    node_limit: u32,
    rules_array_ptr: *const *mut FFIRule,
    is_constant_folding_enabled: bool,
    rules_array_length: u32,
) -> *const EGraphIter {
    ffirun(|| {
        let ctx = &mut *ptr;
        let mut runner = ctx
            .runner
            .take()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        if runner.stop_reason.is_none() {
            let length: usize = rules_array_length as usize;
            let ffi_rules: &[*mut FFIRule] = slice::from_raw_parts(rules_array_ptr, length);
            let mut ffi_tuples: Vec<(&str, &str, &str)> = vec![];
            let mut ffi_strings: Vec<(String, String, String)> = vec![];
            for ffi_rule in ffi_rules.iter() {
                let str_tuple = ffirule_to_tuple(*ffi_rule);
                ffi_strings.push(str_tuple);
            }

            for ffi_string in ffi_strings.iter() {
                ffi_tuples.push((&ffi_string.0, &ffi_string.1, &ffi_string.2));
            }

            let rules: Vec<Rewrite> = rules::mk_rules(&ffi_tuples);

            runner.egraph.analysis.constant_fold = is_constant_folding_enabled;
            runner = runner
                .with_node_limit(node_limit as usize)
                .with_iter_limit(iter_limit as usize) // should never hit
                .with_time_limit(Duration::from_secs(u64::MAX))
                .with_hook(|r| {
                    if r.egraph.analysis.unsound.load(Ordering::SeqCst) {
                        Err("Unsoundness detected".into())
                    } else {
                        Ok(())
                    }
                })
                .run(&rules);
        }
        std::ptr::write(output_size, runner.iterations.len() as u32);
        let res = runner_egraphiters(&runner);
        ctx.runner = Some(runner);
        res
    })
}

#[no_mangle]
pub unsafe extern "C" fn egraph_run(
    ptr: *mut Context,
    output_size: *mut u32,
    node_limit: u32,
    rules_array_ptr: *const *mut FFIRule,
    is_constant_folding_enabled: bool,
    rules_array_length: u32,
) -> *const EGraphIter {
    egraph_run_with_iter_limit(
        ptr,
        output_size,
        u32::MAX,
        node_limit,
        rules_array_ptr,
        is_constant_folding_enabled,
        rules_array_length,
    )
}

fn newest_sound_iter(runner: &Runner, iter: u32) -> u32 {
    if runner.egraph.analysis.unsound.load(Ordering::SeqCst) {
        // go back one more iter, egg can duplicate the final iter in the case of an error
        min(runner.iterations.len().saturating_sub(3) as u32, iter)
    } else {
        min(runner.iterations.len().saturating_sub(1) as u32, iter)
    }
}

fn find_extracted(runner: &Runner, id: u32, iter: u32) -> &Extracted {
    let id = runner.egraph.find(Id::from(id as usize));
    let sound_iter = newest_sound_iter(runner, iter) as usize;

    runner.iterations[sound_iter]
        .data
        .extracted
        .iter()
        .find(|(i, _)| runner.egraph.find(*i) == id)
        .map(|(_, ext)| ext)
        .expect("Couldn't find matching extraction!")
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_simplest(
    ptr: *mut Context,
    node_id: u32,
    iter: u32,
) -> *const c_char {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        let ext = find_extracted(runner, node_id, iter);

        let best_str = CString::new(ext.best.to_string()).unwrap();
        let best_str_pointer = best_str.as_ptr();
        std::mem::forget(best_str);
        best_str_pointer
    })
}

fn get_op_str(n: &Math) -> Option<String> {
    match n {
        Math::Add(_) => Some(String::from("+")),
        Math::Sub(_) => Some(String::from("-")),
        Math::Mul(_) => Some(String::from("*")),
        Math::Div(_) => Some(String::from("/")),
        Math::Pow(_) => Some(String::from("pow")),
        Math::Neg(_) => Some(String::from("neg")),
        Math::Sqrt(_) => Some(String::from("sqrt")),
        Math::Fabs(_) => Some(String::from("fabs")),
        Math::Ceil(_) => Some(String::from("ceil")),
        Math::Floor(_) => Some(String::from("floor")),
        Math::Round(_) => Some(String::from("round")),
        Math::Log(_) => Some(String::from("log")),
        Math::Cbrt(_) => Some(String::from("cbrt")),
        Math::Other(s, _) => Some(s.to_string()),
        _ => None,
    }
}

fn variant_get_simplest(
    extractor: &mut Extractor<AltCost, Math, ConstantFold>,
    cache: &mut IndexMap<Id, String>,
    id: &Id,
) -> String {
    if let Some(v) = cache.get(id) {
        v.clone()
    } else {
        let (_, expr) = extractor.find_best(*id);
        let s = expr.to_string();
        cache.insert(*id, s.clone());
        s
    }
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_variants(
    ptr: *mut Context,
    node_id: u32,
    orig_expr: *const c_char,
) -> *const c_char {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        // root (id, expr)
        let id = Id::from(node_id as usize);
        let orig_recexpr =
            cstring_to_recexpr(orig_expr).unwrap_or_else(|| panic!("could not parse expr"));
        let head_node = &orig_recexpr.as_ref()[orig_recexpr.as_ref().len() - 1];

        // cache
        let mut extractor = Extractor::new(&runner.egraph, AltCost::new(&runner.egraph, vec![]));
        let mut cache: IndexMap<Id, String> = Default::default();

        // extract
        let mut exprs = vec![variant_get_simplest(&mut extractor, &mut cache, &id)];
        for n in &runner.egraph[Id::from(node_id as usize)].nodes {
            // assuming same ops in an eclass cannot
            // have different precisions
            if !n.matches(head_node) {
                match n {
                    // binary
                    Math::Add([p, i, j])
                    | Math::Sub([p, i, j])
                    | Math::Mul([p, i, j])
                    | Math::Div([p, i, j])
                    | Math::Pow([p, i, j]) => {
                        exprs.push(format!(
                            "({} {} {} {})",
                            get_op_str(n).unwrap(),
                            variant_get_simplest(&mut extractor, &mut cache, p),
                            variant_get_simplest(&mut extractor, &mut cache, i),
                            variant_get_simplest(&mut extractor, &mut cache, j)
                        ));
                    }

                    // unary
                    Math::Neg([p, i])
                    | Math::Sqrt([p, i])
                    | Math::Fabs([p, i])
                    | Math::Ceil([p, i])
                    | Math::Floor([p, i])
                    | Math::Round([p, i])
                    | Math::Log([p, i])
                    | Math::Cbrt([p, i]) => {
                        exprs.push(format!(
                            "({} {} {})",
                            get_op_str(n).unwrap(),
                            variant_get_simplest(&mut extractor, &mut cache, p),
                            variant_get_simplest(&mut extractor, &mut cache, i),
                        ));
                    }

                    // constants
                    Math::Constant(c) => exprs.push(c.to_string()),
                    Math::Symbol(s) => exprs.push(s.to_string()),

                    // variary
                    Math::Other(s, args) => {
                        // safe to assume at least one argument
                        let mut expr = format!(
                            "({} {}",
                            s,
                            variant_get_simplest(&mut extractor, &mut cache, &args[0])
                        );
                        for i in &args[1..] {
                            expr.push_str(&format!(
                                " {}",
                                variant_get_simplest(&mut extractor, &mut cache, i)
                            ));
                        }
                        expr.push(')');
                        exprs.push(expr);
                    }
                }
            }
        }

        // format
        let best_str = CString::new(exprs.join(" ")).unwrap();
        let best_str_pointer = best_str.as_ptr();
        std::mem::forget(best_str);
        best_str_pointer
    })
}

#[no_mangle]
pub unsafe extern "C" fn egraph_is_unsound_detected(ptr: *mut Context) -> bool {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));
        runner.egraph.analysis.unsound.load(Ordering::SeqCst)
    })
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_times_applied(ptr: *mut Context, name: *const i8) -> u32 {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));
        let string = ptr_to_string(name);
        runner
            .iterations
            .iter()
            .map(|iter| *iter.applied.get(&string).unwrap_or(&0) as u32)
            .sum()
    })
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_cost(ptr: *mut Context, node_id: u32, iter: u32) -> u32 {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        let ext = find_extracted(runner, node_id, iter);
        ext.cost as u32
    })
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_size(ptr: *mut Context) -> u32 {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));
        match runner.iterations.last() {
            None => 0,
            Some(iter) => iter.egraph_nodes as u32,
        }
    })
}
