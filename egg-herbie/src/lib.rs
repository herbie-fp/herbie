pub mod math;
pub mod rules;

use egg::{Extractor, Id, Iteration, Language, StopReason, Symbol};
use egg_smol::{EGraph, ast::Expr};
use indexmap::IndexMap;
use math::*;

use std::collections::HashMap;

use num_rational::Rational64;

use std::cmp::min;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::time::Duration;
use std::{slice, sync::atomic::Ordering};
use symbolic_expressions::{Sexp, parser};

unsafe fn cstring_to_string(c_string: *const c_char) -> String {
    CStr::from_ptr(c_string).to_str().expect("Failed to unwrap c string!").to_string()
}

unsafe fn cstring_to_sexp(c_string: *const c_char) -> Option<Sexp>
{
    match parser::parse_str(&cstring_to_string(c_string)) {
        Ok(expr) => Some(expr),
        Err(err) => {
            None
        }
    }
}

unsafe fn cstring_to_recexpr(c_string: *const c_char) -> Option<RecExpr>
{
    match cstring_to_string(c_string).parse::<RecExpr>() {
        Ok(expr) => Some(expr),
        Err(err) => {
            None
        }
    }
}

pub struct Context {
    iteration: usize,
    runner: Option<Runner>,
    egglog: EGraph,
    egglog_gen: usize,
    rules: Vec<Rewrite>,
}

// I had to add $(rustc --print sysroot)/lib to LD_LIBRARY_PATH to get linking to work after installing rust with rustup
#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egraph_create() -> *mut Context {
    let egglog_header = std::include_str!("./herbie.egg");
    let mut egglog = EGraph::default();
    egglog.parse_and_run_program(egglog_header).unwrap();

    Box::into_raw(Box::new(Context {
        iteration: 0,
        runner: Some(Runner::new(Default::default()).with_explanations_enabled()),
        egglog,
        egglog_gen: 1,
        rules: vec![],
    }))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egraph_destroy(ptr: *mut Context) {
    std::mem::drop(Box::from_raw(ptr))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn destroy_egraphiters(size: u32, ptr: *mut EGraphIter) {
    std::mem::drop(Box::from_raw(slice::from_raw_parts_mut(ptr, size as usize)))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn destroy_string(ptr: *mut c_char) {
    std::mem::drop(CString::from_raw(ptr));
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

fn runner_egraphiters(runner: &Runner) -> *mut EGraphIter {
    let mut result: Vec<EGraphIter> = runner.iterations.iter().map(convert_iter).collect();
    let ptr = result.as_mut_ptr();
    std::mem::forget(result);
    ptr
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egraph_add_expr(ptr: *mut Context, expr: *const c_char) -> u32 {
    ffirun(|| {
        let _ = env_logger::try_init();
        let ctx = &mut *ptr;
        let mut runner = ctx
            .runner
            .take()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        assert_eq!(ctx.iteration, 0);

        let result = match cstring_to_recexpr(expr) {
            None => 0,
            Some(rec_expr) => {
                runner = runner.with_expr(&rec_expr);
                let id = *runner.roots.last().unwrap();
                let id = usize::from(id) as u32;
                assert!(id < u32::MAX);
                id + 1
            }
        };

        ctx.runner = Some(runner);
        result
    })
}


#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egraph_add_expr_egglog(ptr: *mut Context, expr: *const c_char) -> u32 {
    ffirun(|| {
        let _ = env_logger::try_init();
        let ctx = &mut *ptr;

        assert_eq!(ctx.iteration, 0);

        let name = "eggvar_".to_string() + &ctx.egglog_gen.to_string();
        let content = cstring_to_string(expr);
        let expr = format!("(define {} {} :cost 10000000)", name, content);

        //println!("{}", expr);

        ctx.egglog.parse_and_run_program(&expr.to_string()).unwrap();

        ctx.egglog_gen += 1;
        (ctx.egglog_gen - 1) as u32
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
#[allow(clippy::missing_safety_doc)]
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
            ctx.rules = rules;

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
                .run(&ctx.rules);
        }
        std::ptr::write(output_size, runner.iterations.len() as u32);
        let res = runner_egraphiters(&runner);
        ctx.runner = Some(runner);
        res
    })
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
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

fn egglog_run_rules(egglog: &mut EGraph, node_limit: usize) -> (Duration, Duration, Duration) {
    egglog.node_limit = node_limit;
    egglog.match_limit = 500;
    let mut search = Duration::default();
    let mut apply = Duration::default();
    let mut rebuild: Duration = Duration::default();
    for _i in 0..100 {
        if egglog.num_tuples() > node_limit {
            break;
        }
        egglog.load_ruleset("analysis".into());
        let [s, a, r] = egglog.run_rules(3);
        search += s;
        apply += a;
        rebuild += r;
        egglog.clear_rules();

        if egglog.num_tuples() > node_limit {
            break;
        }
        egglog.load_ruleset("rules".into());
        let [s, a, r] = egglog.run_rules(1);
        search += s;
        apply += a;
        rebuild += r;
        egglog.clear_rules();
    }
    (search, apply, rebuild)
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egraph_run_egglog(
    ptr: *mut Context,
    output_size: *mut u32,
) -> *const EGraphIter {
    ffirun(|| {
        let ctx = &mut *ptr;

        let (search, apply, rebuild) = egglog_run_rules(&mut ctx.egglog, 8_000);

        let mut iters = vec![EGraphIter {
            numnodes: 0,
            numclasses: 0,
            time: search.as_secs_f64() + apply.as_secs_f64() + rebuild.as_secs_f64(),
        }];

        std::ptr::write(output_size, iters.len() as u32);
        let ptr = iters.as_mut_ptr();
        std::mem::forget(iters);
        ptr
    })
}

    
#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egraph_get_stop_reason(ptr: *mut Context) -> u32 {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        match runner.stop_reason {
            Some(StopReason::Saturated) => 0,
            Some(StopReason::IterationLimit(_)) => 1,
            Some(StopReason::NodeLimit(_)) => 2,
            Some(StopReason::Other(_)) => 3,
            _ => 4,
        }
    })
}

fn find_extracted(runner: &Runner, id: u32, iter: u32) -> &Extracted {
    let id = runner.egraph.find(Id::from(id as usize));

    // go back one more iter, egg can duplicate the final iter in the case of an error
    let sound_iter = if runner.egraph.analysis.unsound.load(Ordering::SeqCst) {
        min(runner.iterations.len().saturating_sub(3), iter as usize)
    } else {
        min(runner.iterations.len().saturating_sub(1), iter as usize)
    };

    runner.iterations[sound_iter]
        .data
        .extracted
        .iter()
        .find(|(i, _)| runner.egraph.find(*i) == id)
        .map(|(_, ext)| ext)
        .expect("Couldn't find matching extraction!")
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
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


#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egglog_get_simplest(
    ptr: *mut Context,
    node_id: u32,
) -> *const c_char {
    ffirun(|| {
        let ctx = &mut *ptr;
        let (_, value) = ctx.egglog.eval_expr(&Expr::var("eggvar_".to_string() + &node_id.to_string()), None, false).unwrap();
        let (_cost, extracted) = ctx.egglog.extract(value);

        let converted = parser::parse_str(&extracted.to_string()).unwrap();

        let best_str = CString::new(converted.to_string()).unwrap();
        let best_str_pointer = best_str.as_ptr();
        std::mem::forget(best_str);
        best_str_pointer
    })
}

pub fn excluded_operator(op: &str) -> bool {
    let excluded = std::collections::HashSet::from(["zero"]);
    excluded.contains(op)
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egglog_get_variants(
    ptr: *mut Context,
    node_id: u32,
    _orig_expr: *const c_char,
) -> *const c_char {
    ffirun(|| {
        let ctx = &mut *ptr;

        let (_, value) = ctx.egglog.eval_expr(&Expr::var("eggvar_".to_string() + &node_id.to_string()), None, false).unwrap();
        let exprs = ctx.egglog.extract_variants(value, 100_000).into_iter().filter_map(|expr| {
            let parsed = parser::parse_str(&expr.to_string()).unwrap();
            if let Sexp::List(inner) = &parsed {
                if inner.len() == 1 {
                    if let Sexp::String(v) = &inner[0] {
                        if v.starts_with("eggvar_") || excluded_operator(v) {
                            return None;
                        }
                    }
               }
            }
            Some(parsed)
        }).collect::<Vec<Sexp>>();

        // format
        let expr_strs: Vec<String> = exprs.iter().map(|r| r.to_string()).collect();
        
        /*for expr in &expr_strs {
            println!("variant  {}", expr);
        }*/
        let best_str = CString::new(expr_strs.join(" ")).unwrap();
        let best_str_pointer = best_str.as_ptr();
        std::mem::forget(best_str);
        best_str_pointer
    })
}




unsafe fn make_empty_string() -> *const c_char {
    let best_str = CString::new("".to_string()).unwrap();
    let best_str_pointer = best_str.as_ptr();
    std::mem::forget(best_str);
    best_str_pointer
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egraph_get_proof(
    ptr: *mut Context,
    expr: *const c_char,
    goal: *const c_char,
) -> *const c_char {
    ffirun(|| {
        let ctx = &mut *ptr;

        assert_eq!(ctx.iteration, 0);

        let expr_rec = match cstring_to_recexpr(expr) {
            None => {
                return make_empty_string();
            }
            Some(rec_expr) => rec_expr,
        };

        let goal_rec = match cstring_to_recexpr(goal) {
            None => {
                return make_empty_string();
            }
            Some(rec_expr) => rec_expr,
        };

        let mut runner = ctx
            .runner
            .take()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        let mut proof = runner.explain_equivalence(&expr_rec, &goal_rec);
        ctx.runner = Some(runner);
        let string = CString::new(proof.get_flat_string()).unwrap();
        let string_pointer = string.as_ptr();
        std::mem::forget(string);
        string_pointer
    })
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
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

        // extractor
        let extractor = Extractor::new(&runner.egraph, AltCost::new(&runner.egraph));
        let mut cache: IndexMap<Id, RecExpr> = Default::default();

        // extract variants
        let mut exprs = vec![];
        for n in &runner.egraph[id].nodes {
            // assuming same ops in an eclass cannot
            // have different precisions
            if !n.matches(head_node) {
                // extract if not in cache
                n.for_each(|id| {
                    if cache.get(&id).is_none() {
                        let (_, best) = extractor.find_best(id);
                        cache.insert(id, best);
                    }
                });

                exprs.push(n.join_recexprs(|id| cache.get(&id).unwrap().as_ref()));
            }
        }

        // format
        let expr_strs: Vec<String> = exprs.iter().map(|r| r.to_string()).collect();
        let best_str = CString::new(expr_strs.join(" ")).unwrap();
        let best_str_pointer = best_str.as_ptr();
        std::mem::forget(best_str);
        best_str_pointer
    })
}


#[no_mangle]
#[allow(clippy::missing_safety_doc)]
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
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn egraph_get_times_applied(ptr: *mut Context, name: *const i8) -> u32 {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));
        let sym = Symbol::from(ptr_to_string(name));
        runner
            .iterations
            .iter()
            .map(|iter| *iter.applied.get(&sym).unwrap_or(&0) as u32)
            .sum()
    })
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
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
#[allow(clippy::missing_safety_doc)]
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
