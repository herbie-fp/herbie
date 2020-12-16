pub mod math;
pub mod rules;

use egg::Id;
use math::*;

use std::ffi::{CStr, CString};
use std::os::raw::c_char;
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

// a struct to report failure if the add fails
#[repr(C)]
pub struct EGraphAddResult {
    id: u32,
    successp: bool,
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

// todo don't just unwrap, also make sure the rules are validly parsed
unsafe fn ffirule_to_tuple(rule_ptr: *mut FFIRule) -> (String, String, String) {
    let rule = &mut *rule_ptr;
    let bytes1 = CStr::from_ptr(rule.name).to_bytes();
    let string_result1 = String::from_utf8(bytes1.to_vec()).unwrap();
    let bytes2 = CStr::from_ptr(rule.left).to_bytes();
    let string_result2 = String::from_utf8(bytes2.to_vec()).unwrap();
    let bytes3 = CStr::from_ptr(rule.right).to_bytes();
    let string_result3 = String::from_utf8(bytes3.to_vec()).unwrap();
    (string_result1, string_result2, string_result3)
}

#[no_mangle]
pub unsafe extern "C" fn egraph_run_iter(
    ptr: *mut Context,
    limit: u32,
    rules_array_ptr: *const *mut FFIRule,
    is_constant_folding_enabled: bool,
    rules_array_length: u32,
) {
    ffirun(|| {
        let ctx = &mut *ptr;
        let mut runner = ctx
            .runner
            .take()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        if runner.stop_reason.is_some() {
            // we've already run, just fake an iteration
            ctx.iteration += 1;
        } else {
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
                .with_node_limit(limit as usize)
                .with_hook(|r| {
                    if r.egraph.analysis.unsound.load(Ordering::SeqCst) {
                        Err("Unsoundness detected".into())
                    } else {
                        Ok(())
                    }
                })
                .run(&rules);
        }
        ctx.runner = Some(runner);
    })
}

fn find_extracted(runner: &Runner, id: u32) -> &Extracted {
    let id = runner.egraph.find(Id::from(id as usize));
    let desired_iter = if runner.egraph.analysis.unsound.load(Ordering::SeqCst) {
        // go back one more iter, add egg can duplicate the final iter in the case of an error
        runner.iterations.len().saturating_sub(3)
    } else {
        runner.iterations.len().saturating_sub(1)
    };

    runner.iterations[desired_iter]
        .data
        .extracted
        .iter()
        .find(|(i, _)| runner.egraph.find(*i) == id)
        .map(|(_, ext)| ext)
        .expect("Couldn't find matching extraction!")
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_simplest(ptr: *mut Context, node_id: u32) -> *const c_char {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        let ext = find_extracted(runner, node_id);

        let best_str = CString::new(ext.best.to_string()).unwrap();
        let best_str_pointer = best_str.as_ptr();
        std::mem::forget(best_str);
        best_str_pointer
    })
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_cost(ptr: *mut Context, node_id: u32) -> u32 {
    ffirun(|| {
        let ctx = &*ptr;
        let runner = ctx
            .runner
            .as_ref()
            .unwrap_or_else(|| panic!("Runner has been invalidated"));

        let ext = find_extracted(runner, node_id);
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
