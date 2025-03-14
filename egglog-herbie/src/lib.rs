#![allow(clippy::missing_safety_doc)]

use std::ffi::{CStr, CString};
use std::mem::ManuallyDrop;
use std::os::raw::c_char;

use egglog::extract::Extractor;
use egglog::{EGraph, TermDag};

/// Type of the context.
pub struct Context {
    egraph: EGraph,
}

/// Allocates a new egraph that must be manually freed.
#[no_mangle]
pub extern "C" fn egraph_create() -> *mut Context {
    Box::into_raw(Box::new(Context {
        egraph: EGraph::default(),
    }))
}

/// Manually frees an egraph.
#[no_mangle]
pub unsafe extern "C" fn egraph_free(ptr: *mut Context) {
    assert!(!ptr.is_null());
    let ptr = Box::from_raw(ptr);
    drop(ptr)
}

fn _egraph_run(context: &mut Context, prog: &str) -> String {
    let egraph = &mut context.egraph;
    let result = egraph.parse_and_run_program(None, prog).unwrap();

    // Concatenate the result in a string
    let mut string = String::new();
    for s in result {
        string.push_str(&s);
    }

    string
}

#[no_mangle]
pub unsafe extern "C" fn egraph_run(ptr: *mut Context, prog: *const c_char) -> *const c_char {
    // Ensures that `context` will not be freed
    assert!(!ptr.is_null());
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));

    // Convert `prog` to a Rust FFI string
    assert!(!prog.is_null());
    let cstr = CStr::from_ptr(prog).to_str().unwrap();

    let string = _egraph_run(&mut context, cstr);

    // Return the result as a string
    let cstring = CString::new(string).unwrap();
    ManuallyDrop::new(cstring).as_ptr()
}

fn _egraph_extract(context: &mut Context, strs: &[*mut c_char]) -> String {
    // Convert `strs` to Rust FFI strings
    let terms: Vec<_> = strs
        .iter()
        .map(|&ptr| {
            assert!(!ptr.is_null());
            unsafe { CStr::from_ptr(ptr).to_str().unwrap() }
        })
        .collect();

    // Parse terms into their sort and value
    let terms: Vec<_> = terms
        .iter()
        .map(|term| {
            let expr = context
                .egraph
                .parser
                .get_expr_from_string(None, term)
                .unwrap();
            context.egraph.eval_expr(&expr).unwrap()
        })
        .collect();

    // Extract the terms
    let mut string = String::new();
    let mut termdag = TermDag::default();
    let extractor = Extractor::new(&context.egraph, &mut termdag);
    for (sort, value) in terms {
        let (_, extracted) = extractor.find_best(value, &mut termdag, &sort).unwrap();
        let s = termdag.to_string(&extracted);
        string.push_str(&s);
    }

    string
}

#[no_mangle]
pub unsafe extern "C" fn egraph_extract(
    ptr: *mut Context,
    vec: *const *mut c_char,
    len: usize,
) -> *const c_char {
    // Ensures that `context` will not be freed
    assert!(!ptr.is_null());
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));

    // Convert `vec` to a Rust FFI string
    assert!(!vec.is_null());
    let strs = std::slice::from_raw_parts(vec, len);

    // Run batched extraction
    let string = _egraph_extract(&mut context, strs);

    // Return the result as a string
    let cstring = CString::new(string).unwrap();
    ManuallyDrop::new(cstring).as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn string_free(ptr: *mut c_char) {
    assert!(!ptr.is_null());
    let ptr = CString::from_raw(ptr);
    drop(ptr);
}

#[no_mangle]
pub unsafe extern "C" fn string_length(ptr: *const c_char) -> u32 {
    assert!(!ptr.is_null());
    let len = libc::strlen(ptr);
    len as u32
}
