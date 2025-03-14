use std::mem::ManuallyDrop;
use std::os::raw::c_char;
use std::ffi::{CStr, CString};
use libc;

use egglog::{EGraph, TermDag};
use egglog::extract::Extractor;

/// Type of the context.
pub struct Context {
    egraph: EGraph
}

/// Allocates a new egraph that must be manually freed.
#[no_mangle]
pub extern "C" fn egraph_create() -> *mut Context {
    Box::into_raw(Box::new(Context {
        egraph: EGraph::default()
    }))
}

/// Manually frees an egraph.
#[no_mangle]
pub extern "C" fn egraph_free(ptr: *mut Context) {
    let ptr = unsafe {
        assert!(!ptr.is_null());
        Box::from_raw(ptr)
    };

    drop(ptr)
}

#[no_mangle]
pub extern "C" fn egraph_run(ptr: *mut Context, prog: *const c_char) -> *mut c_char {
    // Ensures that `context` will not be freed
    let mut context = unsafe {
        assert!(!ptr.is_null());
        ManuallyDrop::new(Box::from_raw(ptr))
    };

    // Convert `prog` to a Rust FFI string
    let cstr = unsafe {
        assert!(!prog.is_null());
        CStr::from_ptr(prog)
    };

    // Run the 
    let prog = cstr.to_str().unwrap();
    let egraph = &mut context.egraph;
    let result = egraph.parse_and_run_program(None, prog).unwrap();

    // Concatenate the result in a string
    let mut string = String::new();
    for s in result {
        string.push_str(&s);
    }

    // Return the result as a string
    let cstring = CString::new(string).unwrap();
    let ptr = cstring.into_raw();
    std::mem::forget(context);

    ptr
}

#[no_mangle]
pub extern "C" fn egraph_extract(ptr: *mut Context, vec: *const *mut c_char, len: usize) -> *mut c_char {
    // Ensures that `context` will not be freed
    let mut context = unsafe {
        assert!(!ptr.is_null());
        ManuallyDrop::new(Box::from_raw(ptr))
    };

    // Convert `vec` to a Rust FFI string
    let terms = unsafe {
        assert!(!vec.is_null());
        let vec = std::slice::from_raw_parts(vec, len);
        vec.iter().map(|&ptr| {
            assert!(!ptr.is_null());
            CStr::from_ptr(ptr)
        }).map(|cstr| {
            cstr.to_str().unwrap()
        }).collect::<Vec<&str>>()
    };

    // Parse terms into their sort and value
    let terms: Vec<_> = terms.iter().map(|term| {
        let expr = context.egraph.parser.get_expr_from_string(None, term).unwrap();
        context.egraph.eval_expr(&expr).unwrap()
    }).collect();


    // Extract the terms
    let mut string = String::new();
    let mut termdag = TermDag::default();
    let extractor = Extractor::new(&context.egraph, &mut termdag);
    for (sort, value) in terms {
        let (_, extracted) = extractor.find_best(value, &mut termdag, &sort).unwrap();
        let s = termdag.to_string(&extracted);
        string.push_str(&s);
    }

    // Return the result as a string
    let cstring = CString::new(string).unwrap();
    let ptr = cstring.into_raw();
    std::mem::forget(context);

    ptr
}

#[no_mangle]
pub extern "C" fn string_free(ptr: *mut c_char) {
    let ptr = unsafe {
        assert!(!ptr.is_null());
        CString::from_raw(ptr)
    };

    drop(ptr);
}

#[no_mangle]
pub extern "C" fn string_length(ptr: *const c_char) -> u32 {
    let len = unsafe {
        assert!(!ptr.is_null());
        libc::strlen(ptr)
    };

    return len as u32
}
