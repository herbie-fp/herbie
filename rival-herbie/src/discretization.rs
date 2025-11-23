use rival::Discretization;
use rug::Float;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

// Callback types
// convert: (idx, val_str) -> val_str
pub type ConvertCallback = extern "C" fn(usize, *const c_char) -> *mut c_char;
// distance: (idx, lo_str, hi_str) -> usize
pub type DistanceCallback = extern "C" fn(usize, *const c_char, *const c_char) -> usize;
// free_string: (str) -> void
pub type FreeStringCallback = extern "C" fn(*mut c_char);

#[derive(Clone)]
pub struct CallbackDiscretization {
    pub target: u32,
    pub convert_cb: ConvertCallback,
    pub distance_cb: DistanceCallback,
    pub free_cb: FreeStringCallback,
}

impl Discretization for CallbackDiscretization {
    fn target(&self) -> u32 {
        self.target
    }

    fn convert(&self, idx: usize, v: &Float) -> Float {
        let s = CString::new(v.to_string_radix(10, None)).unwrap();
        let res_ptr = (self.convert_cb)(idx, s.as_ptr());
        if res_ptr.is_null() {
            // Should not happen if Racket side is correct
            return v.clone();
        }
        let res_str = unsafe { CStr::from_ptr(res_ptr).to_string_lossy() };
        let res_float = Float::parse(&*res_str).unwrap();
        let res = Float::with_val(v.prec(), res_float);

        // Free the string returned by Racket
        (self.free_cb)(res_ptr);

        res
    }

    fn distance(&self, idx: usize, lo: &Float, hi: &Float) -> usize {
        let s_lo = CString::new(lo.to_string_radix(10, None)).unwrap();
        let s_hi = CString::new(hi.to_string_radix(10, None)).unwrap();
        (self.distance_cb)(idx, s_lo.as_ptr(), s_hi.as_ptr())
    }
}
