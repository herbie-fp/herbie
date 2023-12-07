#[cfg(target_arch = "x86_64")]
use std::{
    arch::x86_64::{_mm_setzero_pd, _mm_set_pd, _mm_rcp14_sd},
    simd::f64x2
};

#[cfg(target_arch = "x86_64")]
pub fn reciprocal(x: f64) -> f64 {
    unsafe {
        let a = _mm_setzero_pd();
        let b = _mm_set_pd(x, 0.0);
        f64x2::from(_mm_rcp14_sd(a, b)).as_array()[0]
    }
}
