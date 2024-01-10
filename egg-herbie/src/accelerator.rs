#[cfg(target_arch = "x86_64")]
use std::{
    arch::x86_64::{_mm_setzero_pd, _mm_set_pd, _mm_rcp14_sd},
    simd::f64x2
};

#[cfg(target_arch = "x86_64")]
// TODO: NOT A f64! f32 man!
pub fn reciprocal(x: f64) -> f64 {
    unsafe {
        // TODO: This is what we want _mm_rcp_ss, for both f32 and f64
        let a = _mm_setzero_pd();
        let b = _mm_set_pd(x, 0.0);
        // TODO: Look at assembly with godbolt
        // TODO: Don't use portable assembly? Try different core features, not calling instruction but actuually using call
        // TODO: asm!?
        // TODO: First test, feed it (/ 1 x) as fpcore, see output
        f64x2::from(_mm_rcp14_sd(a, b)).as_array()[0]
    }
}
