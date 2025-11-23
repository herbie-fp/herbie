use rival::Discretization;
use rug::Float;

#[derive(Clone, Copy, Debug)]
pub enum DiscretizationType {
    Bool,
    F32,
    F64,
}

#[derive(Clone)]
pub struct RustDiscretization {
    pub target: u32,
    pub types: Vec<DiscretizationType>,
}

impl Discretization for RustDiscretization {
    fn target(&self) -> u32 {
        self.target
    }

    fn convert(&self, idx: usize, v: &Float) -> Float {
        match self.types[idx] {
            DiscretizationType::Bool => {
                if v.is_zero() {
                    Float::with_val(v.prec(), 0.0)
                } else {
                    Float::with_val(v.prec(), 1.0)
                }
            }
            DiscretizationType::F32 => {
                let f = v.to_f32();
                Float::with_val(v.prec(), f)
            }
            DiscretizationType::F64 => {
                let f = v.to_f64();
                Float::with_val(v.prec(), f)
            }
        }
    }

    fn distance(&self, idx: usize, lo: &Float, hi: &Float) -> usize {
        match self.types[idx] {
            DiscretizationType::Bool => {
                let lo_bool = !lo.is_zero();
                let hi_bool = !hi.is_zero();
                if lo_bool == hi_bool {
                    0
                } else {
                    2
                }
            }
            DiscretizationType::F32 => {
                let lo_f = lo.to_f32();
                let hi_f = hi.to_f32();
                let lo_ord = f32_ordinal(lo_f);
                let hi_ord = f32_ordinal(hi_f);
                (hi_ord - lo_ord).abs() as usize
            }
            DiscretizationType::F64 => {
                let lo_f = lo.to_f64();
                let hi_f = hi.to_f64();
                let lo_ord = f64_ordinal(lo_f);
                let hi_ord = f64_ordinal(hi_f);
                let diff = (hi_ord - lo_ord).abs();
                if diff > (usize::MAX as i128) {
                    usize::MAX
                } else {
                    diff as usize
                }
            }
        }
    }
}

fn f32_ordinal(x: f32) -> i64 {
    let u = x.abs().to_bits() as i64;
    if x.is_sign_negative() {
        -u
    } else {
        u
    }
}

fn f64_ordinal(x: f64) -> i128 {
    let u = x.abs().to_bits() as i128;
    if x.is_sign_negative() {
        -u
    } else {
        u
    }
}
