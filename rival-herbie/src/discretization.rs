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

    fn convert(&self, _idx: usize, v: &Float) -> Float {
        v.clone()
    }

    fn distance(&self, idx: usize, lo: &Float, hi: &Float) -> usize {
        let disc = self.types.get(idx).unwrap_or(&DiscretizationType::F64);
        match disc {
            DiscretizationType::Bool => {
                if lo.to_f64() == hi.to_f64() {
                    0
                } else {
                    2
                }
            }
            DiscretizationType::F32 => {
                let x = lo.to_f32();
                let y = hi.to_f32();
                if x == y {
                    return 0;
                }
                let to_ordinal = |v: f32| -> i32 {
                    let bits = v.to_bits() as i32;
                    if bits < 0 {
                        !bits
                    } else {
                        bits
                    }
                };
                let ox = to_ordinal(x);
                let oy = to_ordinal(y);
                oy.wrapping_sub(ox).unsigned_abs() as usize
            }
            DiscretizationType::F64 => {
                let x = lo.to_f64();
                let y = hi.to_f64();
                if x == y {
                    return 0;
                }
                let to_ordinal = |v: f64| -> i64 {
                    let bits = v.to_bits() as i64;
                    if bits < 0 {
                        !bits
                    } else {
                        bits
                    }
                };
                let ox = to_ordinal(x);
                let oy = to_ordinal(y);
                oy.wrapping_sub(ox).unsigned_abs() as usize
            }
        }
    }
}