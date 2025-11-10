//! Interval expression evaluation pipeline

pub mod adjust;
pub mod execute;
pub mod instructions;
pub mod machine;
pub mod macros;
pub mod ops;
pub mod run;
pub mod tricks;

pub mod ast {
    pub use super::ops::Expr;
}
