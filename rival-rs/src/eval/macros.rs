//! Macro for defining interval operations and generated helpers
//! Provides enums, dispatch, optimization, and path reduction wiring
#[macro_export]
macro_rules! def_ops {
    (
        constant {
            $( $const_name:ident: { method: $const_method:ident $(,)? } ),* $(,)?
        },
        unary {
            $(
                $unary_name:ident: {
                    method: $unary_method:ident,
                    bounds: $unary_bounds:expr
                    $(, path_reduce: $unary_path_reduce:expr )?
                    $(, optimize: $unary_optimize:expr )?
                    $(,)?
                }
            ),* $(,)?
        },
        unary_param {
            $(
                $unary_param_name:ident: {
                    method: $unary_param_method:ident,
                    bounds: $unary_param_bounds:expr
                    $(, path_reduce: $unary_param_path_reduce:expr )?
                    $(, optimize: $unary_param_optimize:expr )?
                    $(,)?
                }
            ),* $(,)?
        },
        binary {
            $(
                $binary_name:ident: {
                    method: $binary_method:ident,
                    bounds: $binary_bounds:expr
                    $(, path_reduce: $binary_path_reduce:expr )?
                    $(, optimize: $binary_optimize:expr )?
                    $(,)?
                }
            ),* $(,)?
        },
        ternary {
            $(
                $ternary_name:ident: {
                    method: $ternary_method:ident,
                    bounds: $ternary_bounds:expr
                    $(, path_reduce: $ternary_path_reduce:expr )?
                    $(, optimize: $ternary_optimize:expr )?
                    $(,)?
                }
            ),* $(,)?
        } $(,)?
    ) => {
        /// High-level expression AST
        #[derive(Debug, Clone, PartialEq)]
        pub enum Expr {
            /// Variable reference
            Var(String),
            /// Literal value
            Literal(f64),
            /// Rational value with sign flag and positive denominator
            Rational { num: u64, den: u64, neg: bool },

            // Constants
            $( $const_name, )*

            // Unary operations
            $( $unary_name(Box<Expr>), )*

            // Unary parameterized operations (take a u64 parameter and an expression)
            $( $unary_param_name(u64, Box<Expr>), )*

            // Binary operations
            $( $binary_name(Box<Expr>, Box<Expr>), )*

            // Ternary operations
            $( $ternary_name(Box<Expr>, Box<Expr>, Box<Expr>), )*
        }

        /// Unary instruction operations
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum UnaryOp {
            $( $unary_name, )*
        }

        /// Unary parameterized instruction operations
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum UnaryParamOp {
            $( $unary_param_name, )*
        }

        /// Binary instruction operations
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum BinaryOp {
            $( $binary_name, )*
        }

        /// Ternary instruction operations
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum TernaryOp {
            $( $ternary_name, )*
        }

        /// Constant operations
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum ConstantOp {
            $( $const_name, )*
        }

        /// Compute error amplification bounds for unary operations
        pub fn bounds_for_unary(
            ctx: &TrickContext,
            op: UnaryOp,
            output: &Ival,
            input: &Ival,
        ) -> AmplBounds {
            match op {
                $(
                    UnaryOp::$unary_name => {
                        let bounds_fn: fn(&TrickContext, &Ival, &Ival) -> AmplBounds = $unary_bounds;
                        bounds_fn(ctx, output, input)
                    }
                )*
            }
        }

        /// Compute error amplification bounds for binary operations
        pub fn bounds_for_binary(
            ctx: &TrickContext,
            op: BinaryOp,
            output: &Ival,
            lhs: &Ival,
            rhs: &Ival,
        ) -> (AmplBounds, AmplBounds) {
            match op {
                $(
                    BinaryOp::$binary_name => {
                        let bounds_fn: fn(&TrickContext, &Ival, &Ival, &Ival) -> (AmplBounds, AmplBounds) = $binary_bounds;
                        bounds_fn(ctx, output, lhs, rhs)
                    }
                )*
            }
        }

        /// Compute error amplification bounds for ternary operations
        pub fn bounds_for_ternary(
            ctx: &TrickContext,
            op: TernaryOp,
            output: &Ival,
            arg1: &Ival,
            arg2: &Ival,
            arg3: &Ival,
        ) -> (AmplBounds, AmplBounds, AmplBounds) {
            match op {
                $(
                    TernaryOp::$ternary_name => {
                        let bounds_fn: fn(&TrickContext, &Ival, &Ival, &Ival, &Ival) -> (AmplBounds, AmplBounds, AmplBounds) = $ternary_bounds;
                        bounds_fn(ctx, output, arg1, arg2, arg3)
                    }
                )*
            }
        }

        /// Compute error amplification bounds for unary parameterized operations
        pub fn bounds_for_unary_param(
            ctx: &TrickContext,
            op: UnaryParamOp,
            param: u64,
            output: &Ival,
            input: &Ival,
        ) -> AmplBounds {
            match op {
                $(
                    UnaryParamOp::$unary_param_name => {
                        let bounds_fn: fn(&TrickContext, u64, &Ival, &Ival) -> AmplBounds = $unary_param_bounds;
                        bounds_fn(ctx, param, output, input)
                    }
                )*
            }
        }

        /// Path reduction for unary operations
        pub fn path_reduce_unary<D, F>(
            op: UnaryOp,
            machine: &$crate::eval::machine::Machine<D>,
            idx: usize,
            mut mark: F,
        ) -> $crate::eval::machine::PathOutcome
        where
            D: $crate::eval::machine::Discretization,
            F: FnMut(usize),
        {
            match op {
                $(
                    UnaryOp::$unary_name => def_ops!(@path_reduce_impl machine, idx, mark ; $( $unary_path_reduce )?),
                )*
            }
        }

        /// Path reduction for binary operations
        pub fn path_reduce_binary<D, F>(
            op: BinaryOp,
            machine: &$crate::eval::machine::Machine<D>,
            idx: usize,
            mut mark: F,
        ) -> $crate::eval::machine::PathOutcome
        where
            D: $crate::eval::machine::Discretization,
            F: FnMut(usize),
        {
            match op {
                $(
                    BinaryOp::$binary_name => def_ops!(@path_reduce_impl machine, idx, mark ; $( $binary_path_reduce )?),
                )*
            }
        }

        /// Path reduction for ternary operations
        pub fn path_reduce_ternary<D, F>(
            op: TernaryOp,
            machine: &$crate::eval::machine::Machine<D>,
            idx: usize,
            mut mark: F,
        ) -> $crate::eval::machine::PathOutcome
        where
            D: $crate::eval::machine::Discretization,
            F: FnMut(usize),
        {
            match op {
                $(
                    TernaryOp::$ternary_name => def_ops!(@path_reduce_impl machine, idx, mark ; $( $ternary_path_reduce )?),
                )*
            }
        }

        /// Path reduction for unary parameterized operations
        pub fn path_reduce_unary_param<D, F>(
            op: UnaryParamOp,
            machine: &$crate::eval::machine::Machine<D>,
            idx: usize,
            mut mark: F,
        ) -> $crate::eval::machine::PathOutcome
        where
            D: $crate::eval::machine::Discretization,
            F: FnMut(usize),
        {
            match op {
                $(
                    UnaryParamOp::$unary_param_name => def_ops!(@path_reduce_impl machine, idx, mark ; $( $unary_param_path_reduce )?),
                )*
            }
        }

        /// Optimize unary operation
        pub fn optimize_unary(op: UnaryOp, arg: Expr) -> Expr {
            match op {
                $(
                    UnaryOp::$unary_name => def_ops!(@optimize_unary_impl $unary_name, arg ; $( $unary_optimize )?),
                )*
            }
        }

        /// Optimize binary operation
        pub fn optimize_binary(op: BinaryOp, lhs: Expr, rhs: Expr) -> Expr {
            match op {
                $(
                    BinaryOp::$binary_name => def_ops!(@optimize_binary_impl $binary_name, lhs, rhs ; $( $binary_optimize )?),
                )*
            }
        }

        /// Optimize ternary operation
        pub fn optimize_ternary(op: TernaryOp, arg1: Expr, arg2: Expr, arg3: Expr) -> Expr {
            match op {
                $(
                    TernaryOp::$ternary_name => def_ops!(@optimize_ternary_impl $ternary_name, arg1, arg2, arg3 ; $( $ternary_optimize )?),
                )*
            }
        }

        /// Optimize unary parameterized operation
        pub fn optimize_unary_param(op: UnaryParamOp, param: u64, arg: Expr) -> Expr {
            match op {
                $(
                    UnaryParamOp::$unary_param_name => def_ops!(@optimize_unary_param_impl $unary_param_name, param, arg ; $( $unary_param_optimize )?),
                )*
            }
        }

        /// Execute unary operation on interval
        pub fn execute_unary(op: UnaryOp, output: &mut Ival, input: &Ival) {
            match op {
                $(
                    UnaryOp::$unary_name => output.$unary_method(input),
                )*
            }
        }

        /// Execute binary operation on interval
        pub fn execute_binary(op: BinaryOp, output: &mut Ival, lhs: &Ival, rhs: &Ival) {
            match op {
                $(
                    BinaryOp::$binary_name => output.$binary_method(lhs, rhs),
                )*
            }
        }

        /// Execute ternary operation on interval
        pub fn execute_ternary(op: TernaryOp, output: &mut Ival, arg1: &Ival, arg2: &Ival, arg3: &Ival) {
            match op {
                $(
                    TernaryOp::$ternary_name => output.$ternary_method(arg1, arg2, arg3),
                )*
            }
        }

        /// Execute unary parameterized operation on interval
        pub fn execute_unary_param(op: UnaryParamOp, param: u64, output: &mut Ival, input: &Ival) {
            match op {
                $(
                    UnaryParamOp::$unary_param_name => output.$unary_param_method(input, param),
                )*
            }
        }

        /// Execute constant operation on interval
        pub fn execute_constant(op: ConstantOp, output: &mut Ival) {
            match op {
                $(
                    ConstantOp::$const_name => output.$const_method(),
                )*
            }
        }

        /// Lower expression to instruction, returns register index
        pub fn lower_expr(
            expr: &Expr,
            var_lookup: &std::collections::HashMap<&str, usize>,
            nodes: &mut indexmap::IndexMap<$crate::eval::instructions::InstructionData, usize>,
            current_reg: &mut usize,
        ) -> usize {
            // Add instruction with common subexpression elimination
            fn add_instruction(
                data: $crate::eval::instructions::InstructionData,
                nodes: &mut indexmap::IndexMap<$crate::eval::instructions::InstructionData, usize>,
                current_reg: &mut usize,
            ) -> usize {
                *nodes.entry(data).or_insert_with(|| {
                    let idx = *current_reg;
                    *current_reg += 1;
                    idx
                })
            }

            match expr {
                Expr::Var(name) => *var_lookup.get(name.as_str()).expect("Unknown variable"),
                Expr::Literal(value) => add_instruction($crate::eval::instructions::InstructionData::literal(*value), nodes, current_reg),
                Expr::Rational { num, den, neg } => add_instruction(
                    $crate::eval::instructions::InstructionData::Rational { num: *num, den: *den, neg: *neg },
                    nodes,
                    current_reg,
                ),

                // Constants
                $(
                    Expr::$const_name => add_instruction(
                        $crate::eval::instructions::InstructionData::constant(ConstantOp::$const_name),
                        nodes,
                        current_reg,
                    ),
                )*

                // Unary operations
                $(
                    Expr::$unary_name(arg) => {
                        let arg_reg = lower_expr(arg, var_lookup, nodes, current_reg);
                        add_instruction($crate::eval::instructions::InstructionData::unary(UnaryOp::$unary_name, arg_reg), nodes, current_reg)
                    }
                )*

                // Unary parameterized operations
                $(
                    Expr::$unary_param_name(param, arg) => {
                        let arg_reg = lower_expr(arg, var_lookup, nodes, current_reg);
                        add_instruction($crate::eval::instructions::InstructionData::unary_param(UnaryParamOp::$unary_param_name, *param, arg_reg), nodes, current_reg)
                    }
                )*

                // Binary operations
                $(
                    Expr::$binary_name(lhs, rhs) => {
                        let lhs_reg = lower_expr(lhs, var_lookup, nodes, current_reg);
                        let rhs_reg = lower_expr(rhs, var_lookup, nodes, current_reg);
                        add_instruction($crate::eval::instructions::InstructionData::binary(BinaryOp::$binary_name, lhs_reg, rhs_reg), nodes, current_reg)
                    }
                )*

                // Ternary operations
                $(
                    Expr::$ternary_name(arg1, arg2, arg3) => {
                        let reg1 = lower_expr(arg1, var_lookup, nodes, current_reg);
                        let reg2 = lower_expr(arg2, var_lookup, nodes, current_reg);
                        let reg3 = lower_expr(arg3, var_lookup, nodes, current_reg);
                        add_instruction($crate::eval::instructions::InstructionData::ternary(TernaryOp::$ternary_name, reg1, reg2, reg3), nodes, current_reg)
                    }
                )*
            }
        }

        /// Optimize expression for numerical stability
        pub fn optimize_expr(expr: Expr) -> Expr {
            // TODO: Consider passing the box straight to the optimize closures
            // so that we don't have to reallocate a new box every time we
            // fail to optimize (most likely scenario); we do the opposite here
            match expr {
                $(
                    Expr::$unary_name(x) => optimize_unary(UnaryOp::$unary_name, optimize_expr(*x)),
                )*

                $(
                    Expr::$unary_param_name(param, x) => optimize_unary_param(UnaryParamOp::$unary_param_name, param, optimize_expr(*x)),
                )*

                $(
                    Expr::$binary_name(x, y) => optimize_binary(BinaryOp::$binary_name, optimize_expr(*x), optimize_expr(*y)),
                )*

                $(
                    Expr::$ternary_name(x, y, z) => optimize_ternary(TernaryOp::$ternary_name, optimize_expr(*x), optimize_expr(*y), optimize_expr(*z)),
                )*

                // Leaves
                leaf @ (Expr::Var(_) | Expr::Literal(_) | Expr::Rational { .. } $( | Expr::$const_name )*) => leaf,
            }
        }
    };

    // Helper: Unary optimization - no optimizer specified
    (@optimize_unary_impl $op:ident, $arg:expr ; ) => {
        Expr::$op(Box::new($arg))
    };
    // Helper: Unary optimization - custom optimizer specified
    (@optimize_unary_impl $op:ident, $arg:expr ; $optimizer:expr) => {
        $optimizer($arg)
    };

    // Helper: Binary optimization - no optimizer specified
    (@optimize_binary_impl $op:ident, $lhs:expr, $rhs:expr ; ) => {
        Expr::$op(Box::new($lhs), Box::new($rhs))
    };
    // Helper: Binary optimization - custom optimizer specified
    (@optimize_binary_impl $op:ident, $lhs:expr, $rhs:expr ; $optimizer:expr) => {
        $optimizer($lhs, $rhs)
    };

    // Helper: Ternary optimization - no optimizer specified
    (@optimize_ternary_impl $op:ident, $arg1:expr, $arg2:expr, $arg3:expr ; ) => {
        Expr::$op(Box::new($arg1), Box::new($arg2), Box::new($arg3))
    };
    // Helper: Ternary optimization - custom optimizer specified
    (@optimize_ternary_impl $op:ident, $arg1:expr, $arg2:expr, $arg3:expr ; $optimizer:expr) => {
        $optimizer($arg1, $arg2, $arg3)
    };

    // Helper: Unary param optimization - no optimizer specified
    (@optimize_unary_param_impl $op:ident, $param:expr, $arg:expr ; ) => {
        Expr::$op($param, Box::new($arg))
    };
    // Helper: Unary param optimization - custom optimizer specified
    (@optimize_unary_param_impl $op:ident, $param:expr, $arg:expr ; $optimizer:expr) => {
        $optimizer($param, $arg)
    };

    // Helper: Path reduction - standard behavior if not specified
    (@path_reduce_impl $machine:expr, $idx:expr, $mark:expr ; ) => {
        def_ops!(@standard_path_reduce $machine, $idx, $mark)
    };
    // Helper: Path reduction - custom closure if specified
    (@path_reduce_impl $machine:expr, $idx:expr, $mark:expr ; $closure:expr) => {
        $closure($machine, $idx, $mark)
    };

    // Helper: Standard path reduction (mark all children and execute)
    (@standard_path_reduce $machine:expr, $idx:expr, $mark:expr) => {{
        let instruction = &$machine.state.instructions[$idx];
        let out_reg = $machine.instruction_register($idx);

        // Mark all children (except output register)
        instruction.for_each_input(|reg| {
            if reg != out_reg {
                $mark(reg);
            }
        });

        $crate::eval::machine::PathOutcome {
            hint: $crate::eval::machine::Hint::Execute,
            converged: true,
        }
    }};
}
