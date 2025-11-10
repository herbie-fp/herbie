//! Operation registry generating evaluation and optimization helpers
//! Defines interval operators along with dispatch, bounds, and path reduction hooks
use crate::def_ops;
use crate::eval::adjust::path_reduction;
use crate::eval::tricks::{AmplBounds, TrickContext, crosses_zero, get_slack};
use crate::interval::Ival;
use Expr::*;

def_ops! {
    constant {
        Pi: {
            method: set_pi,
        },

        E: {
            method: set_e,
        },
    },

    unary {
        Pow2: {
            method: pow2_assign,
            bounds: |ctx, _out, inp| {
                AmplBounds::new(ctx.logspan(inp) + 1, 0)
            },
        },
        Fabs: {
            method: fabs_assign,
            bounds: |_, _, _| AmplBounds::zero(),
        },

        Neg: {
            method: neg_assign,
            bounds: |_, _, _| AmplBounds::zero(),
        },

        Sqrt: {
            method: sqrt_assign,
            bounds: |ctx, _, inp| AmplBounds::new(ctx.logspan(inp) / 2, 0),
            optimize: |arg| {
                // sqrt(x^2 + y^2) => hypot(x, y)
                // sqrt(x^2 + 1) => hypot(x, 1)
                // sqrt(1 + x^2) => hypot(1, x)
                match arg {
                    Add(a, b) => match (&*a, &*b) {
                        // sqrt(x^2 + y^2)
                        (Mul(x1, x2), Mul(y1, y2)) if x1 == x2 && y1 == y2 => {
                            Hypot(x1.clone(), y1.clone())
                        }
                        // sqrt(x^2 + 1)
                        (Mul(x1, x2), Literal(one)) if x1 == x2 && (*one - 1.0).abs() == 0.0 => {
                            Hypot(x1.clone(), Box::new(Literal(*one)))
                        }
                        // sqrt(1 + x^2)
                        (Literal(one), Mul(x1, x2)) if x1 == x2 && (*one - 1.0).abs() == 0.0 => {
                            Hypot(Box::new(Literal(*one)), x1.clone())
                        }
                        _ => Sqrt(Box::new(Add(a, b))),
                    },
                    other => Sqrt(Box::new(other)),
                }
            },
        },

        Cbrt: {
            method: cbrt_assign,
            bounds: |ctx, _, inp| AmplBounds::new((2 * ctx.logspan(inp)) / 3, 0),
        },

        Exp: {
            method: exp_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.maxlog(inp, false) + ctx.logspan(out);
                let lower = if ctx.lower_bound_early_stopping {
                    ctx.minlog(inp, true)
                } else { 0 };
                AmplBounds::new(upper, lower)
            },
            optimize: |arg| {
                // Exp/log cancellation: Exp(Log(x)) => x
                if let Log(x) = arg {
                    *x
                } else {
                    Exp(Box::new(arg))
                }
            },
        },

        Exp2: {
            method: exp2_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.maxlog(inp, false) + ctx.logspan(out);
                let lower = if ctx.lower_bound_early_stopping {
                    ctx.minlog(inp, true)
                } else { 0 };
                AmplBounds::new(upper, lower)
            },
        },

        Expm1: {
            method: expm1_assign,
            bounds: |ctx, out, inp| {
                let mx = ctx.maxlog(inp, false);
                let upper = (1 + mx).max(1 + mx - ctx.minlog(out, false));
                AmplBounds::new(upper, 0)
            },
        },

        Log: {
            method: log_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.logspan(inp) - ctx.minlog(out, false) + 1;
                let lower = if ctx.lower_bound_early_stopping {
                    -ctx.maxlog(out, true)
                } else { 0 };
                AmplBounds::new(upper, lower)
            },
            // TODO: Get rid of these optimize clones
            optimize: |arg| {
                // Log/exp cancellation: Log(Exp(x)) => x
                match arg {
                    Exp(x) => *x,
                    // log(1 + x) or log(x + 1) => log1p(x)
                    Add(a, b) => match (&*a, &*b) {
                        (Literal(one), x) if (*one - 1.0).abs() == 0.0 => Log1p(Box::new(x.clone())),
                        (x, Literal(one)) if (*one - 1.0).abs() == 0.0 => Log1p(Box::new(x.clone())),
                        _ => Log(Box::new(Add(a, b))),
                    },
                    other => Log(Box::new(other)),
                }
            },
        },

        Log2: {
            method: log2_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.logspan(inp) - ctx.minlog(out, false) + 1;
                let lower = if ctx.lower_bound_early_stopping {
                    -ctx.maxlog(out, true)
                } else { 0 };
                AmplBounds::new(upper, lower)
            },
        },

        Log10: {
            method: log10_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.logspan(inp) - ctx.minlog(out, false) + 1;
                let lower = if ctx.lower_bound_early_stopping {
                    -ctx.maxlog(out, true)
                } else { 0 };
                AmplBounds::new(upper, lower)
            },
        },

        Log1p: {
            method: log1p_assign,
            bounds: |ctx, out, inp| {
                let upper_base = ctx.maxlog(inp, false) - ctx.minlog(out, false);
                // If input can be negative, add slack
                let lo_neg = inp.lo.as_float().is_sign_negative();
                let hi_neg = inp.hi.as_float().is_sign_negative();
                let slack = if lo_neg || hi_neg { get_slack(ctx.iteration, ctx.slack_unit) } else { 0 };
                AmplBounds::new(upper_base + slack, 0)
            },
        },

        Logb: {
            method: logb_assign,
            bounds: |ctx, _, _| AmplBounds::new(get_slack(ctx.iteration, ctx.slack_unit), 0),
        },

        Sin: {
            method: sin_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.maxlog(inp, false) - ctx.minlog(out, false);
                let lower = if ctx.lower_bound_early_stopping {
                    if ctx.maxlog(inp, false) >= 1 { -1 - ctx.maxlog(out, true) } else { 0 }
                } else { 0 };
                AmplBounds::new(upper, lower)
            },
            optimize: |arg| {
                // sin(PI * x / n) => sinu(2*n, x)
                // sin(PI * x) => sinu(2, x)
                // sin(2 * PI * x) => sinu(1, x)
                match arg {
                    // sin(PI * (x / n))
                    Mul(a, b) if matches!(&*a, Pi) => match &*b {
                        Div(x, n) => if let Literal(nval) = **n {
                            let i = nval as u64;
                            if i as f64 == nval && i > 0 {
                                return Sinu(2 * i, x.clone());
                            }
                            Sin(Box::new(Mul(a, b)))
                        } else {
                            Sin(Box::new(Mul(a, b)))
                        },
                        // sin(PI * x)
                        _ => Sinu(2, b.clone()),
                    },
                    // sin((x / n) * PI) or sin(x * PI)
                    Mul(a, b) if matches!(&*b, Pi) => match &*a {
                        Div(x, n) => if let Literal(nval) = **n {
                            let i = nval as u64;
                            if i as f64 == nval && i > 0 {
                                return Sinu(2 * i, x.clone());
                            }
                            Sin(Box::new(Mul(a, b)))
                        } else {
                            Sin(Box::new(Mul(a, b)))
                        },
                        // sin(x * PI)
                        _ => Sinu(2, a.clone()),
                    },
                    _ => Sin(Box::new(arg)),
                }
            },
        },

        Cos: {
            method: cos_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.maxlog(inp, false) - ctx.minlog(out, false)
                    + ctx.maxlog(inp, false).min(0);
                let lower = if ctx.lower_bound_early_stopping {
                    -ctx.maxlog(out, true) - 2
                } else { 0 };
                AmplBounds::new(upper, lower)
            },
            optimize: |arg| {
                // cos(PI * x / n) => cosu(2*n, x)
                // cos(PI * x) => cosu(2, x)
                // cos(2 * PI * x) => cosu(1, x)
                match arg {
                    // cos(PI * (x / n))
                    Mul(a, b) if matches!(&*a, Pi) => match &*b {
                        Div(x, n) => if let Literal(nval) = **n {
                            let i = nval as u64;
                            if i as f64 == nval && i > 0 {
                                return Cosu(2 * i, x.clone());
                            }
                            Cos(Box::new(Mul(a, b)))
                        } else {
                            Cos(Box::new(Mul(a, b)))
                        },
                        // cos(PI * x)
                        _ => Cosu(2, b.clone()),
                    },
                    // cos((x / n) * PI) or cos(x * PI)
                    Mul(a, b) if matches!(&*b, Pi) => match &*a {
                        Div(x, n) => if let Literal(nval) = **n {
                            let i = nval as u64;
                            if i as f64 == nval && i > 0 {
                                return Cosu(2 * i, x.clone());
                            }
                            Cos(Box::new(Mul(a, b)))
                        } else {
                            Cos(Box::new(Mul(a, b)))
                        },
                        // cos(x * PI)
                        _ => Cosu(2, a.clone()),
                    },
                    _ => Cos(Box::new(arg)),
                }
            },
        },

        Tan: {
            method: tan_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.maxlog(inp, false)
                    + ctx.maxlog(out, false).abs().max(ctx.minlog(out, false).abs())
                    + ctx.logspan(out)
                    + 1;
                let lower = if ctx.lower_bound_early_stopping {
                    ctx.minlog(inp, true)
                        + ctx
                            .maxlog(out, true)
                            .abs()
                            .min(ctx.minlog(out, true).abs())
                        - 1
                } else { 0 };
                AmplBounds::new(upper, lower)
            },
            optimize: |arg| {
                // tan(PI * x / n) => tanu(2 * n, x)
                // tan(PI * x) => tanu(2, x)
                // tan(2 * PI * x) => tanu(1, x)
                match arg {
                    // tan(PI * (x / n))
                    Mul(a, b) if matches!(&*a, Pi) => match &*b {
                        Div(x, n) => if let Literal(nval) = **n {
                            let i = nval as u64;
                            if i as f64 == nval && i > 0 {
                                return Tanu(2 * i, x.clone());
                            }
                            Tan(Box::new(Mul(a, b)))
                        } else {
                            Tan(Box::new(Mul(a, b)))
                        },
                        // tan(PI * x)
                        _ => Tanu(2, b.clone()),
                    },
                    // tan((x / n) * PI) or tan(x * PI)
                    Mul(a, b) if matches!(&*b, Pi) => match &*a {
                        Div(x, n) => if let Literal(nval) = **n {
                            let i = nval as u64;
                            if i as f64 == nval && i > 0 {
                                return Tanu(2 * i, x.clone());
                            }
                            Tan(Box::new(Mul(a, b)))
                        } else {
                            Tan(Box::new(Mul(a, b)))
                        },
                        // tan(x * PI)
                        _ => Tanu(2, a.clone()),
                    },
                    _ => Tan(Box::new(arg)),
                }
            },
        },

        Asin: {
            method: asin_assign,
            bounds: |ctx, out, _| {
                // if maxlog(out) > 1 => slack else 1
                let upper = if ctx.maxlog(out, false) >= 1 { get_slack(ctx.iteration, ctx.slack_unit) } else { 1 };
                AmplBounds::new(upper, 0)
            },
        },

        Acos: {
            method: acos_assign,
            bounds: |ctx, _out, inp| {
                let upper = if ctx.maxlog(inp, false) >= 0 { get_slack(ctx.iteration, ctx.slack_unit) } else { 0 };
                AmplBounds::new(upper, 0)
            },
        },

        Atan: {
            method: atan_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.logspan(inp)
                    - ctx.minlog(inp, false).abs().min(ctx.maxlog(inp, false).abs())
                    - ctx.minlog(out, false);
                let lower = if ctx.lower_bound_early_stopping {
                    - (ctx.minlog(inp, true).abs().max(ctx.maxlog(inp, true).abs()))
                        - ctx.maxlog(out, true)
                        - 2
                } else { 0 };
                AmplBounds::new(upper, lower)
            },
        },

        Sinh: {
            method: sinh_assign,
            bounds: |ctx, out, inp| {
                // maxlog(inp) + logspan(out) - min(minlog(inp), 0)
                let upper = ctx.maxlog(inp, false) + ctx.logspan(out) - ctx.minlog(inp, false).min(0);
                let lower = if ctx.lower_bound_early_stopping { ctx.minlog(inp, true).max(0) } else { 0 };
                AmplBounds::new(upper, lower)
            },
        },

        Cosh: {
            method: cosh_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.maxlog(inp, false) + ctx.logspan(out) + ctx.maxlog(inp, false).min(0);
                let lower = if ctx.lower_bound_early_stopping { (ctx.minlog(inp, true) - 1).max(0) } else { 0 };
                AmplBounds::new(upper, lower)
            },
        },

        Tanh: {
            method: tanh_assign,
            bounds: |ctx, out, inp| {
                let upper = ctx.logspan(out) + ctx.logspan(inp);
                AmplBounds::new(upper, 0)
            },
        },

        Asinh: {
            method: asinh_assign,
            bounds: |ctx, _, _| AmplBounds::new(get_slack(ctx.iteration, ctx.slack_unit), 0),
        },

        Acosh: {
            method: acosh_assign,
            bounds: |ctx, out, _| {
                let z_exp = ctx.minlog(out, false);
                let upper = if z_exp < 2 { get_slack(ctx.iteration, ctx.slack_unit) - z_exp } else { 0 };
                AmplBounds::new(upper, 0)
            },
        },

        Atanh: {
            method: atanh_assign,
            bounds: |ctx, _out, inp| {
                let upper = if ctx.maxlog(inp, false) >= 1 { get_slack(ctx.iteration, ctx.slack_unit) } else { 1 };
                AmplBounds::new(upper, 0)
            },
        },

        Erf: {
            method: erf_assign,
            bounds: |ctx, _, _| AmplBounds::new(get_slack(ctx.iteration, ctx.slack_unit), 0),
        },

        Erfc: {
            method: erfc_assign,
            bounds: |ctx, _, _| AmplBounds::new(get_slack(ctx.iteration, ctx.slack_unit), 0),
        },

        Rint: {
            method: rint_assign,
            bounds: |ctx, _, _| AmplBounds::new(get_slack(ctx.iteration, ctx.slack_unit), 0),
        },

        Round: {
            method: round_assign,
            bounds: |ctx, _, _| AmplBounds::new(get_slack(ctx.iteration, ctx.slack_unit), 0),
        },

        Ceil: {
            method: ceil_assign,
            bounds: |ctx, _, _| AmplBounds::new(get_slack(ctx.iteration, ctx.slack_unit), 0),
        },

        Floor: {
            method: floor_assign,
            bounds: |ctx, _, _| AmplBounds::new(get_slack(ctx.iteration, ctx.slack_unit), 0),
        },

        Trunc: {
            method: trunc_assign,
            bounds: |ctx, _, _| AmplBounds::new(get_slack(ctx.iteration, ctx.slack_unit), 0),
        },

        Not: {
            method: not_assign,
            bounds: |_, _, _| AmplBounds::zero(),
            path_reduce: path_reduction::not_op_path_reduce,
        },

        Error: {
            method: error_assign,
            bounds: |_, _, _| AmplBounds::zero(),
        },

        Assert: {
            method: assert_assign,
            bounds: |_, _, _| AmplBounds::zero(),
            path_reduce: path_reduction::assert_op_path_reduce,
        },
    },

    unary_param {
        Cosu: {
            method: cosu_assign,
            bounds: |ctx, param, out, inp| {
                let n_log = param as i64;
                let upper = ctx.maxlog(inp, false) - n_log - ctx.minlog(out, false) + 2;
                let lower = 0;
                AmplBounds::new(upper, lower)
            },
        },

        Sinu: {
            method: sinu_assign,
            bounds: |ctx, param, out, inp| {
                let n_log = param as i64;
                let upper = ctx.maxlog(inp, false) - n_log - ctx.minlog(out, false) + 2;
                let lower = 0;
                AmplBounds::new(upper, lower)
            },
        },

        Tanu: {
            method: tanu_assign,
            bounds: |ctx, param, out, inp| {
                let n_log = param as i64;
                let upper = ctx.maxlog(inp, false) - n_log
                    + ctx.maxlog(out, false).abs().max(ctx.minlog(out, false).abs()) + 3;
                let lower = 0;
                AmplBounds::new(upper, lower)
            },
        },
    },

    binary {
        Pow: {
            method: pow_assign,
            bounds: |ctx, out, x, y| {
                let maxlog_y = ctx.maxlog(y, false);
                let minlog_y_less = ctx.minlog(y, true);
                let logspan_x = ctx.logspan(x);
                let logspan_out = ctx.logspan(out);
                let maxlog_x = ctx.maxlog(x, false);
                let minlog_x = ctx.minlog(x, false);
                // Slack adjustments
                let y_slack = if crosses_zero(out) && x.lo.as_float().is_sign_negative() { get_slack(ctx.iteration, ctx.slack_unit) } else { 0 };
                let x_slack = if out.lo.as_float().is_zero() { get_slack(ctx.iteration, ctx.slack_unit) } else { 0 };

                // Upper bounds
                let upper_x_base = maxlog_y + logspan_x + logspan_out + x_slack;
                let upper_x = upper_x_base.max(x_slack); // mirror Racket's max(+..., x_slack)
                let abs_maxlog_x = maxlog_x.abs();
                let abs_minlog_x = minlog_x.abs();
                let span_x_mag = abs_maxlog_x.max(abs_minlog_x);
                let upper_y_base = maxlog_y + span_x_mag + logspan_out + y_slack;
                let upper_y = upper_y_base.max(y_slack);

                // Lower bounds
                let lower_x = if ctx.lower_bound_early_stopping { minlog_y_less } else { 0 };
                let min_abs_span = abs_maxlog_x.min(abs_minlog_x);
                let lower_y = if ctx.lower_bound_early_stopping {
                    if min_abs_span == 0 { 0 } else { minlog_y_less }
                } else { 0 };

                (AmplBounds::new(upper_x, lower_x), AmplBounds::new(upper_y, lower_y))
            },
            optimize: |base, exp| {
                if let Literal(exp_val) = exp {
                    // pow(arg, 2) => pow2(arg)
                    if (exp_val - 2.0).abs() == 0.0 {
                        return Pow2(Box::new(base));
                    }
                    // pow(arg, 0.5) => sqrt(arg)
                    if (exp_val - 0.5).abs() == 0.0 {
                        return Sqrt(Box::new(base));
                    }
                }

                // pow(x, p/q) optimizations
                if let Rational { num, den, neg } = &exp {
                    if *den == 1 {
                        return Pow(Box::new(base), Box::new(Rational { num: *num, den:*den, neg:*neg }));
                    }
                    let den_odd = den % 2 == 1;
                    let num_odd = num % 2 == 1;
                    if den_odd && !num_odd {
                        return Pow(Box::new(Fabs(Box::new(base))), Box::new(Rational { num: *num, den: *den, neg: *neg }));
                    }
                    if den_odd && num_odd {
                        return Copysign(
                            Box::new(Pow(Box::new(Fabs(Box::new(base.clone()))), Box::new(Rational { num: *num, den: *den, neg: *neg }))),
                            Box::new(base),
                        );
                    }
                }

                match base {
                    // pow(2, arg) => exp2(arg)
                    Literal(base_val) if (base_val - 2.0).abs() == 0.0 => {
                        Exp2(Box::new(exp))
                    }
                    // pow(E, arg) => exp(arg)
                    E => Exp(Box::new(exp)),
                    // pow(fabs(x), y) stays as is (already optimal for handling negative bases)
                    Fabs(_) => Pow(Box::new(base), Box::new(exp)),
                    _ => Pow(Box::new(base), Box::new(exp)),
                }
            },
        },

        Fdim: {
            method: fdim_assign,
            bounds: |ctx, out, x, y| {
                let output_min = ctx.minlog(out, false);
                let lhs_upper = ctx.maxlog(x, false) - output_min;
                let rhs_upper = ctx.maxlog(y, false) - output_min;
                let lhs_lower = if ctx.lower_bound_early_stopping { ctx.minlog(x, true) - ctx.maxlog(out, true) } else { 0 };
                let rhs_lower = if ctx.lower_bound_early_stopping { ctx.minlog(y, true) - ctx.maxlog(out, true) } else { 0 };
                (AmplBounds::new(lhs_upper, lhs_lower), AmplBounds::new(rhs_upper, rhs_lower))
            },
        },

        Hypot: {
            method: hypot_assign,
            bounds: |ctx, out, x, y| {
                let output_min = ctx.minlog(out, false);
                let x_upper = ctx.maxlog(x, false) - output_min;
                let y_upper = ctx.maxlog(y, false) - output_min;
                let x_lower = if ctx.lower_bound_early_stopping { ctx.minlog(x, true) - ctx.maxlog(out, true) } else { 0 };
                let y_lower = if ctx.lower_bound_early_stopping { ctx.minlog(y, true) - ctx.maxlog(out, true) } else { 0 };
                (AmplBounds::new(x_upper, x_lower), AmplBounds::new(y_upper, y_lower))
            },
        },
        Add: {
            method: add_assign,
            bounds: |ctx, out, lhs, rhs| {
                let output_min = ctx.minlog(out, false);
                let lhs_upper = ctx.maxlog(lhs, false) - output_min;
                let rhs_upper = ctx.maxlog(rhs, false) - output_min;
                let lhs_lower = if ctx.lower_bound_early_stopping {
                    ctx.minlog(lhs, true) - ctx.maxlog(out, true)
                } else { 0 };
                let rhs_lower = if ctx.lower_bound_early_stopping {
                    ctx.minlog(rhs, true) - ctx.maxlog(out, true)
                } else { 0 };
                (AmplBounds::new(lhs_upper, lhs_lower), AmplBounds::new(rhs_upper, rhs_lower))
            },
        },

        Sub: {
            method: sub_assign,
            bounds: |ctx, out, lhs, rhs| {
                let output_min = ctx.minlog(out, false);
                let lhs_upper = ctx.maxlog(lhs, false) - output_min;
                let rhs_upper = ctx.maxlog(rhs, false) - output_min;
                let lhs_lower = if ctx.lower_bound_early_stopping {
                    ctx.minlog(lhs, true) - ctx.maxlog(out, true)
                } else { 0 };
                let rhs_lower = if ctx.lower_bound_early_stopping {
                    ctx.minlog(rhs, true) - ctx.maxlog(out, true)
                } else { 0 };
                (AmplBounds::new(lhs_upper, lhs_lower), AmplBounds::new(rhs_upper, rhs_lower))
            },
            optimize: |lhs, rhs| {
                match (&lhs, &rhs) {
                    // (- (exp x) 1) => expm1(x)
                    (Exp(x), Literal(one)) if (one - 1.0).abs() == 0.0 => {
                        Expm1(x.clone())
                    }
                    // (- 1 (exp x)) => neg(expm1(x))
                    (Literal(one), Exp(x)) if (one - 1.0).abs() == 0.0 => {
                        Neg(Box::new(Expm1(x.clone())))
                    }
                    _ => Sub(Box::new(lhs), Box::new(rhs))
                }
            },
        },

        Mul: {
            method: mul_assign,
            bounds: |ctx, _, lhs, rhs| {
                (AmplBounds::new(ctx.logspan(rhs), 0),
                 AmplBounds::new(ctx.logspan(lhs), 0))
            },
        },

        Div: {
            method: div_assign,
            bounds: |ctx, _, lhs, rhs| {
                let lhs_bounds = AmplBounds::new(ctx.logspan(rhs), 0);
                let rhs_bounds = AmplBounds::new(ctx.logspan(lhs) + 2 * ctx.logspan(rhs), 0);
                (lhs_bounds, rhs_bounds)
            },
        },

        And: {
            method: and_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: path_reduction::bool_op_path_reduce,
        },

        Or: {
            method: or_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: path_reduction::bool_op_path_reduce,
        },

        Eq: {
            method: eq_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: path_reduction::bool_op_path_reduce,
        },

        Ne: {
            method: ne_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: path_reduction::bool_op_path_reduce,
        },

        Lt: {
            method: lt_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: path_reduction::bool_op_path_reduce,
        },

        Le: {
            method: le_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: path_reduction::bool_op_path_reduce,
        },

        Gt: {
            method: gt_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: path_reduction::bool_op_path_reduce,
        },

        Ge: {
            method: ge_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: path_reduction::bool_op_path_reduce,
        },

        Fmin: {
            method: fmin_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: |machine, idx, mark| {
                path_reduction::minmax_path_reduce(machine, idx, mark, false)
            },
        },

        Fmax: {
            method: fmax_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: |machine, idx, mark| {
                path_reduction::minmax_path_reduce(machine, idx, mark, true)
            },
        },

        Copysign: {
            method: copysign_assign,
            bounds: |_, _, _, _| (AmplBounds::zero(), AmplBounds::zero()),
        },

        Atan2: {
            method: atan2_assign,
            bounds: |ctx, out, y, x| {
                // Note: atan2 takes (y, x)
                let upper = ctx.maxlog(x, false) + ctx.maxlog(y, false)
                    - 2 * ctx.minlog(x, false).min(ctx.minlog(y, false))
                    - ctx.minlog(out, false);
                let lower = if ctx.lower_bound_early_stopping {
                    ctx.minlog(x, true) + ctx.minlog(y, true)
                        - 2 * ctx.maxlog(x, true).max(ctx.maxlog(y, true))
                        - ctx.maxlog(out, true)
                } else { 0 };
                (AmplBounds::new(upper, lower), AmplBounds::new(upper, lower))
            },
        },

        Fmod: {
            method: fmod_assign,
            bounds: |ctx, out, x, y| {
                let slack = if crosses_zero(y) { get_slack(ctx.iteration, ctx.slack_unit) } else { 0 };
                let upper_x = ctx.maxlog(x, false) - ctx.minlog(out, false);
                let upper_y = upper_x + slack;
                (AmplBounds::new(upper_x, 0), AmplBounds::new(upper_y, 0))
            },
        },

        Remainder: {
            method: remainder_assign,
            bounds: |ctx, out, x, y| {
                let slack = if crosses_zero(y) { get_slack(ctx.iteration, ctx.slack_unit) } else { 0 };
                let upper_x = ctx.maxlog(x, false) - ctx.minlog(out, false);
                let upper_y = upper_x + slack;
                (AmplBounds::new(upper_x, 0), AmplBounds::new(upper_y, 0))
            },
        },
    },

    ternary {
        Fma: {
            method: fma_assign,
            bounds: |ctx, out, a, b, _c| {
                (AmplBounds::new(ctx.logspan(b) + ctx.logspan(out), 0),
                 AmplBounds::new(ctx.logspan(a) + ctx.logspan(out), 0),
                 AmplBounds::new(ctx.logspan(out), 0))
            },
            optimize: |x, y, z| {
                // fma(x, y, z) => x * y + z
                Add(Box::new(Mul(Box::new(x), Box::new(y))), Box::new(z))
            },
        },

        If: {
            method: if_assign,
            bounds: |_, _, _, _, _| (AmplBounds::zero(), AmplBounds::zero(), AmplBounds::zero()),
            path_reduce: path_reduction::if_op_path_reduce,
        },
    },
}
