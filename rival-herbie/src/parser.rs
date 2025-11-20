use rival::Expr;
use rug::{Float, Rational};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub enum SExpr {
    Atom(String),
    List(Vec<SExpr>),
}

pub fn parse_sexpr(s: &str) -> Result<SExpr, String> {
    let mut chars = s.trim().chars().peekable();
    let res = parse_sexpr_inner(&mut chars)?;
    if chars.peek().is_some() {
        return Err("Trailing characters after S-expression".to_string());
    }
    Ok(res)
}

fn parse_sexpr_inner(chars: &mut Peekable<Chars>) -> Result<SExpr, String> {
    while chars.peek().map_or(false, |c| c.is_whitespace()) {
        chars.next();
    }

    match chars.peek() {
        Some('(') => {
            chars.next();
            let mut list = Vec::new();
            loop {
                while chars.peek().map_or(false, |c| c.is_whitespace()) {
                    chars.next();
                }
                match chars.peek() {
                    Some(')') => {
                        chars.next();
                        return Ok(SExpr::List(list));
                    }
                    Some(_) => {
                        list.push(parse_sexpr_inner(chars)?);
                    }
                    None => return Err("Unclosed parenthesis".to_string()),
                }
            }
        }
        Some(')') => Err("Unexpected closing parenthesis".to_string()),
        Some(_) => {
            let mut atom = String::new();
            while let Some(&c) = chars.peek() {
                if c.is_whitespace() || c == '(' || c == ')' {
                    break;
                }
                atom.push(c);
                chars.next();
            }
            if atom.is_empty() {
                Err("Unexpected end of input".to_string())
            } else {
                Ok(SExpr::Atom(atom))
            }
        }
        None => Err("Unexpected end of input".to_string()),
    }
}

pub fn parse_expr(sexpr: &SExpr) -> Result<Expr, String> {
    match sexpr {
        SExpr::Atom(s) => {
            if s == "PI" {
                Ok(Expr::Pi)
            } else if s == "E" {
                Ok(Expr::E)
            } else if s == "TRUE" {
                Ok(Expr::Literal(Float::with_val(1024, 1.0)))
            } else if s == "FALSE" {
                Ok(Expr::Literal(Float::with_val(1024, 0.0)))
            } else if s == "+inf.0" {
                Ok(Expr::Literal(Float::with_val(1024, f64::INFINITY)))
            } else if s == "-inf.0" {
                Ok(Expr::Literal(Float::with_val(1024, f64::NEG_INFINITY)))
            } else if s == "+nan.0" || s == "-nan.0" {
                Ok(Expr::Literal(Float::with_val(1024, f64::NAN)))
            } else if let Ok(f) = Float::parse(s) {
                Ok(Expr::Literal(Float::with_val(1024, f)))
            } else if let Ok(rat) = Rational::parse(s) {
                Ok(Expr::Rational(Rational::from(rat)))
            } else {
                Ok(Expr::Var(s.clone()))
            }
        }
        SExpr::List(list) => {
            if list.is_empty() {
                return Err("Empty list in expression".to_string());
            }
            let op = match &list[0] {
                SExpr::Atom(s) => s.as_str(),
                _ => return Err("Operator must be an atom".to_string()),
            };
            let args: Result<Vec<Expr>, String> = list[1..].iter().map(parse_expr).collect();
            let args = args?;

            match (op, args.len()) {
                ("+", n) if n >= 2 => {
                    let mut expr = args[0].clone();
                    for arg in args.iter().skip(1) {
                        expr = Expr::Add(Box::new(expr), Box::new(arg.clone()));
                    }
                    Ok(expr)
                }
                ("-", 2) => Ok(Expr::Sub(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("-", 1) => Ok(Expr::Neg(Box::new(args[0].clone()))),
                ("*", n) if n >= 2 => {
                    let mut expr = args[0].clone();
                    for arg in args.iter().skip(1) {
                        expr = Expr::Mul(Box::new(expr), Box::new(arg.clone()));
                    }
                    Ok(expr)
                }
                ("/", 2) => Ok(Expr::Div(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("neg", 1) => Ok(Expr::Neg(Box::new(args[0].clone()))),
                ("sqrt", 1) => Ok(Expr::Sqrt(Box::new(args[0].clone()))),
                ("cbrt", 1) => Ok(Expr::Cbrt(Box::new(args[0].clone()))),
                ("pow", 2) => Ok(Expr::Pow(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("exp", 1) => Ok(Expr::Exp(Box::new(args[0].clone()))),
                ("exp2", 1) => Ok(Expr::Exp2(Box::new(args[0].clone()))),
                ("expm1", 1) => Ok(Expr::Expm1(Box::new(args[0].clone()))),
                ("log", 1) => Ok(Expr::Log(Box::new(args[0].clone()))),
                ("log2", 1) => Ok(Expr::Log2(Box::new(args[0].clone()))),
                ("log10", 1) => Ok(Expr::Log10(Box::new(args[0].clone()))),
                ("log1p", 1) => Ok(Expr::Log1p(Box::new(args[0].clone()))),
                ("logb", 1) => Ok(Expr::Logb(Box::new(args[0].clone()))),
                ("sin", 1) => Ok(Expr::Sin(Box::new(args[0].clone()))),
                ("cos", 1) => Ok(Expr::Cos(Box::new(args[0].clone()))),
                ("tan", 1) => Ok(Expr::Tan(Box::new(args[0].clone()))),
                ("asin", 1) => Ok(Expr::Asin(Box::new(args[0].clone()))),
                ("acos", 1) => Ok(Expr::Acos(Box::new(args[0].clone()))),
                ("atan", 1) => Ok(Expr::Atan(Box::new(args[0].clone()))),
                ("atan2", 2) => Ok(Expr::Atan2(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("sinh", 1) => Ok(Expr::Sinh(Box::new(args[0].clone()))),
                ("cosh", 1) => Ok(Expr::Cosh(Box::new(args[0].clone()))),
                ("tanh", 1) => Ok(Expr::Tanh(Box::new(args[0].clone()))),
                ("asinh", 1) => Ok(Expr::Asinh(Box::new(args[0].clone()))),
                ("acosh", 1) => Ok(Expr::Acosh(Box::new(args[0].clone()))),
                ("atanh", 1) => Ok(Expr::Atanh(Box::new(args[0].clone()))),
                ("erf", 1) => Ok(Expr::Erf(Box::new(args[0].clone()))),
                ("erfc", 1) => Ok(Expr::Erfc(Box::new(args[0].clone()))),
                ("rint", 1) => Ok(Expr::Rint(Box::new(args[0].clone()))),
                ("round", 1) => Ok(Expr::Round(Box::new(args[0].clone()))),
                ("ceil", 1) => Ok(Expr::Ceil(Box::new(args[0].clone()))),
                ("floor", 1) => Ok(Expr::Floor(Box::new(args[0].clone()))),
                ("trunc", 1) => Ok(Expr::Trunc(Box::new(args[0].clone()))),
                ("fmin", 2) => Ok(Expr::Fmin(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("fmax", 2) => Ok(Expr::Fmax(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("fdim", 2) => Ok(Expr::Fdim(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("copysign", 2) => Ok(Expr::Copysign(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("fmod", 2) => Ok(Expr::Fmod(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("remainder", 2) => Ok(Expr::Remainder(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("hypot", 2) => Ok(Expr::Hypot(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("fma", 3) => Ok(Expr::Fma(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                    Box::new(args[2].clone()),
                )),
                ("if", 3) => Ok(Expr::If(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                    Box::new(args[2].clone()),
                )),
                ("fabs", 1) => Ok(Expr::Fabs(Box::new(args[0].clone()))),
                ("not", 1) => Ok(Expr::Not(Box::new(args[0].clone()))),
                ("and", n) if n >= 2 => {
                    let mut expr = args[0].clone();
                    for arg in args.iter().skip(1) {
                        expr = Expr::And(Box::new(expr), Box::new(arg.clone()));
                    }
                    Ok(expr)
                }
                ("or", n) if n >= 2 => {
                    let mut expr = args[0].clone();
                    for arg in args.iter().skip(1) {
                        expr = Expr::Or(Box::new(expr), Box::new(arg.clone()));
                    }
                    Ok(expr)
                }
                ("==", 2) => Ok(Expr::Eq(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("!=", 2) => Ok(Expr::Ne(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                ("<", n) if n >= 2 => {
                    if n == 2 {
                        Ok(Expr::Lt(
                            Box::new(args[0].clone()),
                            Box::new(args[1].clone()),
                        ))
                    } else {
                        let mut exprs = Vec::new();
                        for i in 0..n - 1 {
                            exprs.push(Expr::Lt(
                                Box::new(args[i].clone()),
                                Box::new(args[i + 1].clone()),
                            ));
                        }
                        let mut expr = exprs[0].clone();
                        for e in exprs.iter().skip(1) {
                            expr = Expr::And(Box::new(expr), Box::new(e.clone()));
                        }
                        Ok(expr)
                    }
                }
                ("<=", n) if n >= 2 => {
                    if n == 2 {
                        Ok(Expr::Le(
                            Box::new(args[0].clone()),
                            Box::new(args[1].clone()),
                        ))
                    } else {
                        let mut exprs = Vec::new();
                        for i in 0..n - 1 {
                            exprs.push(Expr::Le(
                                Box::new(args[i].clone()),
                                Box::new(args[i + 1].clone()),
                            ));
                        }
                        let mut expr = exprs[0].clone();
                        for e in exprs.iter().skip(1) {
                            expr = Expr::And(Box::new(expr), Box::new(e.clone()));
                        }
                        Ok(expr)
                    }
                }
                (">", n) if n >= 2 => {
                    if n == 2 {
                        Ok(Expr::Gt(
                            Box::new(args[0].clone()),
                            Box::new(args[1].clone()),
                        ))
                    } else {
                        let mut exprs = Vec::new();
                        for i in 0..n - 1 {
                            exprs.push(Expr::Gt(
                                Box::new(args[i].clone()),
                                Box::new(args[i + 1].clone()),
                            ));
                        }
                        let mut expr = exprs[0].clone();
                        for e in exprs.iter().skip(1) {
                            expr = Expr::And(Box::new(expr), Box::new(e.clone()));
                        }
                        Ok(expr)
                    }
                }
                (">=", n) if n >= 2 => {
                    if n == 2 {
                        Ok(Expr::Ge(
                            Box::new(args[0].clone()),
                            Box::new(args[1].clone()),
                        ))
                    } else {
                        let mut exprs = Vec::new();
                        for i in 0..n - 1 {
                            exprs.push(Expr::Ge(
                                Box::new(args[i].clone()),
                                Box::new(args[i + 1].clone()),
                            ));
                        }
                        let mut expr = exprs[0].clone();
                        for e in exprs.iter().skip(1) {
                            expr = Expr::And(Box::new(expr), Box::new(e.clone()));
                        }
                        Ok(expr)
                    }
                }
                ("assert", 1) => Ok(args[0].clone()),
                ("TRUE", 0) => Ok(Expr::Literal(Float::with_val(1024, 1.0))),
                ("FALSE", 0) => Ok(Expr::Literal(Float::with_val(1024, 0.0))),
                ("PI", 0) => Ok(Expr::Pi),
                ("E", 0) => Ok(Expr::E),
                _ => Err(format!(
                    "Unknown operator or wrong number of arguments: {}",
                    op
                )),
            }
        }
    }
}
