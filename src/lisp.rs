use core::panic;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    iter::{self, Peekable},
    rc::Rc,
};

use regex::Regex;

struct Tokenizer {
    source: String,
    patterns: Vec<Regex>,
}

impl Iterator for Tokenizer {
    type Item = String;
    fn next(&mut self) -> Option<String> {
        for pat in &self.patterns {
            if let Some(capture) = pat.captures(&self.source) {
                let (front, back) = self.source.split_at(capture.get(0)?.end());
                let token = capture.get(1).map(|mat| front[mat.range()].to_string());
                self.source = back.to_string();
                if let Some(token) = token {
                    return Some(token);
                } else {
                    return self.next();
                }
            }
        }
        None
    }
}

#[derive(Clone)]
pub struct ObjSymbol {
    name: String,
}
impl Display for ObjSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.name, f)
    }
}

#[derive(Clone)]
pub struct ObjNumber {
    value: f64,
}
impl Debug for ObjNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}

#[derive(Clone)]
pub enum ObjAtom {
    Symbol(ObjSymbol),
    Number(ObjNumber),
}
impl Debug for ObjAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjAtom::Symbol(symbol) => symbol.fmt(f),
            ObjAtom::Number(number) => number.fmt(f),
        }
    }
}

#[derive(Clone)]
pub struct ObjList {
    list: Vec<ObjExpr>,
}
impl Debug for ObjList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("(")?;
        for expr in &self.list {
            f.write_str(&format!(" {:?}", expr))?;
        }
        f.write_str(" )")
    }
}

#[derive(Clone)]
pub struct ObjLambda {
    func: Rc<LambdaFn>,
}
impl Debug for ObjLambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ObjLambda")
    }
}

#[derive(Clone)]
pub enum ObjExpr {
    Atom(ObjAtom),
    List(ObjList),
    Lambda(ObjLambda),
}
impl From<f64> for ObjExpr {
    fn from(n: f64) -> Self {
        ObjExpr::Atom(ObjAtom::Number(ObjNumber { value: n }))
    }
}
impl From<bool> for ObjExpr {
    fn from(n: bool) -> Self {
        ObjExpr::Atom(ObjAtom::Number(ObjNumber {
            value: n as isize as f64,
        }))
    }
}
impl Debug for ObjExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjExpr::Atom(atom) => atom.fmt(f),
            ObjExpr::List(list) => list.fmt(f),
            ObjExpr::Lambda(lambda) => lambda.fmt(f),
        }
    }
}

fn parse_into_expr<I: Iterator<Item = String>>(tokens: &mut Peekable<I>) -> Option<ObjExpr> {
    match tokens.peek()?.as_str() {
        ")" => None,
        "(" => {
            tokens.next();
            let mut list = Vec::new();
            while let Some(expr) = parse_into_expr(tokens) {
                list.push(expr);
            }
            assert_eq!(tokens.next(), Some(")".to_string()));
            Some(ObjExpr::List(ObjList { list }))
        }
        "'" => {
            tokens.next();
            assert!(tokens.next() == Some("(".to_string()));
            let mut list = Vec::new();
            while let Some(expr) = parse_into_expr(tokens) {
                list.push(expr);
            }
            assert_eq!(tokens.next(), Some(")".to_string()));
            Some(ObjExpr::List(ObjList {
                list: vec![
                    ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol {
                        name: "quote".to_string(),
                    })),
                    ObjExpr::List(ObjList { list }),
                ],
            }))
        }
        _ => {
            let tok = tokens.next()?;
            let atom = if let Ok(value) = tok.parse::<f64>() {
                ObjAtom::Number(ObjNumber { value })
            } else {
                ObjAtom::Symbol(ObjSymbol { name: tok })
            };
            Some(ObjExpr::Atom(atom))
        }
    }
}

macro_rules! generate_expr_as {
    ($name:ident, $type:ident, $pattern:pat, $inner:expr) => {
        #[macro_export]
        macro_rules! $name {
            ($expr:expr, $fname:expr) => {{
                if let $pattern = $expr {
                    Ok($inner)
                } else {
                    Err(format!(
                        "{} expects a(n) {} but got `{:?}`",
                        $fname,
                        stringify!($type),
                        $expr
                    ))
                }
            }};
        }
    };
}
generate_expr_as!(
    expr_as_number,
    number,
    ObjExpr::Atom(ObjAtom::Number(ObjNumber { value })),
    value
);
generate_expr_as!(
    expr_as_boolean,
    number,
    ObjExpr::Atom(ObjAtom::Number(ObjNumber { value })),
    value != 0f64
);
generate_expr_as!(
    expr_as_symbol,
    symbol,
    ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })),
    name
);
generate_expr_as!(
    expr_as_lambda,
    lambda,
    ObjExpr::Lambda(ObjLambda { func }),
    func
);
generate_expr_as!(expr_as_list, list, ObjExpr::List(ObjList { list }), list);

macro_rules! assert_arity {
    ($fname:expr, $arity:expr, $args_list:expr) => {
        assert!(
            $args_list.len() == $arity,
            "`{}` takes {} expression(s) as an argument, got `{:?}`",
            $fname,
            $arity,
            $args_list
        );
    };
}

#[macro_export]
macro_rules! insert_binary_op {
    (_expr, $left:expr, $op:tt, $r:expr) => {
        ($left $op $r)
    };
    ($op:tt, $macro:ident, $env:expr) => {
        $env.insert_proc(
            stringify!($op).to_string(),
            Rc::new(|args_list, env| {
                assert_arity!(stringify!($op), 2, &args_list);
                let l = $macro!(eval_expr(args_list[0].clone(), env)?, stringify!($op))?;
                let r = $macro!(eval_expr(args_list[1].clone(), env)?, stringify!($op))?;
                Ok(ObjExpr::from(insert_binary_op!(_expr, l, $op, r)))
            }),
        );
    };
}

type EvalErr = String;
type LambdaFn = dyn Fn(Vec<ObjExpr>, &mut Env) -> Result<ObjExpr, EvalErr>;

struct StackFrame(HashMap<String, ObjExpr>);
impl StackFrame {
    fn new() -> Self {
        Self(HashMap::new())
    }
}

pub struct Env(Vec<StackFrame>);

impl Env {
    pub fn new() -> Self {
        let mut env = Self(vec![StackFrame::new()]);
        env.insert_proc(
            "begin".to_string(),
            Rc::new(|args_list, env| {
                args_list
                    .iter()
                    .map(|expr| eval_expr(expr.clone(), env))
                    .last()
                    .expect("`begin` expected at least one expression")
            }),
        );
        env.insert_proc(
            "define".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("define", 2, &args_list);
                let name = expr_as_symbol!(args_list[0].clone(), "First argument of `define`")?;
                let definable = eval_expr(args_list[1].clone(), env)?;
                match definable {
                    ObjExpr::Lambda(ObjLambda { func }) => env.insert_proc(name, func),
                    expr => env.insert_var(name, expr),
                }
                Ok(ObjExpr::List(ObjList { list: vec![] }))
            }),
        );
        env.insert_proc(
            "if".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("if", 3, &args_list);
                let cond = expr_as_number!(
                    eval_expr(args_list[0].clone(), env)?,
                    "First argument of 'if'"
                )?;
                if cond != 0f64 {
                    eval_expr(args_list[1].clone(), env)
                } else {
                    eval_expr(args_list[2].clone(), env)
                }
            }),
        );
        env.insert_proc(
            "quote".to_string(),
            Rc::new(|args_list, _| {
                assert_arity!("quote", 1, &args_list);
                Ok(args_list[0].clone())
            }),
        );
        env.insert_proc(
            "lambda".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("lambda", 2, &args_list);
                let arg_names = expr_as_list!(args_list[0].clone(), "First argument of `lambda`")?;
                let expr_to_run = args_list[1].clone();
                let define = if let Some(ObjExpr::Lambda(ObjLambda { func })) =
                    env.get("define".to_string())
                {
                    Ok(func)
                } else {
                    Err("`define` should always be in the environment")
                }?;
                Ok(ObjExpr::Lambda(ObjLambda {
                    func: Rc::new(move |fn_args_list, fn_env: &mut Env| {
                        assert_arity!("lambda function", arg_names.len(), &fn_args_list);
                        fn_env.push_new_stack();
                        for (arg_name, arg_expr) in
                            Iterator::zip(arg_names.clone().into_iter(), fn_args_list)
                        {
                            define(vec![arg_name, arg_expr], fn_env)?;
                        }
                        let out = eval_expr(expr_to_run.clone(), fn_env);
                        fn_env.pop_old_stack();
                        out
                    }),
                }))
            }),
        );
        env.insert_proc(
            "list".to_string(),
            Rc::new(|args_list, _| Ok(ObjExpr::List(ObjList { list: args_list }))),
        );
        env.insert_proc(
            "map".to_string(),
            Rc::new(|args_list, env| {
                expr_as_lambda!(
                    eval_expr(dealias(args_list[0].clone(), env)?, env)?,
                    "First argument of `map`"
                )?;
                let expr_to_run = args_list[0].clone();
                let args_vec = args_list[1..]
                    .iter()
                    .enumerate()
                    .map(|(i, expr)| {
                        expr_as_list!(
                            dealias(expr.clone(), env)?,
                            format!("Argument {} of `map`", i + 1)
                        )
                    })
                    .collect::<Result<Vec<Vec<ObjExpr>>, EvalErr>>()?;
                let mut index = 0;
                let mut results = Vec::<ObjExpr>::new();
                while let Some(args) = iter::once(Some(expr_to_run.clone()))
                    .chain(args_vec.iter().map(|vec| vec.get(index).cloned()))
                    .collect::<Option<Vec<ObjExpr>>>()
                {
                    index += 1;
                    let expr = ObjExpr::List(ObjList { list: args });
                    results.push(eval_expr(expr, env)?)
                }
                Ok(ObjExpr::List(ObjList { list: results }))
            }),
        );
        env.insert_proc(
            "apply".to_string(),
            Rc::new(|args_list, env| {
                let func = expr_as_lambda!(
                    dealias(args_list[0].clone(), env)?,
                    "First argument of `apply`"
                )?;
                let list = expr_as_list!(
                    eval_expr(args_list[1].clone(), env)?,
                    "Second argument of `apply`"
                )?;
                func(list, env)
            }),
        );
        insert_binary_op!(+, expr_as_number, env);
        insert_binary_op!(-, expr_as_number, env);
        insert_binary_op!(*, expr_as_number, env);
        insert_binary_op!(/, expr_as_number, env);
        insert_binary_op!(<, expr_as_boolean, env);
        insert_binary_op!(>, expr_as_boolean, env);
        env.insert_var("pi".to_string(), ObjExpr::from(std::f64::consts::PI));
        env
    }
    fn push_new_stack(&mut self) {
        self.0.push(StackFrame::new());
    }
    fn pop_old_stack(&mut self) {
        self.0.pop();
    }
    fn get(&self, name: String) -> Option<ObjExpr> {
        let mut iter = self.0.iter();
        while let Some(sf) = iter.next_back() {
            if let Some(func) = sf.0.get(&name) {
                return Some(func.clone());
            }
        }
        None
    }
    fn insert_var(&mut self, name: String, expr: ObjExpr) {
        if let Some(a) = self.0.as_mut_slice().last_mut() {
            a.0.insert(name, expr);
        } else {
            panic!("No global scope in provided environment");
        }
    }
    fn insert_proc(&mut self, name: String, func: Rc<LambdaFn>) {
        if let Some(a) = self.0.as_mut_slice().last_mut() {
            a.0.insert(name, ObjExpr::Lambda(ObjLambda { func }));
        } else {
            panic!("No global scope in provided environment");
        }
    }
}
impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

fn eval_expr(expr: ObjExpr, env: &mut Env) -> Result<ObjExpr, EvalErr> {
    let expr = dealias(expr, env)?;
    match expr {
        ObjExpr::List(ObjList { list }) => {
            if list.is_empty() {
                return Err("Got an empty list".to_string());
            }
            let func =
                expr_as_lambda!(eval_expr(list[0].clone(), env)?, "First element of a list")?;
            func(list[1..].to_vec(), env)
        }
        expr => Ok(expr),
    }
}

fn dealias(expr: ObjExpr, env: &Env) -> Result<ObjExpr, EvalErr> {
    match expr {
        ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) => env
            .get(name.clone())
            .ok_or(format!("Variable `{}` does not exist", name)),
        expr => Ok(expr),
    }
}

pub fn parse_lisp(source: &str) -> ObjExpr {
    let mut tokenizer = Tokenizer {
        source: source.to_string(),
        patterns: vec![
            Regex::new(r"(?m)\A\s*;.*$").unwrap(),
            Regex::new(r"(?m)\A\s*([^\s'()]+)").unwrap(),
            Regex::new(r"(?m)\A\s*(')").unwrap(),
            Regex::new(r"(?m)\A\s*(\()").unwrap(),
            Regex::new(r"(?m)\A\s*(\))").unwrap(),
        ],
    }
    .peekable();
    parse_into_expr(&mut tokenizer).unwrap()
}

pub fn run_lisp(source: &str, env: &mut Env) -> Result<ObjExpr, EvalErr> {
    let expr = parse_lisp(source);
    eval_expr(expr, env)
}
#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use super::*;

    #[test]
    fn it_works() {
        let mut env = Env::new();
        let out = run_lisp("(begin (define r 10) (* pi (* r r)))", &mut env).unwrap();
        eprintln!("{:#?}", out);
        assert_eq!(format!("{:?}", out), "314.1592653589793")
    }

    #[test]
    fn if_test() {
        let mut env = Env::new();

        run_lisp("(define b 1)", &mut env).unwrap();
        let out = run_lisp("(if b (* 2 5) 20)", &mut env).unwrap();
        eprintln!("{:#?}", out);
        assert_eq!(format!("{:?}", out), "10");

        run_lisp("(define b 0)", &mut env).unwrap();
        let out = run_lisp("(if b (* 2 5) 20)", &mut env).unwrap();
        eprintln!("{:#?}", out);
        assert_eq!(format!("{:?}", out), "20");
    }

    #[test]
    fn lambda_test() {
        let mut env = Env::new();
        let out = run_lisp("((lambda (r) (begin (define h 10) (* r h))) 2)", &mut env).unwrap();
        eprintln!("{:?}", out);
        assert_eq!(format!("{:?}", out), "20");
    }

    #[test]
    fn lambda_define_test() {
        let mut env = Env::new();
        run_lisp("(define square (lambda (r) (* r r)))", &mut env).unwrap();
        let out = run_lisp("(square 2)", &mut env).unwrap();
        eprintln!("{:?}", out);
        assert_eq!(format!("{:?}", out), "4");

        let out = run_lisp("(square 3)", &mut env).unwrap();
        eprintln!("{:?}", out);
        assert_eq!(format!("{:?}", out), "9");
    }

    #[test]
    fn file_test() {
        let source = read_to_string("src/test.rkt").unwrap();
        let mut env = Env::new();
        let out = run_lisp(&source, &mut env).unwrap();
        eprintln!("{:?}", out);
        //assert_eq!(format!("{:?}", out), "2584");
    }
}
