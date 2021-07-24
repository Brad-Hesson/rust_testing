use core::panic;
use std::{
    cell::RefCell,
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

macro_rules! arg_as_number {
    ($expr:expr, $fname:expr) => {{
        if let ObjExpr::Atom(ObjAtom::Number(ObjNumber { value })) = $expr {
            Ok(value)
        } else {
            Err(format!("{} expects a number but got `{:?}`", $fname, $expr))
        }
    }};
}
macro_rules! arg_as_symbol {
    ($expr:expr, $fname:expr) => {{
        if let ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) = $expr {
            Ok(name)
        } else {
            Err(format!("{} expects a symbol but got `{:?}`", $fname, $expr))
        }
    }};
}
macro_rules! arg_as_lambda {
    ($expr:expr, $fname:expr) => {{
        if let ObjExpr::Lambda(ObjLambda { func }) = $expr {
            Ok(func)
        } else {
            Err(format!("{} expects a lambda but got `{:?}`", $fname, $expr))
        }
    }};
}
macro_rules! arg_as_list {
    ($expr:expr, $fname:expr) => {{
        if let ObjExpr::List(ObjList { list }) = $expr {
            Ok(list)
        } else {
            Err(format!("{} expects a list but got `{:?}`", $fname, $expr))
        }
    }};
}

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

type EvalErr = String;
type LambdaFn = dyn Fn(Vec<ObjExpr>, Env) -> Result<ObjExpr, EvalErr>;

struct StackFrame {
    vars: RefCell<HashMap<String, ObjExpr>>,
}
impl StackFrame {
    fn new() -> Self {
        Self {
            vars: RefCell::new(HashMap::new()),
        }
    }
}
#[derive(Clone)]
pub struct Env(Rc<RefCell<Vec<StackFrame>>>);

impl Env {
    pub fn new() -> Self {
        let env = Self(Rc::new(RefCell::new(vec![StackFrame::new()])));
        env.insert_proc(
            "begin".to_string(),
            Rc::new(|args_list, env| {
                args_list
                    .iter()
                    .map(|expr| eval_expr(expr.clone(), env.clone()))
                    .last()
                    .expect("`begin` expected at least one expression")
            }),
        );
        env.insert_proc(
            "define".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("define", 2, &args_list);
                let name = arg_as_symbol!(args_list[0].clone(), "First argument of `define`")?;
                let definable = eval_expr(args_list[1].clone(), env.clone())?;
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
                let cond = arg_as_number!(
                    eval_expr(args_list[0].clone(), env.clone())?,
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
                let arg_names = arg_as_list!(args_list[0].clone(), "First argument of `lambda`")?;
                let expr_to_run = args_list[1].clone();
                let define = if let Some(ObjExpr::Lambda(ObjLambda { func })) =
                    env.get("define".to_string())
                {
                    Ok(func)
                } else {
                    Err("`define` should always be in the environment")
                }?;
                Ok(ObjExpr::Lambda(ObjLambda {
                    func: Rc::new(move |fn_args_list, fn_env: Env| {
                        assert_arity!("lambda function", arg_names.len(), &fn_args_list);
                        fn_env.push_new_stack();
                        for (arg_name, arg_expr) in
                            Iterator::zip(arg_names.clone().into_iter(), fn_args_list)
                        {
                            define(vec![arg_name, arg_expr], fn_env.clone())?;
                        }
                        let out = eval_expr(expr_to_run.clone(), fn_env.clone());
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
                arg_as_lambda!(
                    eval_expr(dealias(args_list[0].clone(), env.clone()), env.clone())?,
                    "First argument of `map`"
                )?;
                let expr_to_run = args_list[0].clone();
                let args_vec = args_list[1..]
                    .iter()
                    .enumerate()
                    .map(|(i, expr)| {
                        arg_as_list!(
                            dealias(expr.clone(), env.clone()),
                            format!("Argument {} of `map`", i)
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
                    results.push(eval_expr(expr, env.clone())?)
                }
                Ok(ObjExpr::List(ObjList { list: results }))
            }),
        );
        env.insert_proc(
            "*".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("*", 2, &args_list);
                let l = arg_as_number!(eval_expr(args_list[0].clone(), env.clone())?, "*")?;
                let r = arg_as_number!(eval_expr(args_list[1].clone(), env.clone())?, "*")?;
                Ok(ObjExpr::from(l * r))
            }),
        );
        env.insert_proc(
            "+".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("+", 2, &args_list);
                let l = arg_as_number!(eval_expr(args_list[0].clone(), env.clone())?, "+")?;
                let r = arg_as_number!(eval_expr(args_list[1].clone(), env.clone())?, "+")?;
                Ok(ObjExpr::from(l + r))
            }),
        );
        env.insert_proc(
            "-".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("-", 2, &args_list);
                let l = arg_as_number!(eval_expr(args_list[0].clone(), env.clone())?, "-")?;
                let r = arg_as_number!(eval_expr(args_list[1].clone(), env.clone())?, "-")?;
                Ok(ObjExpr::from(l - r))
            }),
        );
        env.insert_proc(
            "<".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("<", 2, &args_list);
                let l = arg_as_number!(eval_expr(args_list[0].clone(), env.clone())?, "<")?;
                let r = arg_as_number!(eval_expr(args_list[1].clone(), env.clone())?, "<")?;
                Ok(ObjExpr::from((l < r) as usize as f64))
            }),
        );
        env.insert_var("pi".to_string(), ObjExpr::from(std::f64::consts::PI));
        env
    }
    fn push_new_stack(&self) {
        self.0.as_ref().borrow_mut().push(StackFrame::new());
    }
    fn pop_old_stack(&self) {
        self.0.as_ref().borrow_mut().pop();
    }
    fn get(&self, name: String) -> Option<ObjExpr> {
        let borrow = self.0.as_ref().borrow();
        let mut iter = borrow.iter();
        while let Some(sf) = iter.next_back() {
            if let Some(func) = sf.vars.borrow().get(&name) {
                return Some(func.clone());
            }
        }
        None
    }
    fn contains_key(&self, name: String) -> bool {
        self.0
            .as_ref()
            .borrow()
            .iter()
            .any(|sf| sf.vars.borrow().contains_key(&name))
    }
    fn insert_var(&self, name: String, expr: ObjExpr) {
        if let Some(a) = self.0.as_ref().borrow().as_slice().last() {
            a.vars.borrow_mut().insert(name, expr);
        } else {
            panic!("No global scope in provided environment");
        }
    }
    fn insert_proc(&self, name: String, func: Rc<LambdaFn>) {
        if let Some(a) = self.0.as_ref().borrow().as_slice().last() {
            a.vars
                .borrow_mut()
                .insert(name, ObjExpr::Lambda(ObjLambda { func }));
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

fn eval_expr(expr: ObjExpr, env: Env) -> Result<ObjExpr, EvalErr> {
    let expr = dealias(expr, env.clone());
    match expr {
        ObjExpr::List(ObjList { list }) => {
            //eprintln!("Evaluating list: {:?}", list);
            let (first_expr, args_list) = if let Some((f, a)) = list.split_first() {
                (f, a)
            } else {
                return Err("Got an empty list".to_string());
            };
            let func = arg_as_lambda!(
                eval_expr(first_expr.clone(), env.clone())?,
                "First element of a list"
            )?;
            func(args_list.to_vec(), env)
        }
        expr => Ok(expr),
    }
}

fn dealias(expr: ObjExpr, env: Env) -> ObjExpr {
    match expr {
        ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) if env.contains_key(name.clone()) => env
            .get(name)
            .expect("Unreachable: Already checked that the key exists"),
        expr => expr,
    }
}

pub fn parse_lisp(source: &str) -> ObjExpr {
    let mut tokenizer = Tokenizer {
        source: source.to_string(),
        patterns: vec![
            Regex::new(r"(?m)\A\s*;.*$").unwrap(),
            Regex::new(r"(?m)\A\s*([^\s()]+)").unwrap(),
            Regex::new(r"(?m)\A\s*(\()").unwrap(),
            Regex::new(r"(?m)\A\s*(\))").unwrap(),
        ],
    }
    .peekable();
    parse_into_expr(&mut tokenizer).unwrap()
}

pub fn run_lisp(source: &str, env: Env) -> Result<ObjExpr, EvalErr> {
    let expr = parse_lisp(source);
    eval_expr(expr, env)
}
#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use super::*;

    #[test]
    fn it_works() {
        let env = Env::new();
        let out = run_lisp("(begin (define r 10) (* pi (* r r)))", env).unwrap();
        eprintln!("{:#?}", out);
        assert_eq!(format!("{:?}", out), "314.1592653589793")
    }

    #[test]
    fn if_test() {
        let env = Env::new();

        run_lisp("(define b 1)", env.clone()).unwrap();
        let out = run_lisp("(if b (* 2 5) 20)", env.clone()).unwrap();
        eprintln!("{:#?}", out);
        assert_eq!(format!("{:?}", out), "10");

        run_lisp("(define b 0)", env.clone()).unwrap();
        let out = run_lisp("(if b (* 2 5) 20)", env.clone()).unwrap();
        eprintln!("{:#?}", out);
        assert_eq!(format!("{:?}", out), "20");
    }

    #[test]
    fn lambda_test() {
        let env = Env::new();
        let out = run_lisp("((lambda (r) (begin (define h 10) (* r h))) 2)", env).unwrap();
        eprintln!("{:?}", out);
        assert_eq!(format!("{:?}", out), "20");
    }

    #[test]
    fn lambda_define_test() {
        let env = Env::new();
        run_lisp("(define square (lambda (r) (* r r)))", env.clone()).unwrap();
        let out = run_lisp("(square 2)", env.clone()).unwrap();
        eprintln!("{:?}", out);
        assert_eq!(format!("{:?}", out), "4");

        let out = run_lisp("(square 3)", env.clone()).unwrap();
        eprintln!("{:?}", out);
        assert_eq!(format!("{:?}", out), "9");
    }

    #[test]
    fn file_test() {
        let source = read_to_string("src/test.rkt").unwrap();
        let env = Env::new();
        let out = run_lisp(&source, env).unwrap();
        eprintln!("{:?}", out);
        //assert_eq!(format!("{:?}", out), "2584");
    }
}
