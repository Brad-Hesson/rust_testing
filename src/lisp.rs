use core::panic;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    iter::Peekable,
    rc::Rc,
};

use regex::Regex;

struct Tokenizer {
    source: String,
    patterns: Regex,
}

impl Iterator for Tokenizer {
    type Item = String;
    fn next(&mut self) -> Option<String> {
        let mat = self.patterns.find(&self.source)?;
        let found = self.source[mat.range()].to_string();
        self.source = self.source[mat.end()..].to_string();
        Some(found)
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
            ObjExpr::Lambda(_) => f.write_str("lambda"),
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
type LambdaFn = dyn Fn(Vec<ObjExpr>, Env) -> Result<ObjExpr, EvalErr>;
#[derive(Clone)]
pub struct Env {
    vars: Rc<RefCell<Vec<RefCell<HashMap<String, Rc<LambdaFn>>>>>>,
}

macro_rules! arg_as {
    (Number, $expr:expr, $fname:expr, $env:expr) => {{
        if let ObjExpr::Atom(ObjAtom::Number(ObjNumber { value })) = eval_expr($expr.clone(), $env)?
        {
            Ok(value)
        } else {
            Err(format!(
                "`{}` expected a number but got `{:?}`",
                $fname, $expr
            ))
        }
    }};
    (Symbol, $expr:expr, $fname:expr, $env:expr) => {{
        if let ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) = $expr.clone() {
            Ok(name)
        } else {
            Err(format!(
                "`{}` expected a symbol but got `{:?}`",
                $fname, $expr
            ))
        }
    }};
    (Lambda, $expr:expr, $fname:expr, $env:expr) => {{
        if let ObjExpr::Lambda(ObjLambda { func }) = eval_expr($expr.clone(), $env)? {
            Ok(func)
        } else {
            Err(format!(
                "`{}` expected a lambda but got `{:?}`",
                $fname, $expr
            ))
        }
    }};
    (List, $expr:expr, $fname:expr, $env:expr) => {{
        if let ObjExpr::List(ObjList { list }) = $expr.clone() {
            Ok(list)
        } else {
            Err(format!(
                "`{}` expected a list but got `{:?}`",
                $fname, $expr
            ))
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

impl Env {
    pub fn new() -> Self {
        let mut vars: HashMap<String, Rc<LambdaFn>> = HashMap::new();
        vars.insert(
            "begin".to_string(),
            Rc::new(|args_list, env| {
                args_list
                    .iter()
                    .map(|expr| eval_expr(expr.clone(), env.clone()))
                    .last()
                    .expect("Got begin proc with no args")
            }),
        );
        vars.insert(
            "define".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("define", 2, &args_list);
                let name = arg_as!(Symbol, args_list[0], "define", env)?;
                if let Ok(func) = arg_as!(Lambda, args_list[1], "define", env.clone()) {
                    env.insert(name, func);
                } else {
                    let expr = eval_expr(args_list[1].clone(), env.clone())?;
                    let rc = Rc::new(move |_, _| Ok(expr.clone()));
                    env.insert(name, rc);
                }
                Ok(ObjExpr::List(ObjList { list: vec![] }))
            }),
        );
        vars.insert(
            "if".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("if", 3, &args_list);
                let cond = arg_as!(Number, args_list[0], "if", env.clone())?;
                if cond != 0f64 {
                    eval_expr(args_list[1].clone(), env)
                } else {
                    eval_expr(args_list[2].clone(), env)
                }
            }),
        );
        vars.insert(
            "quote".to_string(),
            Rc::new(|args_list, _| {
                assert_arity!("quote", 1, &args_list);
                Ok(args_list[0].clone())
            }),
        );
        vars.insert(
            "lambda".to_string(),
            Rc::new(|args_list, _| {
                assert_arity!("lambda", 2, &args_list);
                let arg_names = arg_as!(List, args_list[0], "lambda", env)?
                    .iter()
                    .map(|expr| arg_as!(Symbol, expr, "lambda", env))
                    .collect::<Result<Vec<String>, EvalErr>>()?;
                let arity = arg_names.len();
                let lambda_expr = args_list[1].clone();
                Ok(ObjExpr::Lambda(ObjLambda {
                    func: Rc::new(move |fn_args_list, fn_env| {
                        assert_arity!("lambda", arity, &fn_args_list);
                        fn_env.push_new_stack();
                        Iterator::zip(arg_names.iter(), fn_args_list).for_each(
                            |(arg_name, arg_expr)| {
                                let arg_expr_eval = eval_expr(arg_expr, fn_env.clone());
                                let rc = Rc::new(move |_, _| arg_expr_eval.clone());
                                fn_env.insert(arg_name.clone(), rc);
                            },
                        );
                        let out = eval_expr(lambda_expr.clone(), fn_env.clone());
                        fn_env.pop_old_stack();
                        out
                    }),
                }))
            }),
        );
        vars.insert(
            "*".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("*", 2, &args_list);
                let l = arg_as!(Number, args_list[0], "*", env.clone())?;
                let r = arg_as!(Number, args_list[1], "*", env)?;
                Ok(ObjExpr::from(l * r))
            }),
        );
        vars.insert(
            "+".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("+", 2, &args_list);
                let l = arg_as!(Number, args_list[0], "+", env.clone())?;
                let r = arg_as!(Number, args_list[1], "+", env)?;
                Ok(ObjExpr::from(l + r))
            }),
        );
        vars.insert(
            "-".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("-", 2, &args_list);
                let l = arg_as!(Number, args_list[0], "-", env.clone())?;
                let r = arg_as!(Number, args_list[1], "-", env)?;
                Ok(ObjExpr::from(l - r))
            }),
        );
        vars.insert(
            "<".to_string(),
            Rc::new(|args_list, env| {
                assert_arity!("<", 2, &args_list);
                let l = arg_as!(Number, args_list[0], "<", env.clone())?;
                let r = arg_as!(Number, args_list[1], "<", env)?;
                Ok(ObjExpr::from((l < r) as usize as f64))
            }),
        );
        vars.insert(
            "pi".to_string(),
            Rc::new(|_, _| Ok(ObjExpr::from(std::f64::consts::PI))),
        );
        Self {
            vars: Rc::new(RefCell::new(vec![RefCell::new(vars)])),
        }
    }
    fn push_new_stack(&self) {
        self.vars
            .as_ref()
            .borrow_mut()
            .push(RefCell::new(HashMap::new()));
    }
    fn pop_old_stack(&self) {
        self.vars.as_ref().borrow_mut().pop();
    }
    fn get(&self, name: String) -> Rc<LambdaFn> {
        let borrow = self.vars.as_ref().borrow();
        let mut iter = borrow.iter();
        while let Some(hm) = iter.next_back() {
            if let Some(func) = hm.borrow().get(&name) {
                return func.clone();
            }
        }
        panic! {"Variable `{}` does not exist",name};
    }
    fn insert(&self, name: String, func: Rc<LambdaFn>) {
        if let Some(a) = self.vars.as_ref().borrow().as_slice().last() {
            a.borrow_mut().insert(name, func);
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
    match expr {
        ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) => {
            let func = env.get(name);
            func(vec![], env.clone())
        }
        ObjExpr::List(ObjList { list }) => {
            let (first, args_list) = if let Some((f, a)) = list.split_first() {
                (f, a)
            } else {
                return Err("Got an empty list".to_string());
            };
            if let Ok(name) = arg_as!(Symbol, first, "", env) {
                let func = env.get(name.to_string());
                return func(args_list.to_vec(), env);
            }
            if let Ok(func) = arg_as!(Lambda, first, "", env.clone()) {
                return func(Vec::from(args_list), env);
            }
            Err(format!(
                "First element of a list must be a symbol or a lambda expression, got `{:?}`",
                first
            ))
        }
        expr => Ok(expr),
    }
}

pub fn parse_lisp(source: &str) -> ObjExpr {
    let mut tokenizer = Tokenizer {
        source: source.to_string(),
        patterns: Regex::new(r"\(|\)|[^\s()]+").unwrap(),
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
        assert_eq!(format!("{:?}", out), "2584");
    }
}
