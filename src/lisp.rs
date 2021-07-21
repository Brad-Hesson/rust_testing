use core::panic;
use std::{
    borrow::Cow,
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
type LambdaFn = dyn Fn(Vec<ObjExpr>, Env) -> ObjExpr;
#[derive(Clone)]
pub struct Env {
    vars: Rc<RefCell<HashMap<String, Rc<LambdaFn>>>>,
}

impl Env {
    pub fn new() -> Self {
        let vars_cell: RefCell<HashMap<String, Rc<LambdaFn>>> = RefCell::new(HashMap::new());
        let mut vars = vars_cell.borrow_mut();
        vars.insert(
            "*".to_string(),
            Rc::new(|v, _| match &v[..] {
                [ObjExpr::Atom(ObjAtom::Number(left)), ObjExpr::Atom(ObjAtom::Number(right))] => {
                    ObjExpr::from(left.value * right.value)
                }
                _ => panic!("Wrong args for `*`, got `{:?}`", v),
            }),
        );
        vars.insert(
            "pi".to_string(),
            Rc::new(|_, _| ObjExpr::from(std::f64::consts::PI)),
        );
        drop(vars);
        Self {
            vars: Rc::new(vars_cell),
        }
    }
    fn clone_deep(&self) -> Self {
        Self {
            vars: Rc::new(self.vars.as_ref().clone()),
        }
    }
}
impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

fn eval_expr(expr: ObjExpr, env: Env) -> ObjExpr {
    match expr {
        ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) => {
            let borrow = env.vars.borrow();
            let var = borrow
                .get(&name)
                .unwrap_or_else(|| panic!("Variable `{}` does not exist", name));
            var(vec![], env.clone())
        }
        ObjExpr::List(ObjList { list }) => {
            let (first, args_list) = if let Some((f, a)) = list.split_first() {
                (f, a)
            } else {
                panic!("Got an empty list");
            };
            match first {
                ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) => {
                    eprintln!("Got proc `{}` with args: {:?}", name, args_list);
                    match name.as_str() {
                        "begin" => eval_begin(args_list, env),
                        "define" => eval_define(args_list, env),
                        "if" => eval_if(args_list, env),
                        "quote" => eval_quote(args_list),
                        "lambda" => eval_lambda(args_list),
                        _ => eval_symbol(name, args_list, env),
                    }
                }
                ObjExpr::List(..) => {
                    if let ObjExpr::Lambda(ObjLambda { func }) =
                        eval_expr(first.clone(), env.clone())
                    {
                        func(Vec::from(args_list), env)
                    } else {
                        panic!(
                            "First element of a list must be a symbol or a lambda expression, got `{:?}`",
                            first
                        )
                    }
                }
                _ => panic!(
                    "First element of a list must be a symbol or a lambda expression, got `{:?}`",
                    first
                ),
            }
        }
        expr => expr,
    }
}
fn eval_begin(args_list: &[ObjExpr], env: Env) -> ObjExpr {
    args_list
        .iter()
        .map(|expr| eval_expr(expr.clone(), env.clone()))
        .last()
        .expect("Got begin proc with no args")
}
fn eval_define(args_list: &[ObjExpr], env: Env) -> ObjExpr {
    assert_arity("define", 2, args_list);
    let name = if let ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) = args_list[0].clone() {
        name
    } else {
        panic!(
            "First argument of `define` must be a symbol, got `{:?}`",
            args_list[0]
        );
    };
    let expr = eval_expr(args_list[1].clone(), env.clone());
    if let ObjExpr::Lambda(ObjLambda { func }) = expr {
        env.vars.borrow_mut().insert(name, func);
    } else {
        let rc = Rc::new(move |_, _| expr.clone());
        env.vars.borrow_mut().insert(name, rc);
    }
    ObjExpr::List(ObjList { list: vec![] })
}
fn eval_if(args_list: &[ObjExpr], env: Env) -> ObjExpr {
    assert_arity("if", 3, args_list);
    let cond = if let ObjExpr::Atom(ObjAtom::Number(ObjNumber { value })) =
        eval_expr(args_list[0].clone(), env.clone())
    {
        value != 0.0
    } else {
        panic!("Got invalid if condition: `{:?}`", args_list[0])
    };
    if cond {
        eval_expr(args_list[1].clone(), env)
    } else {
        eval_expr(args_list[2].clone(), env)
    }
}
fn eval_quote(args_list: &[ObjExpr]) -> ObjExpr {
    assert_arity("quote", 1, args_list);
    args_list[0].clone()
}
fn eval_lambda(args_list: &[ObjExpr]) -> ObjExpr {
    assert_arity("lambda", 2, args_list);
    let arg_names: Cow<[String]> = if let ObjExpr::List(ObjList { list }) = args_list[0].clone() {
        list.iter()
            .map(|expr| {
                if let ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) = expr {
                    name.clone()
                } else {
                    panic!(
                        "First argument of `lambda` must be a list of symbols, got a `{:?}`",
                        expr
                    )
                }
            })
            .collect()
    } else {
        panic!(
            "First argument of `lambda` must be a list, got {:?}",
            args_list[0]
        )
    };
    let arity = arg_names.len();
    let lambda_expr = args_list[1].clone();
    ObjExpr::Lambda(ObjLambda {
        func: Rc::new(move |fn_args_list, fn_env| {
            assert_arity("lambda", arity, &fn_args_list);
            let clone_env = fn_env.clone_deep();
            Iterator::zip(arg_names.iter(), fn_args_list).for_each(|(arg_name, arg_expr)| {
                let arg_expr_eval = eval_expr(arg_expr, fn_env.clone());
                let rc = Rc::new(move |_, _| arg_expr_eval.clone());
                clone_env.vars.borrow_mut().insert(arg_name.clone(), rc);
            });
            eval_expr(lambda_expr.clone(), clone_env)
        }),
    })
}
fn eval_symbol(name: &str, args_list: &[ObjExpr], env: Env) -> ObjExpr {
    let func = env
        .vars
        .borrow()
        .get(name)
        .unwrap_or_else(|| panic!("Proc `{}` does not exist", name))
        .clone();
    let args = args_list
        .iter()
        .map(|expr| eval_expr(expr.clone(), env.clone()))
        .collect();
    func(args, env)
}

fn assert_arity(fname: &str, arity: usize, args_list: &[ObjExpr]) {
    assert!(
        args_list.len() == arity,
        "`{}` takes {} expression(s) as an argument, got `{:?}`",
        fname,
        arity,
        args_list
    );
}

pub fn parse_lisp(source: &str) -> ObjExpr {
    let mut tokenizer = Tokenizer {
        source: source.to_string(),
        patterns: Regex::new(r"\(|\)|[^\s()]+").unwrap(),
    }
    .peekable();
    parse_into_expr(&mut tokenizer).unwrap()
}

pub fn run_lisp(source: &str, env: Env) -> ObjExpr {
    let expr = parse_lisp(source);
    eval_expr(expr, env)
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let env = Env::new();
        let out = run_lisp("(begin (define r 10) (* pi (* r r)))", env);
        eprintln!("{:#?}", out);
        assert_eq!(format!("{:?}", out), "314.1592653589793")
    }

    #[test]
    fn if_test() {
        let env = Env::new();

        run_lisp("(define b 1)", env.clone());
        let out = run_lisp("(if b (* 2 5) 20)", env.clone());
        eprintln!("{:#?}", out);
        assert_eq!(format!("{:?}", out), "10");

        run_lisp("(define b 0)", env.clone());
        let out = run_lisp("(if b (* 2 5) 20)", env.clone());
        eprintln!("{:#?}", out);
        assert_eq!(format!("{:?}", out), "20");
    }

    #[test]
    fn lambda_test() {
        let env = Env::new();
        let out = run_lisp("((lambda (r) (begin (define h 10) (* r h))) 2)", env);
        eprintln!("{:?}", out);
        assert_eq!(format!("{:?}", out), "20");
    }

    #[test]
    fn lambda_define_test() {
        let env = Env::new();
        run_lisp("(define square (lambda (r) (* r r)))", env.clone());
        let out = run_lisp("(square 2)", env.clone());
        eprintln!("{:?}", out);
        assert_eq!(format!("{:?}", out), "4");

        let out = run_lisp("(square 3)", env.clone());
        eprintln!("{:?}", out);
        assert_eq!(format!("{:?}", out), "9");
    }
}
