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
struct ObjSymbol {
    name: String,
}
impl Display for ObjSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.name, f)
    }
}

#[derive(Clone)]
struct ObjNumber {
    value: f64,
}
impl Debug for ObjNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}

#[derive(Clone)]
enum ObjAtom {
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
struct ObjList {
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
struct ObjLambda {
    func: Rc<dyn Fn(ObjList, Env) -> ObjExpr>,
    args: Vec<String>,
}

#[derive(Clone)]
enum ObjExpr {
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
            Some(ObjExpr::List(ObjList { list: list }))
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

#[derive(Clone)]
struct Env {
    vars: Rc<RefCell<HashMap<String, Rc<dyn Fn(ObjList, Env) -> ObjExpr>>>>,
}

impl Env {
    fn new() -> Self {
        let vars_cell: RefCell<HashMap<String, Rc<dyn Fn(ObjList, Env) -> ObjExpr>>> =
            RefCell::new(HashMap::new());
        let mut vars = vars_cell.borrow_mut();
        vars.insert(
            "*".to_string(),
            Rc::new(|v, _| match &v.list[..] {
                [ObjExpr::Atom(ObjAtom::Number(left)), ObjExpr::Atom(ObjAtom::Number(right))] => {
                    ObjExpr::from(left.value * right.value)
                }
                _ => panic!("Wrong args for `*`, got `{:?}`", v),
            }),
        );
        vars.insert("pi".to_string(), Rc::new(|_, _| ObjExpr::from(3.14159)));
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

fn eval_expr(expr: ObjExpr, env: Env) -> ObjExpr {
    match expr {
        ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) => {
            let borrow = env.vars.borrow();
            let var = borrow
                .get(&name)
                .expect(&format!("Variable `{}` does not exist", name));
            var(ObjList { list: vec![] }, env.clone())
        }
        ObjExpr::List(ObjList { list }) => {
            let (first, args_list) = if let Some((f, a)) = list.split_first() {
                (f, a)
            } else {
                panic!("Got an empty list");
            };
            let name = if let ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) = first {
                name
            } else if let ObjExpr::Lambda(ObjLambda { func, args: _ }) =
                eval_expr(first.clone(), env.clone())
            {
                return func(
                    ObjList {
                        list: Vec::from(args_list),
                    },
                    env,
                );
            } else {
                panic!(
                    "First element of a list must be a symbol or a lambda expression, got `{:?}`",
                    first
                )
            };
            eprintln!("Got proc `{}` with args: {:?}", name, args_list);
            match name.as_str() {
                "begin" => args_list
                    .iter()
                    .map(|expr| eval_expr(expr.clone(), env.clone()))
                    .last()
                    .expect("Got begin proc with no args"),
                "define" => {
                    assert_arity("define", 2, args_list);
                    let name = if let ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) =
                        args_list[0].clone()
                    {
                        name
                    } else {
                        panic!(
                            "First argument of `define` must be a symbol, got `{:?}`",
                            args_list[0]
                        );
                    };
                    let expr = eval_expr(args_list[1].clone(), env.clone());
                    if let ObjExpr::Lambda(ObjLambda { func, args: _ }) = expr {
                        env.vars.borrow_mut().insert(name.clone(), func);
                    } else {
                        let rc = Rc::new(move |_, _| expr.clone());
                        env.vars.borrow_mut().insert(name.clone(), rc);
                    }
                    ObjExpr::List(ObjList { list: vec![] })
                }
                "if" => {
                    assert_arity("if", 3, args_list);
                    let cond = if let ObjExpr::Atom(ObjAtom::Number(ObjNumber { value })) =
                        eval_expr(args_list[0].clone(), env.clone())
                    {
                        value != 0.0
                    } else {
                        panic!("Got invalid if condition: `{:?}`", args_list[0])
                    };
                    if cond {
                        eval_expr(args_list[1].clone(), env.clone())
                    } else {
                        eval_expr(args_list[2].clone(), env.clone())
                    }
                }
                "quote" => {
                    assert_arity("quote", 1, args_list);
                    args_list[0].clone()
                }
                "lambda" => {
                    assert_arity("lambda", 2, args_list);
                    let arg_names: Vec<String> = if let ObjExpr::List(ObjList { list }) =
                        args_list[0].clone()
                    {
                        list.iter().map(|expr|{
                            if let ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) = expr{
                                name.clone()
                            }else{
                                panic!("First argument of `lambda` must be a list of symbols, got a `{:?}`", expr)
                            }
                        }).collect()
                    } else {
                        panic!(
                            "First argument of `lambda` must be a list, got {:?}",
                            args_list[0]
                        )
                    };
                    let arg_names_clone = arg_names.clone();
                    let expr = args_list[1].clone();
                    let arity = arg_names.len();
                    let func = move |fn_args_list: ObjList, fn_env: Env| {
                        assert_arity("lambda", arity, &fn_args_list.list);
                        let loc_env = fn_env.clone_deep();
                        Iterator::zip(arg_names_clone.iter(), fn_args_list.list).for_each(
                            |(name, expr)| {
                                let expr = eval_expr(expr.clone(), fn_env.clone());
                                let rc = Rc::new(move |_, _| expr.clone());
                                loc_env.vars.borrow_mut().insert(name.clone(), rc);
                            },
                        );
                        eval_expr(expr.clone(), loc_env)
                    };
                    ObjExpr::Lambda(ObjLambda {
                        func: Rc::new(func),
                        args: arg_names.clone(),
                    })
                }
                _ => {
                    let func = env
                        .vars
                        .borrow()
                        .get(name)
                        .expect(&format!("Proc `{}` does not exist", name))
                        .clone();
                    let args = args_list
                        .iter()
                        .map(|expr| eval_expr(expr.clone(), env.clone()))
                        .collect();
                    func(ObjList { list: args }, env)
                }
            }
        }
        expr => expr,
    }
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

fn parse_lisp(source: &str) -> ObjExpr {
    let mut tokenizer = Tokenizer {
        source: source.to_string(),
        patterns: Regex::new(r"\(|\)|[^\s()]+").unwrap(),
    }
    .peekable();
    parse_into_expr(&mut tokenizer).unwrap()
}

fn run_lisp(source: &str, env: Env) -> ObjExpr {
    let expr = parse_lisp(source);
    eval_expr(expr, env)
}

#[test]
fn it_works() {
    let mut env = Env::new();
    let out = run_lisp("(begin (define r 10) (* pi (* r r)))", env);
    eprintln!("{:#?}", out);
    assert_eq!(format!("{:?}", out), "314.159")
}

#[test]
fn if_test() {
    let mut env = Env::new();

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
    eprintln!("{:?}", out)
}

#[test]
fn lambda_define_test() {
    let env = Env::new();
    run_lisp(
        "(define fn (lambda (r) (begin (define h 10))))",
        env.clone(),
    );
    let out = run_lisp("(fn 2)", env.clone());
    eprintln!("{:?}", out)
}
