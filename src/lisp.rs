use std::{
    collections::HashMap,
    env::args,
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
enum ObjExpr {
    Atom(ObjAtom),
    List(ObjList),
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

struct Env {
    vars: HashMap<String, Rc<dyn Fn(ObjList) -> ObjExpr>>,
}

impl Env {
    fn new() -> Self {
        let mut vars: HashMap<String, Rc<dyn Fn(ObjList) -> ObjExpr>> = HashMap::new();
        vars.insert(
            "*".to_string(),
            Rc::new(|v| match &v.list[..] {
                [ObjExpr::Atom(ObjAtom::Number(left)), ObjExpr::Atom(ObjAtom::Number(right))] => {
                    ObjExpr::from(left.value * right.value)
                }
                _ => panic!("Wrong args for `*`, got `{:?}`", v),
            }),
        );
        vars.insert("pi".to_string(), Rc::new(|_| ObjExpr::from(3.14159)));
        Self { vars }
    }
}

fn eval_expr(expr: ObjExpr, env: &mut Env) -> ObjExpr {
    match expr {
        ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) => env
            .vars
            .get(&name)
            .expect(&format!("Variable `{}` does not exist", name))(
            ObjList { list: vec![] }
        ),
        ObjExpr::List(ObjList { list }) => {
            if let Some((first, args_list)) = list.split_first() {
                if let ObjExpr::Atom(ObjAtom::Symbol(ObjSymbol { name })) = first {
                    eprintln!("Got proc `{}` with args: {:?}", name, args_list);
                    match name.as_str() {
                        "begin" => args_list
                            .iter()
                            .map(|expr| eval_expr(expr.clone(), env))
                            .last()
                            .expect("Got begin proc with no args"),
                        "define" => {
                            assert!(
                                args_list.len() == 2,
                                "define takes a symbol and an expression as arguments, got `{:?}`",
                                args_list
                            );
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
                            let expr = eval_expr(args_list[1].clone(), env);
                            let rc = Rc::new(move |_| expr.clone());
                            env.vars.insert(name.clone(), rc);
                            ObjExpr::List(ObjList { list: vec![] })
                        }
                        "if" => {
                            assert!(
                                args_list.len() == 3,
                                "`if` takes a condition, and two expressions as arguments, got `{:?}`",
                                args_list
                            );
                            let cond = if let ObjExpr::Atom(ObjAtom::Number(ObjNumber { value })) =
                                eval_expr(args_list[0].clone(), env)
                            {
                                value != 0.0
                            } else {
                                panic!("Got invalid if condition: `{:?}`", args_list[0])
                            };
                            if cond {
                                args_list[1].clone()
                            } else {
                                args_list[2].clone()
                            }
                        }
                        "quote" => {
                            assert!(
                                args_list.len() == 1,
                                "`quote` takes one expression as an argument, got `{:?}`",
                                args_list
                            );
                            args_list[0].clone()
                        }
                        _ => {
                            let func = env
                                .vars
                                .get(name)
                                .expect(&format!("Proc `{}` does not exist", name))
                                .clone();
                            let args = args_list
                                .iter()
                                .map(|expr| eval_expr(expr.clone(), env))
                                .collect();
                            func(ObjList { list: args })
                        }
                    }
                } else {
                    unimplemented!("First element of a list must be a symbol")
                }
            } else {
                unimplemented!("Got an empty list");
            }
        }
        expr => expr,
    }
}

#[test]
fn it_works() {
    let source = "(begin (define r 10) (* pi (* r r)))";
    let mut tokenizer = Tokenizer {
        source: source.to_string(),
        patterns: Regex::new(r"\(|\)|[^\s()]+").unwrap(),
    }
    .peekable();
    let expr = parse_into_expr(&mut tokenizer).unwrap();
    let mut env = Env::new();
    let out = eval_expr(expr, &mut env);
    eprintln!("{:#?}", out);
}

#[test]
fn if_test() {
    let source = "(begin (define b 1) (if b (* 2 5) 20))";
    let mut tokenizer = Tokenizer {
        source: source.to_string(),
        patterns: Regex::new(r"\(|\)|[^\s()]+").unwrap(),
    }
    .peekable();
    let expr = parse_into_expr(&mut tokenizer).unwrap();
    let mut env = Env::new();
    let out = eval_expr(expr, &mut env);
    eprintln!("{:#?}", out);
}
