use std::iter::Peekable;

use regex::Regex;

struct Tokenizer<'s> {
    source: &'s str,
    patterns: Regex,
}

impl<'s> Iterator for Tokenizer<'s> {
    type Item = &'s str;
    fn next(&mut self) -> Option<&'s str> {
        let mat = self.patterns.find(self.source)?;
        let found = &self.source[mat.range()];
        self.source = &self.source[mat.end()..];
        Some(found)
    }
}

#[derive(Debug)]
struct ObjSymbol {
    name: String,
}

#[derive(Debug)]
struct ObjNumber {
    value: isize,
}

#[derive(Debug)]
enum ObjAtom {
    Symbol(ObjSymbol),
    Number(ObjNumber),
}

#[derive(Debug)]
struct ObjList {
    list: Vec<ObjExpr>,
}

#[derive(Debug)]
enum ObjExpr {
    Atom(ObjAtom),
    List(ObjList),
}

fn parse_into_expr<'a, I: Iterator<Item = &'a str>>(tokens: &mut Peekable<I>) -> Option<ObjExpr> {
    match tokens.peek()? {
        &")" => None,
        &"(" => {
            tokens.next();
            let mut list = Vec::new();
            while let Some(expr) = parse_into_expr(tokens) {
                list.push(expr);
            }
            assert_eq!(tokens.next(), Some(")"));
            Some(ObjExpr::List(ObjList { list }))
        }
        _ => {
            let tok = tokens.next()?;
            let atom = if let Ok(value) = tok.parse::<isize>() {
                ObjAtom::Number(ObjNumber { value })
            } else {
                ObjAtom::Symbol(ObjSymbol {
                    name: tok.to_string(),
                })
            };
            Some(ObjExpr::Atom(atom))
        }
    }
}

#[test]
fn it_works() {
    let source = "(begin (define r 10) (* pi (* r r)))";
    let mut tokenizer = Tokenizer {
        source,
        patterns: Regex::new(r"\(|\)|[^\s()]+").unwrap(),
    }.peekable();
    let expr = parse_into_expr(&mut tokenizer);
    eprintln!("{:#?}", expr);
}
