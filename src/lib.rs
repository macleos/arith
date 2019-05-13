/*
Grammar
expression -> add
add -> mult ((+|-) mult)*
mult -> unary ((*|/) unary)*
unary -> - base | base
base -> number | (expression)
number -> [1-9][0-9]*(.[0-9]+)?
*/
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;
use std::f64::consts::*;
use std::iter::Peekable;
use std::slice::Iter;
use std::str::Chars;
use std::vec::Vec;
#[derive(Debug)]
enum Lexemes {
    Plus,
    Star,
    Hyphen,
    Slash,
    LBracket,
    RBracket,
    Dot,
    Caret,
    Comma,
    Numeral(f64),

    Identifier(String),
}
fn lex(iter: Chars) -> Vec<Lexemes> {
    let mut chars = iter.peekable();
    let mut lexemes: Vec<Lexemes> = Vec::new();
    while let Some(c) = chars.next() {
        match c {
            '+' => lexemes.push(Lexemes::Plus),
            '-' => lexemes.push(Lexemes::Hyphen),
            '*' => lexemes.push(Lexemes::Star),
            '/' => lexemes.push(Lexemes::Slash),
            '(' => lexemes.push(Lexemes::LBracket),
            ')' => lexemes.push(Lexemes::RBracket),
            '^' => lexemes.push(Lexemes::Caret),
            ',' => lexemes.push(Lexemes::Comma),
            ' ' => {}
            '.' => lexemes.push(Lexemes::Dot),
            'a'...'z' | '$' | '@' | '#' => {
                let mut literal = String::new();
                literal.push(c);
                while let Some(ch @ 'a'...'z') | Some(ch @ '$') | Some(ch @ '@') | Some(ch @ '#')
                | Some(ch @ '0'...'9') = chars.peek()
                {
                    literal.push(*ch);
                    chars.next();
                }
                lexemes.push(Lexemes::Identifier(literal))
            }
            '0'...'9' => {
                let mut literal = String::new();
                literal.push(c);
                while let Some(ch @ '0'...'9') = chars.peek() {
                    literal.push(*ch);
                    chars.next();
                }
                if let Some('.') = chars.peek() {
                    chars.next();
                    if let Some(ch @ '0'...'9') = chars.peek() {
                        literal.push('.');
                        literal.push(*ch);
                        chars.next();
                        while let Some(ch @ '0'...'9') = chars.peek() {
                            literal.push(*ch);
                            chars.next();
                        }
                        lexemes.push(Lexemes::Numeral(literal.parse().unwrap()));
                    } else {
                        lexemes.push(Lexemes::Numeral(literal.parse().unwrap()));
                        lexemes.push(Lexemes::Dot);
                    }
                } else {
                    lexemes.push(Lexemes::Numeral(literal.parse().unwrap()))
                }
            }

            _ => {}
        }
    }
    lexemes
}
#[derive(Debug)]
enum Function {
    Sin,
    Cos,
    Tan,
    Noop,
}
impl Function {
    fn construct(ident: String) -> Self {
        match ident.as_str() {
            "sin" => Function::Sin,
            "cos" => Function::Cos,
            "tan" => Function::Tan,
            _ => Function::Noop,
        }
    }
    fn call(&self, args: &mut Vec<Box<Expr>>) -> f64 {
        match self {
            Function::Sin => args[0].as_mut().evaluate().sin(),
            Function::Cos => args[0].as_mut().evaluate().cos(),
            Function::Tan => args[0].as_mut().evaluate().tan(),
            Function::Noop => panic!("Attempted to call undefined function"),
        }
    }
    fn display(&self, args: &mut Vec<Box<Expr>>) -> String {
        match self {
            Function::Sin => format!("\\sin{{ {} }}", args[0].as_mut().display()),
            Function::Cos => format!("\\cos{{ {} }}", args[0].as_mut().display()),
            Function::Tan => format!("\\tan{{ {} }}", args[0].as_mut().display()),
            Function::Noop => panic!("Attempted to call undefined function"),
        }
    }
}
#[derive(Debug)]
enum Value {
    Pi,
    E,
    Unknown,
}
impl Value {
    fn construct(ident: String) -> Self {
        match ident.as_str() {
            "pi" => Value::Pi,
            "e" => Value::E,
            _ => Value::Unknown,
        }
    }
    fn get(&self) -> f64 {
        match self {
            Value::Pi => PI,
            Value::E => E,
            Value::Unknown => panic!("Attempted to get undefined value"),
        }
    }
    fn display(&self) -> String {
        match self {
            Value::Pi => format!("\\pi"),
            Value::E => format!("e"),
            Value::Unknown => panic!("Attempted to get undefined value"),
        }
    }
}
#[derive(Debug)]
enum BinaryOperation {
    Multiplication,
    Division,
    Addition,
    Subtraction,
    Power,
}
impl BinaryOperation {
    fn display(&mut self, l: &mut Box<Expr>, r: &mut Box<Expr>) -> String {
        match self {
            BinaryOperation::Multiplication => {
                format!("{} \\times {}", l.as_mut().display(), r.as_mut().display())
            }
            BinaryOperation::Division => format!(
                "\\frac{{ {} }}{{ {} }}",
                l.as_mut().display(),
                r.as_mut().display()
            ),
            BinaryOperation::Addition => {
                format!("{}+{}", l.as_mut().display(), r.as_mut().display())
            }
            BinaryOperation::Subtraction => {
                format!("{}-{}", l.as_mut().display(), r.as_mut().display())
            }
            BinaryOperation::Power => {
                format!("{}^{{ {} }}", l.as_mut().display(), r.as_mut().display())
            }
        }
    }
    fn apply(&mut self, l: &mut Box<Expr>, r: &mut Box<Expr>) -> f64 {
        match self {
            BinaryOperation::Multiplication => l.as_mut().evaluate() * r.as_mut().evaluate(),
            BinaryOperation::Division => l.as_mut().evaluate() / r.as_mut().evaluate(),
            BinaryOperation::Addition => l.as_mut().evaluate() + r.as_mut().evaluate(),
            BinaryOperation::Subtraction => l.as_mut().evaluate() - r.as_mut().evaluate(),
            BinaryOperation::Power => l.as_mut().evaluate().powf(r.as_mut().evaluate()),
        }
    }
}
#[derive(Debug)]
enum UnaryOperation {
    Negation,
}
impl UnaryOperation {
    fn apply(&mut self, b: &mut Box<Expr>) -> f64 {
        match self {
            UnaryOperation::Negation => -1.0 * b.as_mut().evaluate(),
        }
    }
    fn display(&mut self, b: &mut Box<Expr>) -> String {
        match self {
            UnaryOperation::Negation => format!("-{}", b.as_mut().display()),
        }
    }
}
#[derive(Debug)]
enum Expr {
    Unary(UnaryOperation, Box<Expr>),
    Binary(BinaryOperation, Box<Expr>, Box<Expr>),
    NumericLiteral(f64),
    Grouping(Box<Expr>),
    Function(Function, Vec<Box<Expr>>),
    Value(Value),
}
impl Expr {
    fn evaluate(&mut self) -> f64 {
        return match self {
            Expr::Unary(op, child) => op.apply(child),
            Expr::Binary(op, left, right) => op.apply(left, right),
            Expr::NumericLiteral(n) => *n,
            Expr::Grouping(child) => child.evaluate(),
            Expr::Function(f, args) => f.call(args),
            Expr::Value(v) => v.get(),
        };
    }
    fn display(&mut self) -> String {
        return match self {
            Expr::Unary(op, child) => op.display(child),
            Expr::Binary(op, left, right) => op.display(left, right),
            Expr::NumericLiteral(n) => format!("{}", n),
            Expr::Grouping(child) => format!("({})", child.display()),
            Expr::Function(f, args) => f.display(args),
            Expr::Value(v) => v.display(),
        };
    }
}
fn main() {
    let input = "((4 -pi +sin(2^3) + 1) * -((3*3+4*4)^0.5)) / 2";
    let mut ast = lex_and_parse(input);
    println!("{}", ast.display())
}
#[wasm_bindgen]
fn display(s: &str) -> String {
    lex_and_parse(s).display()
}
#[wasm_bindgen]
fn evaluate(s: &str) -> f64 {
    lex_and_parse(s).evaluate()
}
fn lex_and_parse(s: &str) -> Expr {
    let mut iter = s.chars();
    let mut lexemes = lex(iter);
    let mut iter_lexemes = lexemes.iter().peekable();
    let mut ast = parse_expression(&mut iter_lexemes);
    if let Some(_) = iter_lexemes.peek() {
        panic!("Syntax Error {:?}", iter_lexemes)
    } else {
        return ast;
    }
}
/*
Grammar
expression -> add
add -> mult ((+|-) mult)*
mult -> pow ((*|/) pow)*
pow -> unary ^ pow
unary -> - fn | fn
fn -> IDENTIFIER (args?) | base
args -> expression
base -> NUMBER | IDENTIFIER | (expression)
*/
fn parse_expression(iter: &mut Peekable<Iter<Lexemes>>) -> Expr {
    return parse_add(iter);
}
fn parse_add(iter: &mut Peekable<Iter<Lexemes>>) -> Expr {
    let mut expr = parse_mult(iter);
    loop {
        match iter.peek() {
            Some(Lexemes::Plus) => {
                iter.next();
                let right = parse_mult(iter);
                expr = Expr::Binary(BinaryOperation::Addition, Box::new(expr), Box::new(right));
            }
            Some(Lexemes::Hyphen) => {
                iter.next();
                let right = parse_mult(iter);
                expr = Expr::Binary(
                    BinaryOperation::Subtraction,
                    Box::new(expr),
                    Box::new(right),
                );
            }
            Some(_) => {
                break;
            }

            None => {
                break;
            }
        }
    }
    expr
}
fn parse_mult(iter: &mut Peekable<Iter<Lexemes>>) -> Expr {
    let mut expr = parse_pow(iter);

    loop {
        match iter.peek() {
            Some(Lexemes::Slash) => {
                iter.next();
                let right = parse_pow(iter);
                expr = Expr::Binary(BinaryOperation::Division, Box::new(expr), Box::new(right));
            }
            Some(Lexemes::Star) => {
                iter.next();
                let right = parse_pow(iter);
                expr = Expr::Binary(
                    BinaryOperation::Multiplication,
                    Box::new(expr),
                    Box::new(right),
                );
            }
            Some(_) => {
                break;
            }
            None => {
                break;
            }
        }
    }
    expr
}
fn parse_pow(iter: &mut Peekable<Iter<Lexemes>>) -> Expr {
    let mut left = parse_unary(iter);
    if let Some(Lexemes::Caret) = iter.peek() {
        iter.next();
        let mut right = parse_pow(iter);
        return Expr::Binary(BinaryOperation::Power, Box::new(left), Box::new(right));
    } else {
        return left;
    }
}
fn parse_unary(iter: &mut Peekable<Iter<Lexemes>>) -> Expr {
    return match iter.peek() {
        Some(Lexemes::Hyphen) => {
            iter.next();
            let right = parse_fn(iter);
            Expr::Unary(UnaryOperation::Negation, Box::new(right))
        }
        Some(_) => parse_fn(iter),
        None => panic!("Syntax Error"),
    };
}
fn parse_fn(iter: &mut Peekable<Iter<Lexemes>>) -> Expr {
    return match iter.peek() {
        Some(Lexemes::Identifier(s)) => {
            iter.next();
            if let Some(Lexemes::LBracket) = iter.peek() {
                iter.next();
                let args = parse_args(iter);
                iter.next();
                Expr::Function(Function::construct(s.to_string()), vec![Box::new(args)])
            } else {
                Expr::Value(Value::construct(s.to_string()))
            }
        }
        Some(_) => parse_base(iter),
        None => panic!("Syntax Error"),
    };
}
fn parse_args(iter: &mut Peekable<Iter<Lexemes>>) -> Expr {
    parse_expression(iter)
}
fn parse_base(iter: &mut Peekable<Iter<Lexemes>>) -> Expr {
    return match iter.peek() {
        Some(Lexemes::LBracket) => {
            iter.next();

            let expr = parse_expression(iter);

            iter.next();
            Expr::Grouping(Box::new(expr))
        }
        Some(Lexemes::Numeral(i)) => {
            iter.next();
            Expr::NumericLiteral(*i)
        }
        Some(Lexemes::Identifier(s)) => {
            iter.next();
            Expr::Value(Value::construct(s.to_string()))
        }
        Some(_) => panic!("Syntax Error"),
        None => panic!("Syntax Error"),
    };
}
