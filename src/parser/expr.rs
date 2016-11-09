use super::{Keyword, Operator, Token, TokenAt};

// expr == logexpr

#[derive(Debug, Eq, PartialEq)]
pub enum Atom {
    Const(String),
    FnCall,
    Ident(String),
    Neg(Box<Atom>),
    PExpr(Box<Expr>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Atom(Atom),
    Bin(Box<Expr>, Box<Expr>, Operation),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Operation {
    Add,
    Sub,

    Div,
    Mul,
}

pub fn parse_expr(tokens: &[Token]) -> (Option<Expr>, &[Token]) {
    // TODO: logexpr && comparison, logexpr || comparison

    // TODO: comparison
    parse_comparison(tokens)
}

fn parse_comparison(tokens: &[Token]) -> (Option<Expr>, &[Token]) {
    // TODO: comparison compop summation

    parse_sum(tokens)
}

pub fn parse_sum(tokens: &[Token]) -> (Option<Expr>, &[Token]) {
    let product_res = parse_product(tokens);
    if product_res.0.is_none() {
        return (None, product_res.1);
    }

    let mut expr = product_res.0.unwrap();
    let mut my_tokens = product_res.1;

    loop {
        match my_tokens.get(0) {
            Some(&Token::Op(Operator::Plus)) => {
                match parse_product(&my_tokens[1..]) {
                    (Some(p), remaining) => {
                        expr = Expr::Bin(Box::new(expr), Box::new(p), Operation::Add);
                        my_tokens = remaining;
                    }
                    _ => return (None, my_tokens),
                }
            }
            Some(&Token::Op(Operator::Minus)) => {
                match parse_product(&my_tokens[1..]) {
                    (Some(p), remaining) => {
                        expr = Expr::Bin(Box::new(expr), Box::new(p), Operation::Sub);
                        my_tokens = remaining;
                    }
                    _ => return (None, my_tokens),
                }
            }
            _ => break,
        }
    }
    (Some(expr), my_tokens)
}

pub fn parse_product(tokens: &[Token]) -> (Option<Expr>, &[Token]) {
    let atom_res = parse_atom(tokens);
    if atom_res.0.is_none() {
        return (None, atom_res.1);
    }

    let mut expr = atom_res.0.map(Expr::Atom).unwrap();
    let mut my_tokens = atom_res.1;

    loop {
        match my_tokens.get(0) {
            Some(&Token::Op(Operator::Div)) => {
                match parse_atom(&my_tokens[1..]) {
                    (Some(a), remaining) => {
                        expr = Expr::Bin(Box::new(expr), Box::new(Expr::Atom(a)), Operation::Div);
                        my_tokens = remaining;
                    }
                    _ => return (None, my_tokens),
                }
            }
            Some(&Token::Op(Operator::Mul)) => {
                match parse_atom(&my_tokens[1..]) {
                    (Some(a), remaining) => {
                        expr = Expr::Bin(Box::new(expr), Box::new(Expr::Atom(a)), Operation::Mul);
                        my_tokens = remaining;
                    }
                    _ => return (None, my_tokens),
                }
            }
            _ => break,
        }
    }
    (Some(expr), my_tokens)
}

pub fn parse_atom(tokens: &[Token]) -> (Option<Atom>, &[Token]) {
    if tokens.is_empty() {
        return (None, tokens);
    }

    if tokens[0] == Token::Op(Operator::Not) {
        if let (Some(a), remaining) = parse_atom(&tokens[1..]) {
            return (Some(Atom::Neg(Box::new(a))), remaining);
        }
    }

    // TODO: fncall

    if let Token::Ident(ref s) = tokens[0] {
        return (Some(Atom::Ident(s.clone())), &tokens[1..]);
    }

    if let Token::Const(ref s) = tokens[0] {
        return (Some(Atom::Const(s.clone())), &tokens[1..]);
    }

    // TODO: pexpr

    (None, tokens)
}
