use super::{Delimiter, Keyword, Operator, Token, TokenAt};

// expr == logexpr

#[derive(Debug, Eq, PartialEq)]
pub enum Atom {
    Const(String),
    FnCall(String, Vec<Expr>),
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

    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,

    And,
    Or,
}


pub fn parse_expr(tokens: &[Token]) -> (Option<Expr>, &[Token]) {
    parse_logexpr(tokens)
}

pub fn parse_logexpr(tokens: &[Token]) -> (Option<Expr>, &[Token]) {
    // TODO: logexpr && comparison, logexpr || comparison

    let (mut expr, mut remain) = match parse_comparison(tokens) {
        (Some(e), r) => (e, r),
        _ => return (None, tokens),
    };

    loop {
        let operator = match remain.get(0) {
            Some(&Token::Op(ref o)) => o.clone(),
            _ => break,
        };
        let operation = match *operator {
            Operator::And => Operation::And,
            Operator::Or => Operation::Or,
            _ => break,
        };
        remain = &remain[1..];

        match parse_comparison(remain) {
            (Some(sum), r) => {
                expr = Expr::Bin(Box::new(expr), Box::new(sum), operation);
                remain = r;
            }
            _ => return (None, tokens),
        }
    }
    (Some(expr), remain)
}

pub fn parse_comparison(tokens: &[Token]) -> (Option<Expr>, &[Token]) {
    let (mut expr, mut remain) = match parse_sum(tokens) {
        (Some(e), r) => (e, r),
        _ => return (None, tokens),
    };

    loop {
        let operator = match remain.get(0) {
            Some(&Token::Op(ref o)) => o.clone(),
            _ => break,
        };
        let operation = match *operator {
            Operator::Lt => Operation::Lt,
            Operator::Lte => Operation::Lte,
            Operator::Gt => Operation::Gt,
            Operator::Gte => Operation::Gte,
            Operator::Eq => Operation::Eq,
            Operator::Neq => Operation::Neq,
            _ => break,
        };
        remain = &remain[1..];

        match parse_sum(remain) {
            (Some(sum), r) => {
                expr = Expr::Bin(Box::new(expr), Box::new(sum), operation);
                remain = r;
            }
            _ => return (None, tokens),
        }
    }
    (Some(expr), remain)
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

    if tokens[0] == Token::Delim(Delimiter::LParen) {
        let (optexpr, remain) = parse_expr(&tokens[1..]);
        match (optexpr, remain.get(0)) {
            (Some(e), Some(&Token::Delim(Delimiter::RParen))) => {
                return (Some(Atom::PExpr(Box::new(e))), &remain[1..]);
            }
            _ => return (None, tokens),
        }
    }

    if tokens[0] == Token::Op(Operator::Not) {
        if let (Some(a), remaining) = parse_atom(&tokens[1..]) {
            return (Some(Atom::Neg(Box::new(a))), remaining);
        }
    }

    if let Token::Ident(ref s) = tokens[0] {
        match tokens.get(1) {
            // fncall, continue
            Some(&Token::Delim(Delimiter::LParen)) => (),
            // just an ident, return
            _ => return (Some(Atom::Ident(s.clone())), &tokens[1..]),
        }

        let fn_name = s.clone();
        let mut fn_args = vec![];
        // already verified the opening paren, skip it safely
        let mut remaining = &tokens[2..];

        loop {
            match parse_expr(remaining) {
                (Some(e), r) => {
                    fn_args.push(e);
                    remaining = r
                }
                _ => break,
            }
            match remaining.get(0) {
                Some(&Token::Delim(Delimiter::Comma)) => remaining = &remaining[1..],
                _ => break,
            }
        }
        return match remaining.get(0) {
            Some(&Token::Delim(Delimiter::RParen)) => {
                (Some(Atom::FnCall(fn_name, fn_args)), &remaining[1..])
            }
            _ => (None, tokens),
        };
    }

    if let Token::Const(ref s) = tokens[0] {
        return (Some(Atom::Const(s.clone())), &tokens[1..]);
    }

    (None, tokens)
}
