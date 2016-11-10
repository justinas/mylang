pub use super::lexer::{Delimiter, Keyword, Operator, Token, TokenAt};
use self::expr::Expr;
use self::expr::parse_expr;

#[derive(Debug, Eq, PartialEq)]
pub struct AssignStmt {
    pub ident: String,
    pub expr: expr::Expr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block(Vec<Stmt>);

#[derive(Debug, Eq, PartialEq)]
pub struct DeclStmt {
    pub ident: String,
    pub typ: Type,
}


#[derive(Debug, Eq, PartialEq)]
pub struct FnArg {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FnItem {
    pub block: Block,
    pub name: String,
    pub params: Vec<FnArg>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt {
    Assign(AssignStmt),
    Block(Block),
    Decl(DeclStmt),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Byte,
    Int,
    Void,
}

pub fn parse(tokens: &[Token]) -> Result<Vec<FnItem>, ()> {
    let mut tokens = tokens;
    let mut funcs = vec![];
    while tokens != &[] {
        match parse_fn(tokens) {
            (Some(func), remain) => {
                funcs.push(func);
                tokens = remain;
            }
            (None, remain) => return Err(()),
        }
    }
    Ok(funcs)
}

pub fn parse_fn(tokens: &[Token]) -> (Option<FnItem>, &[Token]) {
    let mut remaining = tokens;
    let fn_name;
    let block;

    if remaining.get(0) != Some(&Token::Keyword(Keyword::Fn)) {
        return (None, tokens);
    }
    remaining = &remaining[1..];

    if let Some(&Token::Ident(ref s)) = remaining.get(0) {
        fn_name = s.clone();
    } else {
        return (None, tokens);
    }
    remaining = &remaining[1..];

    if remaining.get(0) != Some(&Token::Delim(Delimiter::LParen)) {
        return (None, tokens);
    }
    remaining = &remaining[1..];

    // TODO: parse parameters

    if remaining.get(0) != Some(&Token::Delim(Delimiter::RParen)) {
        return (None, tokens);
    }
    remaining = &remaining[1..];

    match parse_block(remaining) {
        (Ok(s), remain) => {
            block = s;
            remaining = remain;
        }
        _ => return (None, tokens),
    }

    (Some(FnItem {
        block: block,
        name: fn_name,
        params: vec![],
    }),
     remaining)
}

pub fn parse_block(tokens: &[Token]) -> (Result<Block, ()>, &[Token]) {
    if tokens.get(0) != Some(&Token::Delim(Delimiter::LCurly)) {
        return (Err(()), tokens);
    }

    let mut block_stmts = Vec::new();
    let mut remaining = &tokens[1..];

    loop {
        match parse_stmt(remaining) {
            (Some(s), r) => {
                block_stmts.push(s);
                remaining = r;
            }
            _ => break,
        }
        if remaining.get(0) != Some(&Token::Delim(Delimiter::Semicolon)) {
            return (Err(()), remaining);
        }
        remaining = &remaining[1..];
    }

    if remaining.get(0) != Some(&Token::Delim(Delimiter::RCurly)) {
        return (Err(()), remaining);
    }

    (Ok(Block(block_stmts)), &remaining[1..])
}

pub fn parse_stmt(tokens: &[Token]) -> (Option<Stmt>, &[Token]) {
    // TODO: if, while
    match parse_assignstmt(tokens) {
        (Some(s), remain) => return (Some(Stmt::Assign(s)), remain),
        _ => (),
    }

    match parse_declstmt(tokens) {
        (Some(s), remain) => return (Some(Stmt::Decl(s)), remain),
        _ => (),
    }

    (None, tokens)
}

pub fn parse_assignstmt(tokens: &[Token]) -> (Option<AssignStmt>, &[Token]) {
    let var_name = if let Some(&Token::Ident(ref s)) = tokens.get(0) {
        s.clone()
    } else {
        return (None, tokens);
    };

    if tokens.get(1) != Some(&Token::Op(Operator::Assign)) {
        return (None, tokens);
    }

    let expr_result = parse_expr(&tokens[2..]);
    if expr_result.0.is_none() {
        return (None, tokens);
    }

    (Some(AssignStmt {
        ident: var_name,
        expr: expr_result.0.unwrap(),
    }),
     expr_result.1)
}

pub fn parse_declstmt(tokens: &[Token]) -> (Option<DeclStmt>, &[Token]) {
    let var_name;
    let typ: Type;

    match tokens.get(0) {
        Some(&Token::Keyword(Keyword::Var)) => (),
        _ => return (None, tokens),
    }

    match tokens.get(1) {
        Some(&Token::Ident(ref s)) => var_name = s.clone(),
        _ => return (None, tokens),
    }

    match tokens.get(2) {
        Some(&Token::Keyword(Keyword::Byte)) => {
            typ = Type::Byte;
        }
        Some(&Token::Keyword(Keyword::Int)) => {
            typ = Type::Int;
        }
        Some(&Token::Keyword(Keyword::Void)) => {
            typ = Type::Void;
        }
        _ => {
            return (None, tokens);
        }
    }

    (Some(DeclStmt {
        ident: var_name,
        typ: typ,
    }),
     &tokens[3..])
}

mod expr;
mod test;
