pub use super::lexer::{Keyword, Operator, Token, TokenAt};
use self::expr::Expr;
use self::expr::parse_expr;

#[derive(Debug, Eq, PartialEq)]
pub struct AssignStmt {
    pub ident: String,
    pub expr: expr::Expr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct DeclStmt {
    pub ident: String,
    pub typ: Type,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt {
    Assign(AssignStmt),
    Decl(DeclStmt),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Byte,
    Int,
    Void,
}

pub fn parse_stmt(tokens: &[Token]) -> (Option<Stmt>, &[Token]) {
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
