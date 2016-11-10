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
pub struct Conditional {
    pub block: Block,
    pub cond: Expr,
}

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
pub struct IfStmt {
    pub _if: Conditional,
    pub _eifs: Vec<Conditional>,
    pub _else: Option<Block>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt {
    Assign(AssignStmt),
    Block(Block),
    Decl(DeclStmt),
    Expr(Expr),
    If(IfStmt),
    While(WhileStmt),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Byte,
    Int,
    Void,
}

#[derive(Debug, Eq, PartialEq)]
pub struct WhileStmt(pub Conditional);

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
        while let Some(&Token::Delim(Delimiter::Semicolon)) = remaining.get(0) {
            remaining = &remaining[1..];
        }
        match parse_stmt(remaining) {
            (Some(s), r) => {
                block_stmts.push(s);
                remaining = r;
            }
            _ => break,
        }
        match (remaining.get(0), block_stmts.last().unwrap()) {
            // Semicolons are always okay after a statement
            (Some(&Token::Delim(Delimiter::Semicolon)), _) => (),

            // It's okay NOT to follow block statements by a semicolon
            (_, &Stmt::Block(..)) => (),
            // Ditto
            (_, &Stmt::If(..)) => (),
            // Ditto
            (_, &Stmt::While(..)) => (),

            _ => break,
        }
    }

    if remaining.get(0) != Some(&Token::Delim(Delimiter::RCurly)) {
        return (Err(()), remaining);
    }

    (Ok(Block(block_stmts)), &remaining[1..])
}

pub fn parse_stmt(tokens: &[Token]) -> (Option<Stmt>, &[Token]) {
    match parse_ifstmt(tokens) {
        (Some(s), remain) => return (Some(Stmt::If(s)), remain),
        _ => (),
    }

    match parse_whilestmt(tokens) {
        (Some(s), remain) => return (Some(Stmt::While(s)), remain),
        _ => (),
    }

    match parse_assignstmt(tokens) {
        (Some(s), remain) => return (Some(Stmt::Assign(s)), remain),
        _ => (),
    }

    match parse_declstmt(tokens) {
        (Some(s), remain) => return (Some(Stmt::Decl(s)), remain),
        _ => (),
    }

    match parse_expr(tokens) {
        (Some(e), remain) => return (Some(Stmt::Expr(e)), remain),
        _ => (),
    }

    (None, tokens)
}

pub fn parse_ifstmt(tokens: &[Token]) -> (Option<IfStmt>, &[Token]) {
    if tokens.get(0) != Some(&Token::Keyword(Keyword::If)) {
        return (None, tokens);
    }

    let mut remaining = &tokens[1..];

    let cond = match parse_expr(remaining) {
        (Some(e), r) => {
            remaining = r;
            e
        }
        _ => return (None, tokens),
    };

    let block = match parse_block(remaining) {
        (Ok(b), r) => {
            remaining = r;
            b
        }
        _ => return (None, tokens),
    };

    let mut ifstmt = IfStmt {
        _if: Conditional {
            block: block,
            cond: cond,
        },
        _eifs: vec![],
        _else: None,
    };

    while let (Some(&Token::Keyword(Keyword::Else)), Some(&Token::Keyword(Keyword::If))) =
              (remaining.get(0), remaining.get(1)) {
        let mut my_remaining = &remaining[2..];

        let cond = match parse_expr(my_remaining) {
            (Some(e), r) => {
                my_remaining = r;
                e
            }
            _ => return (None, tokens),
        };

        let block = match parse_block(my_remaining) {
            (Ok(b), r) => {
                my_remaining = r;
                b
            }
            _ => return (None, tokens),
        };
        ifstmt._eifs.push(Conditional {
            block: block,
            cond: cond,
        });
        remaining = my_remaining
    }

    if let Some(&Token::Keyword(Keyword::Else)) = remaining.get(0) {
        let mut my_remaining = &remaining[1..];
        let block = match parse_block(my_remaining) {
            (Ok(b), r) => {
                my_remaining = r;
                b
            }
            _ => return (Some(ifstmt), remaining),
        };
        ifstmt._else = Some(block);
        remaining = my_remaining;
    }

    (Some(ifstmt), remaining)
}


pub fn parse_whilestmt(tokens: &[Token]) -> (Option<WhileStmt>, &[Token]) {
    if tokens.get(0) != Some(&Token::Keyword(Keyword::While)) {
        return (None, tokens);
    }

    let mut remaining = &tokens[1..];

    let cond = match parse_expr(remaining) {
        (Some(e), r) => {
            remaining = r;
            e
        }
        _ => return (None, tokens),
    };

    let block = match parse_block(remaining) {
        (Ok(b), r) => {
            remaining = r;
            b
        }
        _ => return (None, tokens),
    };

    (Some(WhileStmt(Conditional {
        block: block,
        cond: cond,
    })),
     remaining)
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
