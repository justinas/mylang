pub use super::lexer::{Keyword, Operator, Token, TokenAt};

#[derive(Debug, Eq, PartialEq)]
pub struct DeclStmt {
    pub ident: String,
    pub typ: Type,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Byte,
    Int,
    Void,
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
