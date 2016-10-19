use std;
use std::io;
use std::io::Lines;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Ident(String),
}

pub struct Lexer {
    chars: std::iter::Peekable<std::vec::IntoIter<char>>,

    line_pos: u64,
    char_pos: u64,
}

impl Lexer {
    pub fn new<R: io::Read>(mut r: R) -> Result<Lexer, io::Error> {
        let mut contents = String::new();
        try!(r.read_to_string(&mut contents));
        let chars: Vec<char> = contents.chars().collect();

        let mut l = Lexer {
            chars: chars.into_iter().peekable(),
            line_pos: 1,
            char_pos: 1,
        };

        Ok(l)
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, (Vec<Token>, String)> {
        let mut tokens: Vec<Token> = vec![];
        loop {
            if self.chars.peek().is_none() {
                break;
            }

            let res = match *self.chars.peek().unwrap() {
                'A'...'Z' | 'a'...'z' => self.lex_ident(),
                ' ' | '\t' => {
                    self.chars.next();
                    continue;
                }
                c => return Err((vec![], format!("Unexpected character `{}`", c))),
            };

            match res {
                Ok(token) => tokens.push(token),
                Err(e) => return Err((vec![], String::new())),
            }
        }
        Ok(tokens)
    }

    fn lex_ident(&mut self) -> Result<Token, ()> {
        let mut ident = String::new();
        loop {
            if self.chars.peek().is_none() {
                break;
            }
            match *self.chars.peek().unwrap() {
                'A'...'Z' | 'a'...'z' | '0'...'9' => ident.push(self.chars.next().unwrap()),
                _ => break,
            }
        }

        return Ok(Token::Ident(ident));
    }
}

mod test;
