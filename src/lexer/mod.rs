use std;
use std::io;
use std::io::Lines;

#[derive(Debug, Eq, PartialEq)]
pub enum Operator {
    Minus,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Const(String),
    Ident(String),
    Op(Operator),
    String(String),
}

struct CharIterator {
    it: std::iter::Peekable<std::vec::IntoIter<char>>,
    line_pos: u64,
    char_pos: u64,
}

impl CharIterator {
    fn peek(&mut self) -> Option<&char> {
        self.it.peek()
    }
}

impl Iterator for CharIterator {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        match self.it.next() {
            c @ Some('\n') => {
                self.line_pos += 1;
                self.char_pos = 1;
                c
            }
            c @ Some(_) => {
                self.char_pos += 1;
                c
            }
            None => None,
        }
    }
}

impl From<Vec<char>> for CharIterator {
    fn from(v: Vec<char>) -> Self {
        CharIterator {
            it: v.into_iter().peekable(),
            line_pos: 1,
            char_pos: 1,
        }
    }
}

pub struct Lexer {
    chars: CharIterator,
}

impl Lexer {
    pub fn new<R: io::Read>(mut r: R) -> Result<Lexer, io::Error> {
        let mut contents = String::new();
        try!(r.read_to_string(&mut contents));
        let chars: Vec<char> = contents.chars().collect();

        let mut l = Lexer { chars: chars.into() };

        Ok(l)
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, (Vec<Token>, String)> {
        let mut tokens: Vec<Token> = vec![];
        loop {
            if self.chars.peek().is_none() {
                break;
            }

            let res = match *self.chars.peek().unwrap() {
                '-' => {
                    self.chars.next();
                    if self.chars.peek().is_none() {
                        Ok(Token::Op(Operator::Minus))
                    } else {
                        match *self.chars.peek().unwrap() {
                            '0'...'9' => self.lex_const(true),
                            _ => Ok(Token::Op(Operator::Minus)),
                        }
                    }
                }
                '0'...'9' => self.lex_const(false),
                'A'...'Z' | 'a'...'z' => self.lex_ident(),
                '"' => self.lex_str(),
                c if c.is_whitespace() => {
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

    fn lex_const(&mut self, negative: bool) -> Result<Token, ()> {
        let mut val = String::new();
        if negative {
            val.push('-');
        }

        loop {
            if self.chars.peek().is_none() {
                break;
            }
            match *self.chars.peek().unwrap() {
                '0'...'9' => val.push(self.chars.next().unwrap()),
                'A'...'Z' | 'a'...'z' => return Err(()), // Unexpected symbol in integer constant
                _ => break,
            }
        }

        return Ok(Token::Const(val));
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

    fn lex_str(&mut self) -> Result<Token, ()> {
        let mut val = String::new();
        assert!(self.chars.next() == Some('"'));

        loop {
            if self.chars.peek().is_none() {
                return Err(()); // Unexpected EOF
            }
            match self.chars.next() {
                Some('\\') => {
                    match self.chars.next() {
                        Some('\\') => val.push('\\'),
                        Some('\"') => val.push('\"'),
                        Some('n') => val.push('\n'),
                        Some('t') => val.push('\t'),
                        Some(c) => return Err(()), // Invalid escape char
                        None => return Err(()), // Unexpected EOF
                    }
                }
                Some('"') => break,
                Some(c) => val.push(c),
                None => unreachable!(),
            }
        }

        return Ok(Token::String(val));
    }
}

mod test;
