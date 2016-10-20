use std;
use std::io;
use std::io::Lines;

pub use self::error::{Error, ErrorVariant};

#[derive(Debug, Eq, PartialEq)]
pub enum Delimiter {
    Comma,
    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,
}

impl Delimiter {
    fn from_char(c: char) -> Self {
        match c {
            ',' => Delimiter::Comma,
            '(' => Delimiter::LParen,
            ')' => Delimiter::RParen,
            '{' => Delimiter::LCurly,
            '}' => Delimiter::RCurly,
            '[' => Delimiter::LSquare,
            ']' => Delimiter::RSquare,
            _ => unreachable!(),
        }
    }
}


#[derive(Debug, Eq, PartialEq)]
pub enum Operator {
    Minus,
    Plus,
    Mul,
    Div,

    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    Not,

    Assign,
}

impl Operator {
    fn from_char(c: char) -> Self {
        match c {
            '+' => Operator::Plus,
            '*' => Operator::Mul,
            '/' => Operator::Div,
            '>' => Operator::Gt,
            '<' => Operator::Lt,
            '!' => Operator::Not,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Const(String),
    Delim(Delimiter),
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

        let l = Lexer { chars: chars.into() };

        Ok(l)
    }

    fn error(&self, var: ErrorVariant) -> Error {
        Error {
            line: self.chars.line_pos,
            chr: self.chars.char_pos,
            var: var,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, (Vec<Token>, Error)> {
        let mut tokens: Vec<Token> = vec![];
        'main: loop {
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

                '!' => {
                    self.chars.next();
                    if self.chars.peek().is_none() {
                        Ok(Token::Op(Operator::Not))
                    } else {
                        match *self.chars.peek().unwrap() {
                            '=' => {
                                self.chars.next();
                                Ok(Token::Op(Operator::Neq))
                            }
                            _ => Ok(Token::Op(Operator::Not)),
                        }
                    }
                }

                '=' => {
                    self.chars.next();
                    if self.chars.peek().is_none() {
                        Ok(Token::Op(Operator::Assign))
                    } else {
                        match *self.chars.peek().unwrap() {
                            '=' => {
                                self.chars.next();
                                Ok(Token::Op(Operator::Eq))
                            }
                            _ => Ok(Token::Op(Operator::Assign)),
                        }
                    }
                }

                '/' => {
                    self.chars.next();
                    if self.chars.peek().is_none() {
                        Ok(Token::Op(Operator::Div))
                    } else {
                        match *self.chars.peek().unwrap() {
                            // Single-line comment
                            '/' => {
                                self.chars.next();
                                loop {
                                    match self.chars.next() {
                                        Some('\n') | None => continue 'main,
                                        _ => (),
                                    }
                                }
                            }
                            '*' => {
                                self.chars.next();
                                loop {
                                    match self.chars.next() {
                                        Some('*') => {
                                            match self.chars.next() {
                                                Some('/') => continue 'main,
                                                None => {
                                                    return Err((tokens,
                                                                self.error(ErrorVariant::EOF)))
                                                }
                                                _ => (),
                                            }
                                        }
                                        Some(_) => (),
                                        None => return Err((tokens, self.error(ErrorVariant::EOF))),

                                    }
                                }
                            }
                            _ => Ok(Token::Op(Operator::Div)),
                        }
                    }
                }

                '+' | '*' => Ok(Token::Op(Operator::from_char(self.chars.next().unwrap()))),
                ',' | '(' | ')' | '{' | '}' | '[' | ']' => {
                    Ok(Token::Delim(Delimiter::from_char(self.chars.next().unwrap())))
                }
                '<' | '>' => self.lex_ltgt(),
                '0'...'9' => self.lex_const(false),
                'A'...'Z' | 'a'...'z' => self.lex_ident(),
                '"' => self.lex_str(),

                c if c.is_whitespace() => {
                    self.chars.next();
                    continue;
                }

                c => return Err((tokens, self.error(ErrorVariant::UnknownCharacter))),
            };

            match res {
                Ok(token) => tokens.push(token),
                Err(e) => {
                    let err = Error {
                        line: self.chars.line_pos,
                        chr: self.chars.char_pos,
                        var: e,
                    };
                    return Err((tokens, err));
                }
            }
        }
        Ok(tokens)
    }

    fn lex_const(&mut self, negative: bool) -> Result<Token, ErrorVariant> {
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
                'A'...'Z' | 'a'...'z' => return Err(ErrorVariant::UnknownCharacter),
                _ => break,
            }
        }

        return Ok(Token::Const(val));
    }

    fn lex_ident(&mut self) -> Result<Token, ErrorVariant> {
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

    fn lex_ltgt(&mut self) -> Result<Token, ErrorVariant> {
        let this = self.chars.next().unwrap();
        if self.chars.peek().is_none() {
            return Ok(Token::Op(Operator::from_char(this)));
        }
        let op;
        match (this, *self.chars.peek().unwrap()) {
            ('>', '=') => {
                self.chars.next();
                op = Operator::Gte;
            }
            ('<', '=') => {
                self.chars.next();
                op = Operator::Lte;
            }
            _ => op = Operator::from_char(this),
        }
        Ok(Token::Op(op))
    }

    fn lex_str(&mut self) -> Result<Token, ErrorVariant> {
        let mut val = String::new();
        assert!(self.chars.next() == Some('"'));

        loop {
            if self.chars.peek().is_none() {
                return Err(ErrorVariant::EOF);
            }
            match self.chars.next() {
                Some('\\') => {
                    if self.chars.peek().is_none() {
                        return Err(ErrorVariant::EOF);
                    }
                    match *self.chars.peek().unwrap() {
                        '\\' => val.push('\\'),
                        '\"' => val.push('\"'),
                        'n' => val.push('\n'),
                        't' => val.push('\t'),
                        _ => {
                            return Err(ErrorVariant::InvalidStringEscape);
                        }
                    }
                    self.chars.next();
                }
                Some('"') => break,
                Some(c) => val.push(c),
                None => unreachable!(),
            }
        }

        return Ok(Token::String(val));
    }
}

mod error;
mod test;
