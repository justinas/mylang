use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub enum ErrorVariant {
    EOF,
    UnknownCharacter,
    InvalidStringEscape,
}

impl fmt::Display for ErrorVariant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let desc = match *self {
            ErrorVariant::EOF => "unexpected end of file",
            ErrorVariant::UnknownCharacter => "unknown character occured",
            ErrorVariant::InvalidStringEscape => "invalid escape character in string",
        };
        write!(f, "{}", desc)
    }
}

#[derive(Debug)]
pub struct Error {
    pub line: u64,
    pub chr: u64,
    pub var: ErrorVariant,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "Lexer error: {} at line {}, char {}",
               self.var,
               self.line,
               self.chr)
    }
}
