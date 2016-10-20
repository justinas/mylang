#[derive(Debug, Eq, PartialEq)]
pub enum ErrorVariant {
    EOF,
    UnknownCharacter,
    InvalidStringEscape,
}

#[derive(Debug)]
pub struct Error {
    pub line: u64,
    pub chr: u64,
    pub var: ErrorVariant,
}
