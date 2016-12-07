use super::super::parser::Type;
#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    InvalidType(Type),
    RequiresNArgs(usize),
    SymbolNotFound(String),
    TypesIncompatible(Type, Type),
    VariableRedefined(String),
}
