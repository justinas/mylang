use super::super::parser::Type;
#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    BreakOutOfContext,
    InvalidType(Type),
    NoMainFunction,
    RequiresNArgs(usize),
    SymbolNotFound(String),
    TypesIncompatible(Type, Type),
    VariableRedeclared(String),
}
