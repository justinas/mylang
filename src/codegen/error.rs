#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    VariableRedefined(String),
}
