use std::collections::hash_map::{Entry, HashMap};

use super::parser;
pub use self::error::Error;
pub use self::gen::{Gen, Typed};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Context<'a> {
    arguments: Vec<Symbol>,
    functions: &'a [parser::FnItem],
    symbol_stack: Vec<Vec<Symbol>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn find_function(&self, name: &str) -> Option<&parser::FnItem> {
        self.functions.iter().find(|f| f.name == name)
    }

    pub fn find_symbol(&self, name: &str) -> Option<&Symbol> {
        if let Some(p) = self.arguments.iter().rev().find(|s| s.name == name) {
            return Some(p);
        }
        self.symbol_stack
            .iter()
            .rev()
            .flat_map(|frame| frame.iter().rev())
            .find(|s| s.name == name)
    }

    // Finds the symbol offset off the frame.
    pub fn find_symbol_location(&self, name: &str) -> Option<isize> {
        if let Some(p) = self.arguments.iter().rev().position(|s| s.name == name) {
            return Some((p + 1) as isize);
        }
        let full_len = self.symbol_stack.iter().flat_map(|frame| frame.iter()).count() as isize;
        self.symbol_stack
            .iter()
            .rev()
            .flat_map(|frame| frame.iter().rev())
            .position(|s| s.name == name)
            .map(|p| -full_len + p as isize)
    }

    // Pop the top frame of the stack.
    //
    // PANICS: if there are no frames in the stack.
    pub fn pop_frame(&mut self) -> Vec<Symbol> {
        self.symbol_stack.pop().unwrap()
    }

    // Push a new frame of locals (say, when a new block is allocated)
    pub fn push_frame(&mut self, frame: Vec<Symbol>) {
        self.symbol_stack.push(frame);
    }

    // Pushes a symbol to the last frame.
    //
    // PANICS: if there are no frames in the stack.
    pub fn push_symbol(&mut self, symbol: Symbol) {
        self.symbol_stack.last_mut().unwrap().push(symbol)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    locals: HashMap<String, Variable>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
    // Math operations: pop 2, push 1
    Add,
    Sub,
    Div,
    Mul,

    // Logical operations: pop 2, push 1
    And,
    Or,

    // Binary negation: pop 1, push 1
    Neg,

    // Pop to nowhere: pop 1
    Popn,
    // Push local word: push 1
    Pushlw(i64),
    // Push immediate word: push 1
    Pushiw(i64),
    // Push return value: push 1
    Pushr,

    __Marker(Marker),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Marker {
    Call(String),
    PushCurPC,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Symbol {
    pub name: String,
}

impl Function {
    pub fn new(item: parser::FnItem) -> Result<Self, Error> {
        let mut func = Function { locals: HashMap::new() };
        func.resolve_locals(item)?;
        Ok(func)
    }

    fn add_local(&mut self, name: &str, typ: parser::Type) -> Result<(), Error> {
        match self.locals.entry(name.into()) {
            Entry::Occupied(_) => return Err(Error::VariableRedefined(name.into())),
            Entry::Vacant(e) => {
                e.insert(Variable { typ: typ });
            }
        }
        Ok(())
    }

    fn add_locals_from_block(&mut self, block: &parser::Block) -> Result<(), Error> {
        for stmt in &block.0 {
            match *stmt {
                parser::Stmt::Block(ref b) => {
                    self.add_locals_from_block(b)?;
                }
                parser::Stmt::Decl(ref d) => {
                    self.add_local(&d.ident, d.typ.clone())?;
                }
                parser::Stmt::If(ref i) => {
                    self.add_locals_from_block(&i._if.block)?;
                    for c in &i._eifs {
                        self.add_locals_from_block(&c.block)?;
                    }
                    if let Some(ref c) = i._else {
                        self.add_locals_from_block(c)?;
                    }
                }
                parser::Stmt::While(ref w) => {
                    self.add_locals_from_block(&w.0.block);
                }
                _ => (),
            }
        }
        Ok(())
    }

    fn resolve_locals(&mut self, item: parser::FnItem) -> Result<(), Error> {
        for p in &item.params {
            self.add_local(&p.name, p.typ.clone());
        }
        self.add_locals_from_block(&item.block)?;
        Ok(())
    }
}

impl Symbol {
    fn new<T: Into<String>>(name: T) -> Self {
        Symbol { name: name.into() }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Variable {
    typ: parser::Type,
}

mod error;
mod gen;
mod test;