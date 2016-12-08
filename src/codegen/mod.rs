use std::collections::hash_map::{Entry, HashMap};

use super::parser;
pub use super::parser::FnItem;
pub use self::error::Error;
pub use self::gen::{Gen, Typed};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Context<'a> {
    arguments: Vec<Symbol>,
    functions: &'a [FnItem],
    loop_depth: usize,
    symbol_stack: Vec<Vec<Symbol>>,
    this_fn: Option<usize>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn find_function(&self, name: &str) -> Option<&FnItem> {
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

    // Peek the top frame.
    //
    // PANICS: if there are no frames in the stack.
    pub fn top_frame(&mut self) -> &Vec<Symbol> {
        self.symbol_stack.last().unwrap()
    }

    // Peek the top frame mutably.
    //
    // PANICS: if there are no frames in the stack.
    pub fn top_frame_mut(&mut self) -> &mut Vec<Symbol> {
        self.symbol_stack.last_mut().unwrap()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
    // Math operations: pop 2, push 1
    Add,
    Sub,
    Div,
    Mul,

    // Comparison operations: pop 2, push 1
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,

    // Logical operations: pop 2, push 1
    And,
    Or,

    // Binary negation: pop 1, push 1
    Neg,

    // Set FP to SP
    Fpush,
    // Restore previous FP
    Fpop,

    // Pop to local word at (fp+i64): pop 1
    Poplw(i64),
    // Pop to nowhere: pop 1
    Popn,
    // Push local word from (fp+i64): push 1
    Pushlw(i64),
    // Push immediate word: push 1
    Pushiw(i64),
    // Push returned value: push 1
    Pushr,

    // Return void
    Ret,
    // Return value: pop 1
    Retw,

    __Marker(Marker),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Marker {
    Break(u64), // u64 = loop depth
    Call(String),
    Jmprel(i64),
    Jmpzrel(i64),
    PushCurPC,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub typ: parser::Type,
}

impl Symbol {
    fn new<T: Into<String>>(name: T, typ: parser::Type) -> Self {
        Symbol {
            name: name.into(),
            typ: typ,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Variable {
    typ: parser::Type,
}

mod error;
mod gen;
mod test;
