use std::cmp::Ordering;
use std::collections::hash_map::{Entry, HashMap};

use super::parser;
pub use super::parser::FnItem;
pub use self::error::Error;
pub use self::gen::{Gen, Typed};
use self::Instruction::*;

#[derive(Debug)]
pub struct Program {
    pub func_locations: HashMap<String, usize>,
    pub instructions: Vec<Instruction>,
}

pub fn parse_program(funcs: &[FnItem]) -> Result<Program, Error> {
    let mut funcs = funcs.to_vec();
    funcs.sort_by(|a, b| if a.name == "main" {
        Ordering::Less
    } else {
        Ordering::Equal
    });
    if funcs[0].name != "main" {
        return Err(Error::NoMainFunction);
    }

    let mut func_locations = HashMap::new();
    let mut v = vec![];

    for (pos, f) in funcs.iter().enumerate() {
        let mut ctx = Context {
            arguments: f.params.iter().map(|p| Symbol::new(p.name.clone(), p.typ)).collect(),
            functions: &funcs,
            loop_depth: 1,
            symbol_stack: vec![],
            this_fn: Some(pos),
        };
        func_locations.insert(f.name.clone(), v.len());
        v.extend_from_slice(&f.gen(&mut ctx)?);
    }

    // Resolve markers
    for (pos, ins) in v.iter_mut().enumerate() {
        *ins = match *ins {
            __Marker(ref mut m) => {
                match *m {
                    Marker::Break(_) => unreachable!(),
                    Marker::Call(ref s) => Call(func_locations[s] as u64),
                    Marker::Jmprel(offset) => Jmp((pos as i64 + offset) as u64),
                    Marker::Jmpzrel(offset) => Jmpz((pos as i64 + offset) as u64),
                }
            }
            ref i => i.clone(),
        }
    }

    Ok(Program {
        func_locations: func_locations,
        instructions: v,
    })
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Context<'a> {
    arguments: Vec<Symbol>,
    functions: &'a [FnItem],
    loop_depth: usize,
    symbol_stack: Vec<Vec<Symbol>>,
    this_fn: Option<usize>,
}

impl<'a> Context<'a> {
    fn new() -> Self {
        Default::default()
    }

    fn find_function(&self, name: &str) -> Option<&FnItem> {
        self.functions.iter().find(|f| f.name == name)
    }

    fn find_symbol(&self, name: &str) -> Option<&Symbol> {
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
    fn find_symbol_location(&self, name: &str) -> Option<isize> {
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
    fn pop_frame(&mut self) -> Vec<Symbol> {
        self.symbol_stack.pop().unwrap()
    }

    // Push a new frame of locals (say, when a new block is allocated)
    fn push_frame(&mut self, frame: Vec<Symbol>) {
        self.symbol_stack.push(frame);
    }

    // Pushes a symbol to the last frame.
    //
    // PANICS: if there are no frames in the stack.
    fn push_symbol(&mut self, symbol: Symbol) {
        self.symbol_stack.last_mut().unwrap().push(symbol)
    }

    // Peek the top frame.
    //
    // PANICS: if there are no frames in the stack.
    fn top_frame(&mut self) -> &Vec<Symbol> {
        self.symbol_stack.last().unwrap()
    }

    // Peek the top frame mutably.
    //
    // PANICS: if there are no frames in the stack.
    fn top_frame_mut(&mut self) -> &mut Vec<Symbol> {
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

    // Call: push ret addr & jmp
    Call(u64),

    // Absolute jump
    Jmp(u64),
    // Absolute jump-if-zero
    Jmpz(u64),

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
