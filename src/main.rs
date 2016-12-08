use std::fs::File;
use std::io::{stderr, Write};

fn pretty_print(v: &Vec<codegen::Instruction>) {
    for (pos, ins) in v.iter().enumerate() {
        println!("{:>04}: {:?}", pos, ins);
    }
}

fn main() {
    let filename = match std::env::args().skip(1).next() {
        Some(f) => f,
        None => {
            write!(stderr(), "Usage: ./mylang [filename]");
            return;
        }
    };

    let f = match File::open(&filename) {
        Ok(f) => f,
        Err(e) => {
            write!(stderr(), "Error while opening {}: {}", filename, e);
            return;
        }
    };

    let lexer = match lexer::Lexer::new(f) {
        Ok(l) => l,
        Err(e) => {
            write!(stderr(), "Lexer I/O error: {:?}", e);
            return;
        }
    };

    let tokens_at = match lexer.lex() {
        Ok(t) => t,
        Err((t, e)) => {
            write!(stderr(), "{}", e);
            return;
        }
    };

    let tokens: Vec<_> = tokens_at.into_iter().map(|t| t.0).collect();
    let funcs = match parser::parse(&tokens) {
        Ok(f) => f,
        Err(_) => {
            write!(stderr(), "Syntax error.");
            return;
        }
    };

    match codegen::parse_program(&funcs) {
        Ok(v) => pretty_print(&v),
        Err(e) => {
            write!(stderr(), "{:?}", e);
        }
    }
}

pub mod codegen;
pub mod lexer;
pub mod parser;
