use std::fs::File;
use std::io::{stderr, Write};

fn pretty_print(tokens: &Vec<lexer::TokenAt>) {
    println!("Tokens:");
    for t in tokens {
        println!("    {:?}", t);
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
            pretty_print(&t);
            write!(stderr(), "{}", e);
            return;
        }
    };

    let tokens: Vec<_> = tokens_at.into_iter().map(|t| t.0).collect();
    println!("{:#?}", parser::parse(&tokens).unwrap())
}

mod lexer;
mod parser;
