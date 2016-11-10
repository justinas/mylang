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
        Ok(t) => {
            pretty_print(&t);
            t
        }
        Err((t, e)) => {
            pretty_print(&t);
            write!(stderr(), "{}", e);
            return;
        }
    };
}

mod lexer;
mod parser;
