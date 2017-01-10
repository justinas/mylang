use std::collections::HashMap;
use std::fs::File;
use std::io::{stderr, Write};

fn pretty_print(program: &codegen::Program) {
    let reverse_fn_map: HashMap<usize, String> =
        program.func_locations.iter().map(|(name, pos)| (*pos, name.clone())).collect();
    for (pos, ins) in program.instructions.iter().enumerate() {
        print!("{:>04}: {:30}", pos, format!("{:?}", ins));
        if let Some(func_name) = reverse_fn_map.get(&pos) {
            print!("# {}", func_name);
        }
        print!("\n");
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

    let p = match codegen::parse_program(&funcs) {
        Ok(p) => {
            pretty_print(&p);
            p
        }
        Err(e) => {
            write!(stderr(), "{:?}", e);
            return;
        }
    };

    println!("");
    println!("-----------------");
    println!("");
    let mut bytes = vec![];
    p.encode(&mut bytes).unwrap();
    let mut machine = vm::Machine::new(&bytes);
    println!("{}", machine.run());
}

pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod vm;
