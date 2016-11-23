mod ast;
mod interpreter;
mod lexer;
mod parser;
mod util;

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        println!("Usage: basic-rs program.bas");
        return
    }

    let path = Path::new(&args[1]);
    let display = path.display();

    let mut file = match File::open(&path) {
        // The `description` method of `io::Error` returns a string that
        // describes the error
        Err(why) => panic!("couldn't open {}: {}", display, why.description()),
        Ok(file) => file,
    };

    // Read the file contents into a string, returns `io::Result<usize>`
    let mut s = String::new();
    if let Err(why) = file.read_to_string(&mut s) {
        panic!("couldn't read {}: {}", display, why.description());
    }

    let mut tokens = vec!();
    for lexer_result in lexer::Lexer::new(&s) {
        match lexer_result {
            Err(error) => {
                println!("{}", error);
                return;
            },
            Ok(token) => {
                println!("Token: {}", token);
                tokens.push(token);
            },
        }
    }

    let parser = parser::Parser::new(tokens);
    let ast = match parser.run() {
        Err(error) => {
            println!("{}", error);
            return;
        },
        Ok(ast) => {
            println!("Ast: {:#?}", ast);
            ast
        },
    };

    let mut interpreter = interpreter::Interpreter::new(ast);
    match interpreter.run() {
        Err(error) => {
            println!("{:?}", error);
            return;
        },
        Ok(()) => {
            println!("YYYYYYYYYEEEEEESSSSSSSS");
        }
    }
}