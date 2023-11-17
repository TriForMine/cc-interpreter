mod environment;
mod expr;
mod interpreter;
mod parser;
mod scanner;
mod stmt;

use crate::interpreter::Interpreter;
use crate::scanner::Scanner;
use std::io::Write;
use std::process::exit;
use std::{env, fs};

fn run(interpreter: &mut Interpreter, source: &str) -> Result<(), std::io::Error> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan()?;

    let mut parser = parser::Parser::new(tokens);
    let stmts = parser.parse()?;

    interpreter.interpret(stmts)?;

    Ok(())
}

fn run_file(path: &str) -> Result<(), std::io::Error> {
    let mut interpreter = Interpreter::new();
    let contents = fs::read_to_string(path)?;

    run(&mut interpreter, &contents)
}

fn run_prompt() -> Result<(), std::io::Error> {
    let mut interpreter = Interpreter::new();

    loop {
        print!("> ");
        std::io::stdout().flush()?;
        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;

        match run(&mut interpreter, &input) {
            Ok(_) => (),
            Err(e) => println!("Error: {}", e),
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("Usage: {} <name>", args[0]);
        exit(64);
    } else if args.len() == 2 {
        match run_file(&args[1]) {
            Ok(_) => exit(0),
            Err(e) => {
                println!("Error: {}", e);
                exit(1);
            }
        }
    } else {
        match run_prompt() {
            Ok(_) => exit(0),
            Err(e) => {
                println!("Error: {}", e);
                exit(1);
            }
        }
    }
}
