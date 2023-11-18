mod environment;
mod expr;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod stmt;
mod tests;

use crate::interpreter::Interpreter;
use crate::resolver::Resolver;
use crate::scanner::Scanner;
use anyhow::Result;
use std::cell::RefCell;
use std::io::Write;
use std::process::exit;
use std::rc::Rc;
use std::{env, fs};

fn run(interpreter: Rc<RefCell<Interpreter>>, source: &str) -> Result<()> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan()?;

    let mut parser = parser::Parser::new(tokens);
    let stmts = parser.parse()?;

    let mut resolver = Resolver::new(interpreter);
    resolver.resolve_statements(&stmts.iter().collect())?;

    resolver
        .interpreter
        .borrow_mut()
        .interpret(stmts.iter().collect())?;

    Ok(())
}

pub fn run_string(source: &str) -> Result<()> {
    let interpreter = Rc::new(RefCell::new(Interpreter::new()));

    run(interpreter, source)
}

pub fn run_file(path: &str) -> Result<()> {
    let interpreter = Rc::new(RefCell::new(Interpreter::new()));
    let contents = fs::read_to_string(path)?;

    run(interpreter, &contents)
}

fn run_prompt() -> Result<()> {
    let interpreter = Rc::new(RefCell::new(Interpreter::new()));

    loop {
        print!("> ");
        std::io::stdout().flush()?;
        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;

        match run(interpreter.clone(), &input) {
            Ok(_) => (),
            Err(e) => println!("Error: {}", e),
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 2 {
        match run_file(&args[1]) {
            Ok(_) => exit(0),
            Err(e) => {
                println!("Error: {}", e);
                exit(1);
            }
        }
    } else if args.len() == 3 {
        match run_string(&args[2]) {
            Ok(_) => exit(0),
            Err(e) => {
                println!("Error: {}", e);
                exit(1);
            }
        }
    } else if args.len() == 1 {
        match run_prompt() {
            Ok(_) => exit(0),
            Err(e) => {
                println!("Error: {}", e);
                exit(1);
            }
        }
    } else {
        println!("Usage: {} <name>", args[0]);
        exit(64);
    }
}
