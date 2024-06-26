pub mod brainfuck;

use std::fs;
use std::env;
use brainfuck::interpreter::BrainfuckInterpreter;

#[derive(Debug)]
#[allow(dead_code)]
enum ErrorCode {
    Success,
    Failure
}

fn main() -> Result<(), ErrorCode> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: ./brainfuck <filename>");
        return Err(ErrorCode::Failure);
    }

    let program = fs::read_to_string(&args[1])
        .expect("Should have been able to read the file");
    let interpreter = BrainfuckInterpreter::new(String::from(program));
    interpreter.run();

    return Ok(());
}
