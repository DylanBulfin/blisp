use error::InterpreteResult;
use interpreter::Value;

pub mod error;
pub mod functions;
pub mod interpreter;
pub mod lexer;
pub mod macros;
pub mod parser;

pub fn run(input: &str) -> InterpreteResult<Value> {
    let mut tokens = lexer::tokenize(input.chars().collect())?;
    let node = parser::parse_prog(&mut tokens)?;
    interpreter::eval(node)
}
