use blisp::{self, error::InterpreteResult, lexer::tokenize};

fn main() -> InterpreteResult<()> {
    // Read the input file
    let input = std::fs::read_to_string("programs/fib.bl").unwrap();

    println!("Result: {}", blisp::run(&input)?);

    Ok(())
}
