use blisp::{self, error::InterpreteResult, lexer::tokenize};

fn main() -> InterpreteResult<()> {
    // Read the input file
    let input = std::fs::read_to_string("programs/collatz.bl").unwrap();

    let tokens = tokenize(input.chars().collect())?;


    println!("Result: {}", blisp::run(&input)?);

    Ok(())
}
