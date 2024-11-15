use blisp::{self, error::InterpreteResult};

fn main() -> InterpreteResult<()> {
    // Read the input file
    let input = std::fs::read_to_string("programs/fib2.bl").unwrap();

    println!("Result: {}", blisp::run(&input)?);

    Ok(())
}
