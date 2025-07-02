use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(path)?;

    let mut token_stream = lexer::tokenize(&contents);

    dbg!(token_stream.next());

    Ok(())
}
