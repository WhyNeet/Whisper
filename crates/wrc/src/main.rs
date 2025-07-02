use lexer::stream::TokenStream;
use parser::Parser;
use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(path)?;

    let token_stream = TokenStream::new(&contents);
    let ast = Parser::new(token_stream).run();

    dbg!(ast);

    Ok(())
}
