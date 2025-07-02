use js_codegen::JsCodegen;
use lexer::stream::TokenStream;
use parser::Parser;
use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(&path)?;

    let token_stream = TokenStream::new(&contents);
    let ast = Parser::new(token_stream).run();

    let codegen = JsCodegen::new();
    let output = codegen.generate_module(&ast);

    let (outpath, _) = path.rsplit_once(".").unwrap();
    let outpath = format!("{outpath}.js");
    fs::write(outpath, output.as_bytes())?;

    Ok(())
}
