use checker::Checker;
use js::{
    codegen::Codegen as JsCodegen, transformer::TypedAstTransformer as JsTypedAstTransformer,
};
use lexer::stream::TokenStream;
use parser::Parser;
use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(&path)?;

    let token_stream = TokenStream::new(&contents);
    let module = Parser::new(token_stream).run();

    let module = Checker::new().run(&module);

    let codegen = JsTypedAstTransformer::default();
    let program = codegen.run(&module);

    let output = JsCodegen::default().run(program);

    let (outpath, _) = path.rsplit_once(".").unwrap();
    let outpath = format!("{outpath}.js");
    fs::write(outpath, output.as_bytes())?;

    Ok(())
}
