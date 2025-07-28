use std::{env, fs};

use pipeline::CompilationPipeline;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(&path)?;

    let output = CompilationPipeline::compile_module(contents);

    let (outpath, _) = path.rsplit_once(".").unwrap();
    let outpath = format!("{outpath}.js");
    fs::write(outpath, output.as_bytes())?;

    Ok(())
}
