use pipeline::CompilationPipeline;
use std::{env, fs, path::PathBuf};

pub fn run(filename: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let current_dir = env::current_dir()?;
    let file_path = current_dir.join(filename);
    let contents = fs::read_to_string(&file_path)?;

    let pipeline = CompilationPipeline::default();
    pipeline.compile_module(
        contents,
        file_path.file_stem().unwrap().to_str().unwrap(),
        file_path.parent().unwrap(),
    )?;

    let outdir = current_dir.join("dist");
    fs::create_dir_all(&outdir)?;

    Ok(())
}
