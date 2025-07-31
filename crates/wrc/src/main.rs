use clap::Parser;
use wrc::{
    args::{Args, Commands},
    commands::compile,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    match args.command {
        Commands::Compile { filename } => compile::run(filename),
    }
}
