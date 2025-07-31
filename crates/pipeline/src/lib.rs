use checker::Checker;
use common::module::{ExternalModule, ModuleRegistry};
use js::{
    codegen::Codegen as JsCodegen, transformer::TypedAstTransformer as JsTypedAstTransformer,
};
use lexer::stream::TokenStream;
use parser::Parser;
use resolver::ModuleResolver;
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};
use string_cache::DefaultAtom as Atom;
use tcast::module::Module;

#[derive(Debug, Default)]
pub struct CompilationPipeline {
    registry: Arc<ModuleRegistry>,
}

impl CompilationPipeline {
    pub fn compile_module(
        &self,
        contents: String,
        name: &str,
        root_dir: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let artifacts = self.parse_and_resolve_module(contents, Atom::from(name), root_dir)?;

        for artifact in artifacts {
            let codegen = JsTypedAstTransformer::new(Arc::clone(&self.registry));
            let program = codegen.run(artifact.module);
            let contents = JsCodegen::default().run(program);

            let path = artifact
                .parent_dir
                .join(format!("{}.js", artifact.name.as_ref()));
            fs::write(path, contents)?;
        }

        Ok(())
    }

    pub fn parse_and_resolve_module(
        &self,
        contents: String,
        name: Atom,
        root_dir: &Path,
    ) -> Result<Vec<Artifact>, Box<dyn std::error::Error>> {
        let token_stream = TokenStream::new(&contents);
        let mut module = Parser::new(token_stream).run();
        ModuleResolver::new(Arc::clone(&self.registry)).run(&mut module);

        let mut artifacts = vec![];

        for dep in module.dependencies.iter() {
            let actual_path = dep
                .path
                .iter()
                .fold(PathBuf::new(), |acc, unit| acc.join(unit.as_ref()));
            let mut actual_path = root_dir.join(actual_path);
            if actual_path.is_dir() {
                actual_path.push("mod.wr");
            } else {
                actual_path.set_file_name(format!(
                    "{}.wr",
                    actual_path.file_name().unwrap().to_str().unwrap()
                ));
            }
            let contents = fs::read_to_string(actual_path)?;

            let mut dep_artifacts = self.parse_and_resolve_module(
                contents,
                dep.path.last().unwrap().clone(),
                root_dir,
            )?;
            let artifact = dep_artifacts.last().unwrap();
            let module = ExternalModule {
                name: dep.path.last().unwrap().clone(),
                symbols: artifact.module.symbols.clone(),
                module_path: dep.path.clone(),
            };
            artifacts.append(&mut dep_artifacts);

            self.registry.insert(dep.module_id, module).unwrap();
        }

        artifacts.push(Artifact {
            name,
            parent_dir: root_dir.to_path_buf(),
            module: Checker::new(Arc::clone(&self.registry)).run(module),
        });

        Ok(artifacts)
    }
}

pub struct Artifact {
    pub name: Atom,
    pub parent_dir: PathBuf,
    pub module: Module,
}
