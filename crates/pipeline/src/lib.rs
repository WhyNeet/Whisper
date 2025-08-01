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
    fn resolve_module_path(
        &self,
        module_parts: &[Atom],
        root_dir: &Path,
    ) -> Result<PathBuf, Box<dyn std::error::Error>> {
        let mut path = root_dir.to_path_buf();

        for part in module_parts {
            path.push(part.as_ref());
        }

        let candidates = [path.join("mod.wr"), path.with_extension("wr")];

        for candidate in candidates {
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        Err("Module not found.".into())
    }

    pub fn compile_module(
        &self,
        contents: String,
        root_dir: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let artifacts = self.parse_and_resolve_module(contents, Atom::from("index"), root_dir)?;

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
        module_name: Atom,
        root_dir: &Path,
    ) -> Result<Vec<Artifact>, Box<dyn std::error::Error>> {
        let token_stream = TokenStream::new(&contents);
        let mut module = Parser::new(token_stream).run();
        ModuleResolver::new(Arc::clone(&self.registry)).run(&mut module, root_dir);

        let mut artifacts = vec![];

        for dep in module.dependencies.iter() {
            let module_path = self.resolve_module_path(&dep.relative_path, root_dir)?;
            let contents = fs::read_to_string(&module_path)?;

            let mut dep_artifacts = self.parse_and_resolve_module(
                contents,
                if module_path.file_stem().unwrap().eq("mod") {
                    Atom::from("index")
                } else {
                    dep.relative_path.last().unwrap().clone()
                },
                module_path.parent().unwrap(),
            )?;
            let artifact = dep_artifacts.last().unwrap();
            let module = ExternalModule {
                id: dep.module_id,
                name: artifact.name.clone(),
                symbols: artifact.module.symbols.clone(),
                module_path: dep.relative_path.clone(),
            };
            artifacts.append(&mut dep_artifacts);

            self.registry.insert(dep.module_id, module).unwrap();
        }

        artifacts.push(Artifact {
            name: module_name,
            parent_dir: root_dir.to_path_buf(),
            module: Checker::new(Arc::clone(&self.registry)).run(module),
        });

        Ok(artifacts)
    }
}

#[derive(Debug)]
pub struct Artifact {
    pub name: Atom,
    pub parent_dir: PathBuf,
    pub module: Module,
}
