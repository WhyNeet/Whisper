use checker::Checker;
use common::module::{ExternalModule, ModuleRegistry};
use js::{
    codegen::Codegen as JsCodegen, transformer::TypedAstTransformer as JsTypedAstTransformer,
};
use lexer::stream::TokenStream;
use parser::Parser;
use resolver::ModuleResolver;
use std::{env, fs, path::PathBuf, sync::Arc};
use tcast::module::Module;

#[derive(Debug, Default)]
pub struct CompilationPipeline {
    registry: Arc<ModuleRegistry>,
}

impl CompilationPipeline {
    pub fn compile_module(&self, contents: String) -> Result<String, Box<dyn std::error::Error>> {
        let module = self.parse_and_resolve_module(contents)?;

        let codegen = JsTypedAstTransformer::new(Arc::clone(&self.registry));
        let program = codegen.run(module);

        Ok(JsCodegen::default().run(program))
    }

    pub fn parse_and_resolve_module(
        &self,
        contents: String,
    ) -> Result<Module, Box<dyn std::error::Error>> {
        let token_stream = TokenStream::new(&contents);
        let mut module = Parser::new(token_stream).run();
        ModuleResolver::new(Arc::clone(&self.registry)).run(&mut module);

        for dep in module.dependencies.iter() {
            let mut actual_path = dep
                .path
                .iter()
                .fold(PathBuf::new(), |acc, unit| acc.join(unit.as_ref()));
            if actual_path.is_dir() {
                actual_path.push("mod.wr");
            } else {
                actual_path.set_file_name(format!(
                    "{}.wr",
                    actual_path.file_name().unwrap().to_str().unwrap()
                ));
            }
            let absolute_path = env::current_dir().unwrap().join(actual_path);
            let contents = fs::read_to_string(absolute_path)?;

            let module = self.parse_and_resolve_module(contents)?;
            let module = ExternalModule {
                name: dep.path.last().unwrap().clone(),
                symbols: module.symbols,
                module_path: dep.path.clone(),
            };

            self.registry.insert(dep.module_id, module).unwrap();
        }

        Ok(Checker::new(Arc::clone(&self.registry)).run(module))
    }
}
