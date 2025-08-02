use ast::{
    module::{Dependency, Module},
    stmt::{Import, Statement},
};
use common::module::ModuleRegistry;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use string_cache::DefaultAtom as Atom;

#[derive(Debug)]
pub struct ModuleResolver {
    registry: Arc<ModuleRegistry>,
    deps: Option<Vec<Dependency>>,
}

impl ModuleResolver {
    pub fn new(registry: Arc<ModuleRegistry>) -> Self {
        Self {
            registry,
            deps: None,
        }
    }

    pub fn run(&mut self, module: &mut Module, path: &Path) {
        self.deps = Some(vec![]);

        for stmt in module.stmts.iter_mut() {
            self.statement(stmt, path);
        }

        module.dependencies = self.deps.take().unwrap();
    }

    fn statement(&mut self, stmt: &mut Statement, fs_path: &Path) {
        match stmt {
            Statement::Import(import) => {
                let Import {
                    module_id, path, ..
                } = import.as_mut();

                let id = self.registry.register();
                *module_id = Some(id);
                let mut relative_path = PathBuf::new();

                for part in path.iter() {
                    relative_path.push(part.as_ref());
                }

                self.deps.as_mut().unwrap().push(Dependency {
                    module_id: id,
                    relative_path: path.to_vec(),
                });

                if !fs_path.join(&relative_path).with_extension("wr").exists() {
                    println!("rel path: {:?}", fs_path.join(&relative_path));
                    path.push(Atom::from("mod"));
                }
            }
            _ => (),
        }
    }
}
