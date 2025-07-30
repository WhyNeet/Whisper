use std::sync::Arc;

use ast::{
    module::{Dependency, Module},
    stmt::{Import, Statement},
};
use common::module::ModuleRegistry;

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

    pub fn run(&mut self, module: &mut Module) {
        self.deps = Some(vec![]);

        for stmt in module.stmts.iter_mut() {
            self.statement(stmt);
        }

        module.dependencies = self.deps.take().unwrap();
    }

    fn statement(&mut self, stmt: &mut Statement) {
        match stmt {
            Statement::Import(import) => {
                let Import { path, module_id } = import.as_mut();

                let id = self.registry.register();
                *module_id = Some(id);
                self.deps.as_mut().unwrap().push(Dependency {
                    module_id: id,
                    path: path.clone(),
                })
            }
            _ => (),
        }
    }
}
