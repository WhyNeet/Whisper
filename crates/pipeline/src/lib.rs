use checker::Checker;
use js::{
    codegen::Codegen as JsCodegen, transformer::TypedAstTransformer as JsTypedAstTransformer,
};
use lexer::stream::TokenStream;
use parser::Parser;
use tcast::module::Module;

pub struct CompilationPipeline {}

impl CompilationPipeline {
    pub fn compile_module(contents: String) -> String {
        let token_stream = TokenStream::new(&contents);
        let module = Parser::new(token_stream).run();

        let module = Checker::new().run(module);

        let codegen = JsTypedAstTransformer::default();
        let program = codegen.run(module);

        JsCodegen::default().run(program)
    }

    pub fn parse_module(contents: String) -> Module {
        let token_stream = TokenStream::new(&contents);
        let module = Parser::new(token_stream).run();

        Checker::new().run(module)
    }
}
