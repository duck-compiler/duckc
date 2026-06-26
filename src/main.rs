use crate::backend::{
    gost,
    semantics::{
        self,
        context::SemanticsContext,
        diagnostic::DiagnosticKind,
        module::{ModuleId, ModuleTables},
    },
};

mod ast;
mod backend;
mod frontend;
mod mimic;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    if args.len() == 1 {
        println!("Usage: duckc <filename>");
        return;
    }

    let file_name = &args[0];
    let _src = match std::fs::read_to_string(file_name) {
        Ok(src) => src,
        Err(e) => {
            println!("Error reading {file_name}: {e:?}");
            return;
        }
    };

    if std::env::args().any(|f| f == "--lex") {
        use std::io::{self, Read};
        let src = &_src;
        let mut s = frontend::lexer::LexState::init(file_name, src);
    
        loop {
            dbg!(s.lex_single());
            let mut buf = [0; 8192];
            io::stdin().read(&mut buf).unwrap();
        }
        
        std::process::exit(0);
    } 
    
    let mut ast = mimic::hello_world_program();

    let count = ast::assign_generate_node_ids(&mut ast);

    let mut context = SemanticsContext::new();
    let module = ModuleId(0);

    let root = context.new_scope(None);
    context.modules.push(ModuleTables::new(ast, count, root));

    semantics::passes::collect_module(module, &mut context);
    semantics::passes::resolve_module(module, &mut context);

    if !context.diagnostics.is_empty() {
        for diagnostic in &context.diagnostics {
            println!("{:?}", diagnostic);
            if let DiagnosticKind::Error = diagnostic.kind {
                return;
            }
        }
    }

    let gost = gost::translate(&context, module);
    let go_src = gost::emit_gost(gost);

    println!("{go_src}");
}
