use crate::backend::{gost, semantics::{self, context::SemanticsContext, diagnostic::DiagnosticKind}};

mod ast;
mod backend;
mod frontend;
mod mimic;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    if args.len() != 1 {
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

    let mut context = SemanticsContext::new();

    let module = context.add_module(mimic::test_struct_program());
    semantics::analyze_module(&mut context, module);

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
