//! compiler driver is the entrypoint for the whole duck compiler pipeline

mod home;
mod toolchain;

pub use toolchain::{GO_VERSION, ensure_go_toolchain, resolve_go_binary};

use bumpalo::Bump;

use crate::{
    ast::AstRoot,
    backend::{
        gost,
        semantics::{self, context::SemanticsContext, diagnostic::DiagnosticKind},
    },
    mimic,
};

pub struct CompileOutput {
    pub go_source: String,
}

pub enum CompileError {
    Io(String),
    Diagnostics(Vec<String>),
}

pub fn run(file_path: &str) -> Result<CompileOutput, CompileError> {
    let arena = Bump::new();
    let source = std::fs::read_to_string(file_path)
        .map_err(|err| CompileError::Io(format!("error reading {file_path}: {err}")))?;

    if let Err(err) = ensure_go_toolchain() {
        eprintln!("warning: {err}");
    }

    let ast = parse(&source);
    compile(ast, &arena)
}

fn parse<'src>(source: &'src str) -> AstRoot<'src> {
    let _ = source;
    mimic::test_struct_program()
}

fn compile<'src>(ast: AstRoot<'src>, arena: &'src Bump) -> Result<CompileOutput, CompileError> {
    let mut context = SemanticsContext::new(arena);
    let module = context.add_module(ast);

    semantics::analyze_module(&mut context, module);

    if !context.diagnostics.is_empty() {
        let has_error = context.diagnostics.iter().any(|d| matches!(d.kind, DiagnosticKind::Error));
        let messages = context.diagnostics.iter().map(|d| format!("{d:?}")).collect::<Vec<_>>();

        if has_error {
            return Err(CompileError::Diagnostics(messages));
        }

        for message in &messages {
            eprintln!("{message}");
        }
    }

    let gost_root = gost::translate(&context, module);
    let go_source = gost::emit_gost(gost_root);

    Ok(CompileOutput { go_source })
}
