//! The backend is supposed to go from ast structures to go code

use crate::{ast::AstRoot, backend::gost::GostRoot, semantics::{context::SemanticsContext, module::ModuleId}};

pub mod gost_emit;
pub use gost_emit::emit_gost;
pub mod gost;
mod gost_translate;

pub fn translate<'src>(context: &SemanticsContext<'src>, module: ModuleId) -> GostRoot<'src> {
    let translator = gost_translate::Translator { context, module };
    let go_syntax_tree_root = GostRoot {
        body: context.modules[module.0 as usize].ast.statements
            .iter()
            .map(|stmt| translator.translate_statement(stmt))
            .collect::<Vec<_>>()
    };

    return go_syntax_tree_root;
}
