//! The backend is supposed to go from ast structures to go code

use crate::{backend::gost::go_tree::GostRoot, backend::semantics::{context::SemanticsContext, module::ModuleId}};

mod emit;
mod translate;

pub mod go_tree;
pub use emit::emit_gost;

pub fn translate<'src>(context: &SemanticsContext<'src>, module: ModuleId) -> GostRoot<'src> {
    let translator = translate::Translator { context, module };
    let go_syntax_tree_root = GostRoot {
        body: context.modules[module.0 as usize].ast.statements
            .iter()
            .map(|stmt| translator.translate_statement(stmt))
            .collect::<Vec<_>>()
    };

    return go_syntax_tree_root;
}
