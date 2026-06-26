//! The backend is supposed to go from ast structures to go code

use crate::{ast::AstRoot, backend::gost::GostRoot};

pub mod gost_emit;
pub use gost_emit::emit_gost;
pub mod gost;

mod gost_translate;

pub fn translate<'src>(program: AstRoot<'src>) -> GostRoot<'src> {
    let go_syntax_tree_root = GostRoot {
        body: program.statements
            .into_iter()
            .map(|s| gost_translate::translate_statement(s))
            .collect::<Vec<_>>()
    };

    return go_syntax_tree_root;
}
