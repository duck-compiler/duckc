//! The backend is supposed to go from ast structures to go code

use crate::{ast::statement::Stmt, backend::gost::go_tree::{GoStatement, GostRoot}, backend::semantics::{context::SemanticsContext, module::ModuleId}};

mod emit;
mod translate;

pub mod go_tree;
pub use emit::emit_gost;

#[cfg(test)]
mod gost_test;

pub fn translate<'src>(context: &SemanticsContext<'src>, module: ModuleId) -> GostRoot<'src> {
    let translator = translate::GostTranslator::new(context, module);

    let imports = collect_go_imports(context, module)
        .into_iter()
        .map(|(alias, path)| GoStatement::GoImport { alias, path });

    let statements = context.modules[module.0 as usize]
        .ast
        .statements
        .iter()
        .filter(|stmt| !matches!(stmt.variant, Stmt::Use(_)))
        .flat_map(|stmt| translator.translate_statement(stmt));

    GostRoot {
        body: imports.chain(statements).collect::<Vec<_>>()
    }
}

fn collect_go_imports<'src>(
    context: &SemanticsContext<'src>,
    module: ModuleId
) -> Vec<(Option<&'src str>, &'src str)> {
    context.modules[module.0 as usize]
        .ast
        .statements
        .iter()
        .filter_map(|stmt| match &stmt.variant {
            Stmt::Use(use_statement) => Some((
                use_statement.alias.as_ref().map(|alias| alias.ident),
                use_statement.path.ident
            )),
            _ => None,
        })
        .collect()
}
