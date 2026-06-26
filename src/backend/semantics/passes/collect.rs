use crate::{ast::{AstRoot, Stmt}, backend::semantics::{context::SemanticsContext, module::ModuleId, symbol::{Origin, SymbolData, SymbolKind}}};

pub fn collect_module<'src>(
    module: ModuleId,
    context: &mut SemanticsContext<'src>,
) {
    let root = context.modules[module.0 as usize].root_scope;

    let ast = std::mem::replace(
        &mut context.modules[module.0 as usize].ast,
        AstRoot { statements: Vec::new() },
    );

    for stmt in &ast.statements {
        if let Stmt::FunctionDefinition { name, .. } = &stmt.variant {
            let sym = context.add_symbol(SymbolData {
                name: name.ident,
                kind: SymbolKind::Function,
                type_: None,
                origin: Origin::Duck {
                    module,
                    declaration: name.id,
                },
            });

            context.define(root, name.ident, sym);
            context.modules[module.0 as usize].definitions[name.id.0 as usize] = Some(sym);
        }
    }

    context.modules[module.0 as usize].ast = ast;
}
