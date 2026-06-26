use crate::{ast::{AstRoot, statement::Stmt}, type_resolve::type_env::{ModuleId, Origin, ScopeId, SymbolData, SymbolKind, TypeEnv}};

pub fn collect_module<'src>(
    module: ModuleId,
    type_env: &mut TypeEnv<'src>,
) {
    let root = type_env.modules[module.0 as usize].root_scope;

    let ast = std::mem::replace(
        &mut type_env.modules[module.0 as usize].ast,
        AstRoot { statements: Vec::new() },
    );

    for stmt in &ast.statements {
        if let Stmt::FunctionDefinition { name, .. } = &stmt.variant {
            let sym = type_env.add_symbol(SymbolData {
                name: name.ident,
                kind: SymbolKind::Function,
                type_: None,
                origin: Origin::Duck {
                    module,
                    declaration: name.id,
                },
            });

            type_env.define(root, name.ident, sym);
            type_env.modules[module.0 as usize].definitions[name.id.0 as usize] = Some(sym);
        }
    }

    type_env.modules[module.0 as usize].ast = ast;
}
