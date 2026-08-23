use crate::{ast::{AstRoot, Identifier, Stmt}, backend::semantics::{context::SemanticsContext, diagnostic::Diagnostic, module::ModuleId, symbol::{Origin, ScopeId, SymbolData, SymbolKind}}};

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
        match &stmt.variant {
            Stmt::FunctionDefinition { name, .. } => {
                declare_top_level(context, root, module, name, SymbolKind::Function);
            }
            Stmt::StructDefinition { name, .. } => {
                declare_top_level(context, root, module, name, SymbolKind::Struct);
            }
            Stmt::Use(_) => {}
            Stmt::VariableDeclaration { .. }
            | Stmt::VariableAssignment { .. }
            | Stmt::Expression { .. }
            | Stmt::Return { .. }
            | Stmt::Break
            | Stmt::Continue => {
                context.report(Diagnostic::not_allowed_at_top_level(stmt.span));
            }
        }
    }

    context.modules[module.0 as usize].ast = ast;
}

fn declare_top_level<'src>(
    context: &mut SemanticsContext<'src>,
    root: ScopeId,
    module: ModuleId,
    name: &Identifier<'src>,
    kind: SymbolKind,
) {
    let symbol = context.add_symbol(SymbolData {
        name: name.ident,
        kind,
        type_: None,
        origin: Origin::Duck {
            module,
            declaration: name.id,
        },
    });

    if context.lookup(root, name.ident).is_some() {
        context.report(Diagnostic::already_defined(name.ident, name.span));
    } else {
        context.define(root, name.ident, symbol);
    }

    context.modules[module.0 as usize].definitions[name.id.0 as usize] = Some(symbol);
}
