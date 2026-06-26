//! this compiler pass let's every named reference point to its declaration

use crate::{ast::{AstRoot, Block, Expression, MemoryTarget, NodeId, Statement, expression::Expr, memory_target::MemTar, statement::Stmt}, backend::semantics::{context::SemanticsContext, diagnostic::Diagnostic, module::ModuleId, symbol::{Origin, ScopeId, SymbolData, SymbolId, SymbolKind}}};

pub fn resolve_module<'src>(
    module: ModuleId,
    context: &mut SemanticsContext<'src>
) {
    let root_scope = context.modules[module.0 as usize].root_scope;

    let ast = std::mem::replace(
        &mut context.modules[module.0 as usize].ast,
        AstRoot { statements: Vec::new() }
    );

    let mut scope_resolver = ScopeResolver {
        context,
        module,
        scope: root_scope
    };

    for statement in &ast.statements {
        scope_resolver.resolve_statement(statement);
    }

    context.modules[module.0 as usize].ast = ast;
}

struct ScopeResolver<'a, 'src> {
    context: &'a mut SemanticsContext<'src>,
    module: ModuleId,
    scope: ScopeId
}

impl<'a, 'src> ScopeResolver<'a, 'src> {
    fn set_resolved(&mut self, node: NodeId, sym: SymbolId) {
        self.context.modules[self.module.0 as usize].resolutions[node.0 as usize] = Some(sym);
    }

    fn set_definition(&mut self, node: NodeId, sym: SymbolId) {
        self.context.modules[self.module.0 as usize].definitions[node.0 as usize] = Some(sym);
    }

    fn declare(&mut self, name: &'src str, kind: SymbolKind, declaration: NodeId) -> SymbolId {
        let sym = self.context.add_symbol(SymbolData {
            name,
            kind,
            type_: None,
            origin: Origin::Duck {
                module: self.module,
                declaration: declaration
            }
        });

        self.context.define(self.scope, name, sym);
        self.set_definition(declaration, sym);

        sym
    }

    fn resolve_statement(&mut self, statement: &Statement<'src>) {
        match &statement.variant {
            Stmt::FunctionDefinition { name: _, params, body, return_type: _ } => {
                let fn_scope = self.context.new_scope(Some(self.scope));
                let prev = self.scope;

                self.scope = fn_scope;

                for param in &params.list {
                    self.declare(
                        param.name.ident,
                        SymbolKind::Param,
                        param.name.id
                    );
                }

                self.resolve_block(&body);
                self.scope = prev;
            }
            Stmt::VariableDeclaration { name, type_: _, init_expression } => {
                if let Some(init_expression) = init_expression {
                    self.resolve_expression(init_expression);
                }
                self.declare(name.ident, SymbolKind::Variable, name.id);
            },
            Stmt::VariableAssignment { target, assign_expression } => {
                self.resolve_memory_target(target);
                self.resolve_expression(assign_expression);
            },
            Stmt::Expression { expr } => {
                self.resolve_expression(expr);
            }
            Stmt::Use(_) => {}
        }
    }

    fn resolve_expression(&mut self, expr: &Expression<'src>) {
        match &*expr.variant {
            Expr::FunctionCall { name, args } => {
                if let Some(sym) = self.context.lookup(self.scope, name.ident) {
                    self.set_resolved(expr.id, sym);
                } else {
                    self.context.report(
                        Diagnostic::symbol_not_found(
                            SymbolKind::Function,
                            name.ident,
                            name.span
                        )
                    );
                }

                for arg in &args.list {
                    self.resolve_expression(arg);
                }
            },
            Expr::Binary { left, op: _, right } => {
                self.resolve_expression(left);
                self.resolve_expression(right);
            }
            Expr::Unary { op: _, expr } => {
                self.resolve_expression(expr);
            }
            Expr::While { expr, body } => {
                self.resolve_expression(expr);
                self.resolve_block(body);
            }
            Expr::If { expr, body } => {
                self.resolve_expression(expr);
                self.resolve_block(body);
            }
            Expr::MemoryTarget(memory_target) => {
                self.resolve_memory_target(memory_target);
            }
            Expr::StringLiteral(..) | Expr::IntLiteral(..)
            | Expr::BoolLiteral(..) | Expr::FloatLiteral(..)
            | Expr::GoImmediateSource { .. } => {}
        }
    }

    fn resolve_block(&mut self, block: &Block<'src>) {
        for stmt in &block.statements {
            self.resolve_statement(stmt);
        }
    }

    fn resolve_memory_target(&mut self, memory_target: &MemoryTarget<'src>) {
        match &memory_target.variant {
            MemTar::Name(identifier) => {
                if let Some(sym) = self.context.lookup(self.scope, identifier.ident) {
                    self.set_resolved(identifier.id, sym);
                } else {
                    self.context.report(Diagnostic::symbol_not_found(
                        SymbolKind::Variable,
                        identifier.ident,
                        memory_target.span,
                    ));
                }
            },
            MemTar::Dereference(expr) => {
                self.resolve_expression(&expr);
            }
            MemTar::ArrayAccess { target, index_expression } => {
                self.resolve_memory_target(target);
                self.resolve_expression(index_expression);
            }
            MemTar::FieldAccess { target, field_name: _ } => {
                self.resolve_memory_target(target);
            }
        }
    }
}
