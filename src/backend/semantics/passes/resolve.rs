//! this compiler pass let's every named reference point to its declaration

use crate::{ast::{AstRoot, Block, Expression, MemoryTarget, NodeId, Statement, expression::Expr, memory_target::MemTar, statement::Stmt, type_expression::{TypeAnnotation, TypeExpression}, use_statement::UseStatement}, backend::semantics::{context::SemanticsContext, diagnostic::Diagnostic, module::ModuleId, symbol::{Origin, ScopeId, SymbolData, SymbolId, SymbolKind}}};

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
        self.declare_with_origin(
            name,
            kind,
            declaration,
            Origin::Duck {
                module: self.module,
                declaration: declaration
            }
        )
    }

    fn declare_with_origin(
        &mut self,
        name: &'src str,
        kind: SymbolKind,
        declaration: NodeId,
        origin: Origin<'src>
    ) -> SymbolId {
        let symbol = self.context.add_symbol(SymbolData {
            name,
            kind,
            type_: None,
            origin
        });

        self.context.define(self.scope, name, symbol);
        self.set_definition(declaration, symbol);

        symbol
    }

    fn resolve_statement(&mut self, statement: &Statement<'src>) {
        match &statement.variant {
            Stmt::FunctionDefinition { name: _, params, body, return_type } => {
                for param in &params.list {
                    self.resolve_type_annotation(&param.type_);
                }

                self.resolve_type_annotation(return_type);

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
            Stmt::StructDefinition { name: _, fields } => {
                for field in &fields.list {
                    self.resolve_type_annotation(&field.type_);
                }
            }
            Stmt::VariableDeclaration { name, type_, init_expression } => {
                self.resolve_type_annotation(type_);
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
            Stmt::Use(use_statement) => {
                self.resolve_use_statement(use_statement);
            }
        }
    }

    fn resolve_type_annotation(&mut self, annotation: &TypeAnnotation<'src>) {
        if let Some(type_expr) = &annotation.annotation {
            self.resolve_type_expr(type_expr);
        }
    }

    fn resolve_type_expr(&mut self, type_expr: &TypeExpression<'src>) {
        match type_expr {
            TypeExpression::Ident(identifier) => {
                if let Some(sym) = self.context.lookup(self.scope, identifier.ident) {
                    self.set_resolved(identifier.id, sym);
                } else {
                    self.context.report(Diagnostic::symbol_not_found(
                        SymbolKind::Struct,
                        identifier.ident,
                        identifier.span,
                    ));
                }
            }
            TypeExpression::Array { inner } => {
                self.resolve_type_expr(inner);
            }
            TypeExpression::Int | TypeExpression::Float
            | TypeExpression::Bool | TypeExpression::String => {}
        }
    }

    fn resolve_use_statement(&mut self, use_statement: &UseStatement<'src>) {
        let bound = use_statement.alias.as_ref().unwrap_or(&use_statement.path);

        self.declare_with_origin(
            bound.ident,
            SymbolKind::Module,
            bound.id,
            Origin::GoPackage {
                path: use_statement.path.ident
            },
        );
    }

    fn resolve_expression(&mut self, expr: &Expression<'src>) {
        match &*expr.variant {
            Expr::FunctionCall { target, args } => {
                self.resolve_expression(target);

                for arg in &args.list {
                    self.resolve_expression(arg);
                }
            },
            Expr::ArrayExpression { values_exprs } => {
                for expr in values_exprs {
                    self.resolve_expression(expr);
                }
            }
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
            Expr::If { expr, body, else_branch } => {
                self.resolve_expression(expr);
                self.resolve_block(body);

                if let Some(else_branch) = else_branch {
                    self.resolve_block(else_branch);
                }
            }
            Expr::MemoryTarget(memory_target) => {
                self.resolve_memory_target(memory_target);
            }
            Expr::StructInit { type_name, fields } => {
                if let Some(sym) = self.context.lookup(self.scope, type_name.ident) {
                    self.set_resolved(type_name.id, sym);
                } else {
                    self.context.report(Diagnostic::symbol_not_found(
                        SymbolKind::Struct,
                        type_name.ident,
                        type_name.span,
                    ));
                }

                for field in fields {
                    self.resolve_expression(&field.value);
                }
            }
            Expr::StringLiteral(..) | Expr::IntLiteral(..)
            | Expr::BoolLiteral(..) | Expr::FloatLiteral(..) => {}
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
