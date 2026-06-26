//! this compiler pass let's every named reference point to its declaration

use crate::{ast::{AstRoot, Block, Expression, MemoryTarget, NodeId, Statement, expression::Expr, memory_target::{self, MemTar}, statement::Stmt}, type_resolve::type_env::{ModuleId, Origin, ScopeId, SymbolData, SymbolId, SymbolKind, TypeEnv}};

pub fn resolve_scopes<'src>(
    module: ModuleId,
    type_env: &mut TypeEnv<'src>
) {
    let root_scope = type_env.modules[module.0 as usize].root_scope;

    let ast = std::mem::replace(
        &mut type_env.modules[module.0 as usize].ast,
        AstRoot { statements: Vec::new() }
    );

    let mut scope_resolver = ScopeResolver {
        type_env,
        module,
        scope: root_scope
    };

    for statement in &ast.statements {
        scope_resolver.resolve_statement(statement);
    }

    type_env.modules[module.0 as usize].ast = ast;
}

struct ScopeResolver<'a, 'src> {
    type_env: &'a mut TypeEnv<'src>,
    module: ModuleId,
    scope: ScopeId
}

impl<'a, 'src> ScopeResolver<'a, 'src> {
    fn set_resolved(&mut self, node: NodeId, sym: SymbolId) {
        self.type_env.modules[self.module.0 as usize].resolutions[node.0 as usize] = Some(sym);
    }

    fn set_definition(&mut self, node: NodeId, sym: SymbolId) {
        self.type_env.modules[self.module.0 as usize].definitions[node.0 as usize] = Some(sym);
    }

    fn declare(&mut self, name: &'src str, kind: SymbolKind, declaration: NodeId) -> SymbolId {
        let sym = self.type_env.add_symbol(SymbolData {
            name,
            kind,
            type_: None,
            origin: Origin::Duck {
                module: self.module,
                declaration: declaration
            }
        });

        self.type_env.define(self.scope, name, sym);
        self.set_definition(declaration, sym);

        sym
    }

    fn resolve_statement(&mut self, statement: &Statement<'src>) {
        match &statement.variant {
            Stmt::FunctionDefinition { name: _, params, body, return_type: _ } => {
                let fn_scope = self.type_env.new_scope(Some(self.scope));
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
            Stmt::VariableDeclaration { name, type_, init_expression } => {
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
                if let Some(sym) = self.type_env.lookup(self.scope, name.ident) {
                    self.set_resolved(expr.id, sym);
                } else {
                    todo!("diagnostic")
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
                if let Some(sym) = self.type_env.lookup(self.scope, identifier.ident) {
                    self.set_resolved(identifier.id, sym);
                } else {
                    todo!("diagnostic")
                }
            },
            MemTar::Dereference(expr) => {
                self.resolve_expression(&expr);
            }
            MemTar::ArrayAccess { target, index_expression } => {
                self.resolve_memory_target(memory_target);
            }
            MemTar::FieldAccess { target, field_name } => {
                self.resolve_memory_target(target);
            }
        }
    }
}
