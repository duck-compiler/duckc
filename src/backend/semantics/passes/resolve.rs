//! this compiler pass let's every named reference point to its declaration

use crate::{ast::{AstRoot, Block, Expression, MemoryTarget, NodeId, ParameterList, Statement, expression::Expr, memory_target::MemTar, statement::Stmt, struct_definition::{Method, MethodKind, SELF_NAME}, type_expression::{TypeAnnotation, TypeExpression, TypeParam}, use_statement::UseStatement}, backend::semantics::{context::SemanticsContext, diagnostic::Diagnostic, go_map::short_go_package_name, module::ModuleId, symbol::{Origin, ScopeId, SymbolData, SymbolId, SymbolKind}, r#type::{Type, TypeParamId}}};

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
        scope: root_scope,
        in_function: false,
    };

    for statement in &ast.statements {
        scope_resolver.resolve_statement(statement);
    }

    context.modules[module.0 as usize].ast = ast;
}

struct ScopeResolver<'a, 'src> {
    context: &'a mut SemanticsContext<'src>,
    module: ModuleId,
    scope: ScopeId,
    in_function: bool,
}

impl<'a, 'src> ScopeResolver<'a, 'src> {
    fn set_resolved(&mut self, node: NodeId, sym: SymbolId) {
        self.context.modules[self.module.0 as usize].resolutions[node.0 as usize] = Some(sym);
    }

    fn definition_of(&self, node: NodeId) -> Option<SymbolId> {
        self.context.modules[self.module.0 as usize].definitions[node.0 as usize]
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
            Stmt::FunctionDefinition { name, type_params, params, body, return_type } => {
                if self.in_function {
                    self.context.report(Diagnostic::nested_declaration_not_allowed(statement.span));
                }

                let (previous_scope, declared) = self.enter_type_param_scope(type_params);
                self.store_symbol_type_params(name.id, declared);

                for param in &params.list {
                    self.resolve_type_annotation(&param.type_);
                }

                self.resolve_type_annotation(return_type);
                self.resolve_body_in_new_scope(params, body, None);

                self.scope = previous_scope;
            }
            Stmt::StructDefinition { name, type_params, fields, impl_block } => {
                if self.in_function {
                    self.context.report(Diagnostic::nested_declaration_not_allowed(statement.span));
                }

                let (previous_scope, declared) = self.enter_type_param_scope(type_params);
                self.store_symbol_type_params(name.id, declared);

                for field in fields {
                    self.resolve_type_annotation(&field.type_);
                }

                for method in impl_block.iter().flat_map(|impl_block| &impl_block.methods) {
                    self.resolve_method(method);
                }

                self.scope = previous_scope;
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
            Stmt::Return { value } => {
                if let Some(value) = value {
                    self.resolve_expression(value);
                }
            }
            Stmt::Break | Stmt::Continue => {}
        }
    }

    fn resolve_method(&mut self, method: &Method<'src>) {
        let (previous_scope, declared) = self.enter_type_param_scope(&method.type_params);
        self.context.modules[self.module.0 as usize].method_type_params.insert(method.name.id, declared);

        for param in &method.params.list {
            self.resolve_type_annotation(&param.type_);
        }

        self.resolve_type_annotation(&method.return_type);

        let instance_method = match method.kind {
            MethodKind::Instance => Some(method.name.id),
            MethodKind::Static => None,
        };

        self.resolve_body_in_new_scope(&method.params, &method.body, instance_method);

        self.scope = previous_scope;
    }

    fn enter_type_param_scope(
        &mut self,
        type_params: &[TypeParam<'src>],
    ) -> (ScopeId, Vec<TypeParamId>) {
        let previous_scope = self.scope;

        if type_params.is_empty() {
            return (previous_scope, Vec::new());
        }

        self.scope = self.context.new_scope(Some(previous_scope));

        let declared = type_params
            .iter()
            .map(|type_param| {
                let name = type_param.name.ident;

                if self.context.lookup(self.scope, name).is_some() {
                    self.context.report(Diagnostic::already_defined(name, type_param.name.span));
                    return self.context.add_type_param(name);
                }

                self.declare_type_param(name, type_param.name.id)
            })
            .collect();

        (previous_scope, declared)
    }

    fn store_symbol_type_params(&mut self, declaration: NodeId, declared: Vec<TypeParamId>) {
        if let Some(owner) = self.definition_of(declaration) {
            self.context.symbol_type_params.insert(owner, declared);
        }
    }

    fn declare_type_param(&mut self, name: &'src str, declaration: NodeId) -> TypeParamId {
        let type_param = self.context.add_type_param(name);
        let type_ = self.context.intern(Type::TypeParam(type_param));

        let symbol = self.declare(name, SymbolKind::TypeParam, declaration);
        self.context.symbols[symbol.0 as usize].type_ = Some(type_);

        type_param
    }

    fn declare_self(&mut self, method_name: NodeId) {
        let symbol = self.context.add_symbol(SymbolData {
            name: SELF_NAME,
            kind: SymbolKind::Param,
            type_: None,
            origin: Origin::Duck {
                module: self.module,
                declaration: method_name,
            },
        });

        self.context.define(self.scope, SELF_NAME, symbol);
        self.context.modules[self.module.0 as usize].self_symbols.insert(method_name, symbol);
    }

    fn resolve_body_in_new_scope(
        &mut self,
        params: &ParameterList<'src>,
        body: &Block<'src>,
        instance_method: Option<NodeId>,
    ) {
        let body_scope = self.context.new_scope(Some(self.scope));
        let previous_scope = self.scope;
        let was_in_function = self.in_function;

        self.scope = body_scope;
        self.in_function = true;

        if let Some(method_name) = instance_method {
            self.declare_self(method_name);
        }

        for param in &params.list {
            if self.context.scopes[body_scope.0 as usize].names.contains_key(param.name.ident) {
                self.context.report(Diagnostic::already_defined(param.name.ident, param.name.span));
            }

            self.declare(param.name.ident, SymbolKind::Param, param.name.id);
        }

        self.resolve_block(body);

        self.scope = previous_scope;
        self.in_function = was_in_function;
    }

    fn resolve_type_annotation(&mut self, annotation: &TypeAnnotation<'src>) {
        if let Some(type_expr) = &annotation.annotation {
            self.resolve_type_expr(type_expr);
        }
    }

    fn resolve_type_expr(&mut self, type_expr: &TypeExpression<'src>) {
        match type_expr {
            TypeExpression::Ident { name, type_args } => {
                if let Some(sym) = self.context.lookup(self.scope, name.ident) {
                    self.set_resolved(name.id, sym);
                } else {
                    self.context.report(Diagnostic::symbol_not_found(
                        SymbolKind::Struct,
                        name.ident,
                        name.span,
                    ));
                }

                self.resolve_type_args(type_args);
            }
            TypeExpression::Array { inner } | TypeExpression::Pointer { inner } => {
                self.resolve_type_expr(inner);
            }
            TypeExpression::Tuple(elements) => {
                for element in elements {
                    self.resolve_type_expr(element);
                }
            }
            TypeExpression::Int | TypeExpression::Int8
            | TypeExpression::Int16 | TypeExpression::Int32
            | TypeExpression::Int64 | TypeExpression::Uint
            | TypeExpression::Uint8 | TypeExpression::Uint16
            | TypeExpression::Uint32 | TypeExpression::Uint64
            | TypeExpression::Float | TypeExpression::Float32
            | TypeExpression::Bool | TypeExpression::String => {}
        }
    }

    fn resolve_type_args(&mut self, type_args: &[TypeExpression<'src>]) {
        for type_arg in type_args {
            self.resolve_type_expr(type_arg);
        }
    }

    fn resolve_use_statement(&mut self, use_statement: &UseStatement<'src>) {
        let (name, declaration) = match &use_statement.alias {
            Some(alias) => (alias.ident, alias.id),
            None => (short_go_package_name(use_statement.path.ident), use_statement.path.id),
        };

        self.context.go_package_names.insert(use_statement.path.ident.to_string(), name);

        self.declare_with_origin(
            name,
            SymbolKind::Module,
            declaration,
            Origin::GoPackage {
                path: use_statement.path.ident
            },
        );
    }

    fn resolve_expression(&mut self, expr: &Expression<'src>) {
        match &*expr.variant {
            Expr::FunctionCall { target, type_args, args } => {
                self.resolve_expression(target);
                self.resolve_type_args(type_args);

                for arg in &args.list {
                    self.resolve_expression(arg);
                }
            },
            Expr::ArrayExpression { values_exprs } => {
                for expr in values_exprs {
                    self.resolve_expression(expr);
                }
            }
            Expr::TupleExpression { values } => {
                for expr in values {
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
            Expr::Reference { expr } => {
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
            Expr::StructInit { type_name, type_args, fields } => {
                if let Some(sym) = self.context.lookup(self.scope, type_name.ident) {
                    self.set_resolved(type_name.id, sym);
                } else {
                    self.context.report(Diagnostic::symbol_not_found(
                        SymbolKind::Struct,
                        type_name.ident,
                        type_name.span,
                    ));
                }

                self.resolve_type_args(type_args);

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
            MemTar::FieldAccess { target, field_name: _, type_args } => {
                self.resolve_memory_target(target);
                self.resolve_type_args(type_args);
            }
            MemTar::TupleIndex { target, index: _ } => {
                self.resolve_memory_target(target);
            }
        }
    }
}
