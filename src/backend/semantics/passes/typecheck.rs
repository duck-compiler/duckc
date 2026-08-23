//! this compiler pass fills node_types and symbol types, consuming resolutions/definitions.
//! it walks no scopes. every reference/declaration lookup goes through the tables.

use crate::{ast::{AstRoot, Block, Expression, Identifier, MemoryTarget, NodeId, Span, Statement, expression::{Expr, ExpressionList, FieldInit}, memory_target::MemTar, statement::Stmt, type_expression::{self, TypeAnnotation, TypeExpression}}, backend::semantics::{context::SemanticsContext, diagnostic::Diagnostic, go_map, module::ModuleId, symbol::{Origin, SymbolId}, r#type::{Type, TypeId}}};

pub fn typecheck_module<'src>(
    module: ModuleId,
    context: &mut SemanticsContext<'src>,
) {
    let ast = std::mem::replace(
        &mut context.modules[module.0 as usize].ast,
        AstRoot { statements: Vec::new() },
    );

    let mut checker = TypeChecker { context, module };

    for statement in &ast.statements {
        checker.declare_signature(statement);
    }

    for statement in &ast.statements {
        checker.check_statement(statement);
    }

    context.modules[module.0 as usize].ast = ast;
}

struct TypeChecker<'a, 'src> {
    context: &'a mut SemanticsContext<'src>,
    module: ModuleId,
}

impl<'a, 'src> TypeChecker<'a, 'src> {
    fn set_node_type(&mut self, node: NodeId, type_id: TypeId) {
        self.context.modules[self.module.0 as usize].node_types[node.0 as usize] = Some(type_id);
    }

    fn resolution_of(&self, node: NodeId) -> Option<SymbolId> {
        self.context.modules[self.module.0 as usize].resolutions[node.0 as usize]
    }

    fn definition_of(&self, node: NodeId) -> Option<SymbolId> {
        self.context.modules[self.module.0 as usize].definitions[node.0 as usize]
    }

    fn symbol_type(&self, sym: SymbolId) -> Option<TypeId> {
        self.context.symbols[sym.0 as usize].type_
    }

    fn set_symbol_type(&mut self, sym: SymbolId, type_id: TypeId) {
        self.context.symbols[sym.0 as usize].type_ = Some(type_id);
    }

    fn type_id_from_type_annotation(&mut self, annotation: &TypeAnnotation) -> TypeId {
        match &annotation.annotation {
            None => self.context.intern(Type::Unit),
            Some(type_expr) => self.type_id_from_type_expr(type_expr),
        }
    }

    fn type_id_from_type_expr(&mut self, type_expr: &TypeExpression) -> TypeId {
        let type_ = match type_expr {
            TypeExpression::Int => Type::Int,
            TypeExpression::Float => Type::Float,
            TypeExpression::Bool => Type::Bool,
            TypeExpression::String => Type::String,
            TypeExpression::Array { inner } => {
                let inner_id = self.type_id_from_type_expr(inner);
                Type::Array(inner_id)
            }
            TypeExpression::Ident(identifier) => {
                match self.resolution_of(identifier.id) {
                    Some(sym) => Type::Struct(sym),
                    None => Type::TypeError,
                }
            }
        };
        self.context.intern(type_)
    }

    fn declare_signature(&mut self, statement: &Statement<'src>) {
        match &statement.variant {
            Stmt::FunctionDefinition { name, params, return_type, .. } => {
                let param_types = params
                    .list
                    .iter()
                    .map(|param| self.type_id_from_type_annotation(&param.type_))
                    .collect::<Vec<_>>();

                let return_type_id = self.type_id_from_type_annotation(return_type);

                let fn_type = self.context.intern(Type::Fn {
                    params: param_types,
                    return_type: return_type_id
                });

                if let Some(symbol) = self.definition_of(name.id) {
                    self.set_symbol_type(symbol, fn_type);
                }
            }
            Stmt::StructDefinition { name, fields } => {
                let field_types = fields.list
                    .iter()
                    .map(|field| (field.name.ident, self.type_id_from_type_annotation(&field.type_)))
                    .collect::<Vec<_>>();

                if let Some(symbol) = self.definition_of(name.id) {
                    self.context.struct_fields.insert(symbol, field_types);
                }
            }
            _ => {}
        }
    }

    fn check_statement(&mut self, statement: &Statement<'src>) {
        match &statement.variant {
            Stmt::FunctionDefinition { params, body, .. } => {
                for param in &params.list {
                    let type_id = self.type_id_from_type_annotation(&param.type_);
                    if let Some(sym) = self.definition_of(param.name.id) {
                        self.set_symbol_type(sym, type_id);
                    }
                }
                self.check_block(body);
            }
            Stmt::VariableDeclaration { name, type_, init_expression } => {
                let declared = type_.annotation.as_ref().map(|_| self.type_id_from_type_annotation(type_));
                let inferred = init_expression.as_ref().map(|expr| self.check_expression(expr));

                let type_id = match (declared, inferred) {
                    (Some(declared), Some(inferred)) => {
                        if !self.compatible(declared, inferred) {
                            self.report_mismatch(declared, inferred, name.span);
                        }

                        declared
                    }
                    (Some(declared), None) => declared,
                    (None, Some(inferred)) => inferred,
                    (None, None) => self.context.intern(Type::TypeError),
                };

                if let Some(sym) = self.definition_of(name.id) {
                    self.set_symbol_type(sym, type_id);
                }
            }
            Stmt::VariableAssignment { target, assign_expression } => {
                let target_type = self.check_memory_target(target);
                let value_type = self.check_expression(assign_expression);
                if !self.compatible(target_type, value_type) {
                    self.report_mismatch(target_type, value_type, assign_expression.span);
                }
            }
            Stmt::Expression { expr } => {
                self.check_expression(expr);
            }
            Stmt::Use(_) => {}
            Stmt::StructDefinition { .. } => {}
        }
    }

    fn check_block(&mut self, block: &Block<'src>) {
        for statement in &block.statements {
            self.check_statement(statement);
        }
    }

    fn check_expression(&mut self, expr: &Expression<'src>) -> TypeId {
        let type_id = match &*expr.variant {
            Expr::StringLiteral(_) => self.context.intern(Type::String),
            Expr::IntLiteral(_) => self.context.intern(Type::Int),
            Expr::FloatLiteral(_) => self.context.intern(Type::Float),
            Expr::BoolLiteral(_) => self.context.intern(Type::Bool),
            Expr::MemoryTarget(memory_target) => self.check_memory_target(memory_target),
            Expr::FunctionCall { target, args } => self.check_function_call(target, args, expr.span),
            Expr::ArrayExpression { values_exprs } => self.check_array_expression(values_exprs, expr.span),
            Expr::StructInit { type_name, fields } => self.check_struct_init(type_name, fields, expr.span),
            Expr::Binary { left, right, .. } => {
                let left_type = self.check_expression(left);
                let right_type = self.check_expression(right);
                if !self.compatible(left_type, right_type) {
                    self.report_mismatch(left_type, right_type, expr.span);
                }
                left_type
            }
            Expr::Unary { expr: inner, .. } => self.check_expression(inner),
            Expr::If { expr: condition, body } => {
                self.check_condition(condition);
                self.check_block(body);
                self.context.intern(Type::Unit)
            }
            Expr::While { expr: condition, body } => {
                self.check_condition(condition);
                self.check_block(body);
                self.context.intern(Type::Unit)
            }
        };
        self.set_node_type(expr.id, type_id);
        type_id
    }

    fn check_condition(&mut self, expr: &Expression<'src>) {
        let condition_type = self.check_expression(expr);
        let bool_type = self.context.intern(Type::Bool);
        if !self.compatible(bool_type, condition_type) {
            self.report_mismatch(bool_type, condition_type, expr.span);
        }
    }

    fn check_memory_target(&mut self, memory_target: &MemoryTarget<'src>) -> TypeId {
        match &memory_target.variant {
            MemTar::Name(identifier) => {
                match self.resolution_of(identifier.id).and_then(|sym| self.symbol_type(sym)) {
                    Some(type_id) => type_id,
                    None => self.context.intern(Type::TypeError),
                }
            }
            MemTar::FieldAccess { target, field_name } => {
                self.check_field_access(target, field_name, memory_target.span)
            }
            MemTar::ArrayAccess { target, index_expression } => {
                self.check_array_access(target, index_expression, memory_target.span)
            }
            _ => self.context.intern(Type::TypeError),
        }
    }

    fn check_array_expression(
        &mut self,
        values_exprs: &Vec<Box<Expression<'src>>>,
        span: Span<'src>,
    ) -> TypeId {
        let Some((first, remaining)) = values_exprs.split_first() else {
            self.context.report(Diagnostic::empty_array_literal(span));
            return self.context.intern(Type::TypeError);
        };

        let elem_type = self.check_expression(first);
        for value in remaining {
            let value_type = self.check_expression(value);
            if !self.compatible(elem_type, value_type) {
                self.report_mismatch(elem_type, value_type, value.span);
            }
        }

        self.context.intern(Type::Array(elem_type))
    }

    fn check_array_access(
        &mut self,
        target: &MemoryTarget<'src>,
        index_expression: &Expression<'src>,
        span: Span<'src>,
    ) -> TypeId {
        let target_type = self.check_memory_target(target);
        let index_type = self.check_expression(index_expression);

        let int_type = self.context.intern(Type::Int);

        if !self.compatible(int_type, index_type) {
            self.report_mismatch(int_type, index_type, index_expression.span);
        }

        match &self.context.types[target_type.0 as usize] {
            Type::Array(inner) => *inner,
            Type::TypeError => target_type,
            _ => {
                self.context.report(Diagnostic::not_indexable(span));
                self.context.intern(Type::TypeError)
            }
        }
    }

    fn check_field_access(
        &mut self,
        target: &MemoryTarget<'src>,
        field_name: &Identifier<'src>,
        span: Span<'src>,
    ) -> TypeId {
        if let MemTar::Name(identifier) = &target.variant {
            let go_package = self.resolution_of(identifier.id).and_then(|sym| {
                match &self.context.symbols[sym.0 as usize].origin {
                    Origin::GoPackage { path } => Some(*path),
                    Origin::Duck { .. } | Origin::GoType { .. } => None,
                }
            });

            if let Some(package) = go_package {
                let raw_func = self.context.go_resolver.lookup(package, field_name.ident).cloned();

                return match raw_func {
                    Ok(raw_func) => {
                        let types = self.context.go_resolver.types_of(package)
                            .expect("package must already be loaded, lookup above just succeeded against it")
                            .clone();

                        match go_map::map_go_signature(self.context, package, &raw_func, &types) {
                            Some(fn_type_id) => fn_type_id,
                            None => {
                                self.context.report(Diagnostic::unknown_package_member(
                                    package,
                                    field_name.ident,
                                    "its signature uses a Go type Duck cannot express",
                                    span,
                                ));
                                self.context.intern(Type::TypeError)
                            }
                        }
                    }
                    Err(reason) => {
                        self.context.report(Diagnostic::unknown_package_member(package, field_name.ident, &reason, span));
                        self.context.intern(Type::TypeError)
                    }
                };
            }
        }

        let target_type = self.check_memory_target(target);
        match self.context.types[target_type.0 as usize].clone() {
            Type::Struct(struct_sym) => {
                let field_type = self.context.struct_fields.get(&struct_sym)
                    .and_then(|fields| fields.iter().find(|(name, _)| *name == field_name.ident))
                    .map(|(_, type_id)| *type_id);

                match field_type {
                    Some(type_id) => type_id,
                    None => {
                        let struct_name = self.context.symbols[struct_sym.0 as usize].name;
                        self.context.report(Diagnostic::unknown_struct_field(struct_name, field_name.ident, span));
                        self.context.intern(Type::TypeError)
                    }
                }
            }
            Type::TypeError => target_type,
            _ => {
                self.context.report(Diagnostic::not_a_struct(span));
                self.context.intern(Type::TypeError)
            }
        }
    }

    fn check_struct_init(
        &mut self,
        type_name: &Identifier<'src>,
        fields: &Vec<FieldInit<'src>>,
        span: Span<'src>,
    ) -> TypeId {
        let Some(struct_symbol) = self.resolution_of(type_name.id) else {
            for field in fields {
                self.check_expression(&field.value);
            }

            return self.context.intern(Type::TypeError);
        };

        let Some(declared_fields) = self.context.struct_fields.get(&struct_symbol).cloned() else {
            for field in fields {
                self.check_expression(&field.value);
            }

            self.context.report(Diagnostic::not_a_struct(span));
            return self.context.intern(Type::TypeError);
        };

        let struct_name = self.context.symbols[struct_symbol.0 as usize].name;

        let mut known = Vec::with_capacity(fields.len());
        for field in fields {
            let value_type = self.check_expression(&field.value);

            let matching_field = declared_fields
                .iter()
                .find(|(name, _)| *name == field.name.ident);

            match matching_field {
                Some((_, declared_type)) => {
                    if !self.compatible(*declared_type, value_type) {
                        self.report_mismatch(*declared_type, value_type, field.value.span);
                    }

                    known.push(field.name.ident);
                }
                None => {
                    self.context.report(Diagnostic::unknown_struct_field(struct_name, field.name.ident, field.name.span));
                }
            }
        }

        let missing = declared_fields
            .iter()
            .map(|(name, _)| *name)
            .filter(|name| !known.contains(name))
            .collect::<Vec<_>>();

        if !missing.is_empty() {
            self.context.report(Diagnostic::missing_struct_fields(struct_name, &missing, span));
        }

        self.context.intern(Type::Struct(struct_symbol))
    }

    fn check_function_call(
        &mut self,
        target: &Expression<'src>,
        args: &ExpressionList<'src>,
        call_span: Span<'src>,
    ) -> TypeId {
        let callee_type = self.check_expression(target);
        let arg_types = args.list
            .iter()
            .map(|arg| self.check_expression(arg))
            .collect::<Vec<_>>();

        match self.context.types[callee_type.0 as usize].clone() {
            Type::Fn { params, return_type } => {
                if params.len() != arg_types.len() {
                    self.context.report(Diagnostic::wrong_arg_count(params.len(), arg_types.len(), call_span));
                } else {
                    for (expected, found) in params.iter().zip(arg_types.iter()) {
                        if !self.compatible(*expected, *found) {
                            self.report_mismatch(*expected, *found, call_span);
                        }
                    }
                }
                return_type
            }
            Type::TypeError => callee_type,
            _ => {
                self.context.report(Diagnostic::not_callable(call_span));
                self.context.intern(Type::TypeError)
            }
        }
    }

    fn compatible(&self, a: TypeId, b: TypeId) -> bool {
        a == b
            || self.context.types[a.0 as usize] == Type::TypeError
            || self.context.types[b.0 as usize] == Type::TypeError
    }

    fn report_mismatch(&mut self, expected: TypeId, found: TypeId, span: Span<'src>) {
        let expected_name = self.type_name(expected);
        let found_name = self.type_name(found);
        self.context.report(Diagnostic::type_mismatch(&expected_name, &found_name, span));
    }

    fn type_name(&self, type_id: TypeId) -> String {
        match &self.context.types[type_id.0 as usize] {
            Type::Unit => "unit".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "string".to_string(),
            Type::Array(inner) => format!("[{}]", self.type_name(*inner)),
            Type::Struct(sym) => self.context.symbols[sym.0 as usize].name.to_string(),
            Type::Fn { .. } => "function".to_string(),
            Type::TypeError => "<error>".to_string(),
        }
    }
}
