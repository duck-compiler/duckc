//! this compiler pass fills node_types and symbol types, consuming resolutions/definitions.
//! it walks no scopes. every reference/declaration lookup goes through the tables.

use crate::{ast::{AstRoot, Block, Expression, Identifier, MemoryTarget, NodeId, Span, Statement, expression::{BinaryOperator, Expr, ExpressionList, FieldInit}, memory_target::MemTar, statement::Stmt, type_expression::{self, TypeAnnotation, TypeExpression}}, backend::semantics::{context::SemanticsContext, diagnostic::Diagnostic, go_map, module::ModuleId, symbol::{Origin, SymbolId}, r#type::{Type, TypeId}}};

mod literal;
use literal::strip_literal_op;

pub fn typecheck_module<'src>(
    module: ModuleId,
    context: &mut SemanticsContext<'src>,
) {
    let ast = std::mem::replace(
        &mut context.modules[module.0 as usize].ast,
        AstRoot { statements: Vec::new() },
    );

    let mut checker = TypeChecker {
        context,
        module,
        current_return_type: None,
        in_loop: false
    };

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
    current_return_type: Option<TypeId>,
    in_loop: bool,
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
            TypeExpression::Int8 => Type::Int8,
            TypeExpression::Int16 => Type::Int16,
            TypeExpression::Int32 => Type::Int32,
            TypeExpression::Int64 => Type::Int64,
            TypeExpression::Uint => Type::Uint,
            TypeExpression::Uint8 => Type::Uint8,
            TypeExpression::Uint16 => Type::Uint16,
            TypeExpression::Uint32 => Type::Uint32,
            TypeExpression::Uint64 => Type::Uint64,
            TypeExpression::Float => Type::Float,
            TypeExpression::Float32 => Type::Float32,
            TypeExpression::Bool => Type::Bool,
            TypeExpression::String => Type::String,
            TypeExpression::Array { inner } => {
                let inner_id = self.type_id_from_type_expr(inner);
                Type::Array(inner_id)
            }
            TypeExpression::Pointer { inner } => {
                let inner_id = self.type_id_from_type_expr(inner);
                Type::Pointer(inner_id)
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
            Stmt::FunctionDefinition { params, body, return_type, .. } => {
                for param in &params.list {
                    let type_id = self.type_id_from_type_annotation(&param.type_);
                    if let Some(symbol) = self.definition_of(param.name.id) {
                        self.set_symbol_type(symbol, type_id);
                    }
                }

                let return_type_id = self.type_id_from_type_annotation(return_type);
                let previous_return_type = self.current_return_type.replace(return_type_id);

                self.check_block(body);

                self.current_return_type = previous_return_type;
            }
            Stmt::VariableDeclaration { name, type_, init_expression } => {
                let declared = type_.annotation.as_ref().map(|_| self.type_id_from_type_annotation(type_));
                let inferred = init_expression.as_ref().map(|expr| match declared {
                    Some(declared) => self.check_expected_expression_type(expr, declared),
                    None => self.check_expression(expr),
                });

                let type_id = match (declared, inferred) {
                    (Some(declared), Some(inferred)) => {
                        if !self.compatible(declared, inferred) {
                            self.report_mismatch(declared, inferred, name.span);
                        }

                        declared
                    }
                    (Some(declared), None) => declared,
                    (None, Some(inferred)) => inferred,
                    (None, None) => {
                        self.context.report(Diagnostic::cannot_infer_type(name.ident, name.span));
                        self.context.intern(Type::TypeError)
                    }
                };

                if let Some(sym) = self.definition_of(name.id) {
                    self.set_symbol_type(sym, type_id);
                }
            }
            Stmt::VariableAssignment { target, assign_expression } => {
                let target_type = self.check_memory_target(target);
                let value_type = self.check_expected_expression_type(assign_expression, target_type);
                if !self.compatible(target_type, value_type) {
                    self.report_mismatch(target_type, value_type, assign_expression.span);
                }
            }
            Stmt::Expression { expr } => {
                self.check_unused_expression(expr);
            }
            Stmt::Use(_) => {}
            Stmt::StructDefinition { .. } => {}
            Stmt::Return { value } => {
                let expected = self.current_return_type.unwrap_or_else(|| self.context.intern(Type::Unit));

                let found = match value {
                    Some(expr) => self.check_expected_expression_type(expr, expected),
                    None => self.context.intern(Type::Unit),
                };

                if !self.compatible(expected, found) {
                    self.report_mismatch(expected, found, statement.span);
                }
            }
            Stmt::Break | Stmt::Continue => {
                if !self.in_loop {
                    self.context.report(Diagnostic::break_or_continue_outside_loop(statement.span));
                }
            }
        }
    }

    fn check_block(&mut self, block: &Block<'src>) {
        for statement in &block.statements {
            self.check_statement(statement);
        }
    }

    // returns value typeid of last item in block
    fn check_block_value(&mut self, block: &Block<'src>, expected: Option<TypeId>) -> TypeId {
        let mut value = self.context.intern(Type::Unit);

        for (index, statement) in block.statements.iter().enumerate() {
            let is_last = index == block.statements.len() - 1;

            if is_last {
                match &statement.variant {
                    Stmt::Expression { expr } => {
                        value = match expected {
                            Some(expected) => self.check_expected_expression_type(expr, expected),
                            None => self.check_expression(expr),
                        };
                        continue;
                    }
                    Stmt::Return { .. } | Stmt::Break | Stmt::Continue => {
                        self.check_statement(statement);
                        value = self.context.intern(Type::Never);
                        continue;
                    }
                    _ => {}
                }
            }

            self.check_statement(statement);
        }

        value
    }

    fn check_unused_expression(&mut self, expr: &Expression<'src>) {
        let Expr::If {
            expr: condition,
            body, else_branch
        } = &*expr.variant
        else {
            self.check_expression(expr);
            return;
        };

        self.check_condition(condition);
        self.check_block(body);

        if let Some(else_body) = else_branch {
            self.check_block(else_body);
        }

        let unit = self.context.intern(Type::Unit);
        self.set_node_type(expr.id, unit);
    }

    fn check_expression(&mut self, expr: &Expression<'src>) -> TypeId {
        let type_id = match &*expr.variant {
            Expr::StringLiteral(_) => self.context.intern(Type::String),
            Expr::IntLiteral(_) | Expr::FloatLiteral(_) => self.default_literal_type(expr, false),
            Expr::BoolLiteral(_) => self.context.intern(Type::Bool),
            Expr::MemoryTarget(memory_target) => self.check_memory_target(memory_target),
            Expr::FunctionCall { target, args } => self.check_function_call(target, args, expr.span),
            Expr::ArrayExpression { values_exprs } => self.check_array_expression(values_exprs, expr.span, None),
            Expr::StructInit { type_name, fields } => self.check_struct_init(type_name, fields, expr.span),
            Expr::Binary { left, op, right } => {
                let (left_type, right_type) = if strip_literal_op(left).is_some() && strip_literal_op(right).is_none() {
                    let right_type = self.check_expression(right);
                    let left_type = self.check_expected_expression_type(left, right_type);
                    (left_type, right_type)
                } else {
                    let left_type = self.check_expression(left);
                    let right_type = self.check_expected_expression_type(right, left_type);
                    let left_type = self.try_represent_literal(left, right_type).unwrap_or(left_type);
                    (left_type, right_type)
                };

                if !self.compatible(left_type, right_type) {
                    self.report_mismatch(left_type, right_type, expr.span);
                }

                match op {
                    BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div => left_type,
                    BinaryOperator::Eq | BinaryOperator::NotEq
                    | BinaryOperator::Less | BinaryOperator::Greater
                    | BinaryOperator::LessEq | BinaryOperator::GreaterEq => self.context.intern(Type::Bool),
                    BinaryOperator::And | BinaryOperator::Or => {
                        let bool_type = self.context.intern(Type::Bool);

                        if !self.compatible(bool_type, left_type) {
                            self.report_mismatch(bool_type, left_type, left.span);
                        }

                        if !self.compatible(bool_type, right_type) {
                            self.report_mismatch(bool_type, right_type, right.span);
                        }

                        bool_type
                    }
                }
            }
            Expr::Unary { expr: inner, .. } => match strip_literal_op(expr) {
                Some((literal, negated)) => self.default_literal_type(literal, negated),
                None => self.check_expression(inner),
            },
            Expr::Reference { expr: inner } => self.check_reference(inner, expr.span),
            Expr::If { expr: condition, body, else_branch } => {
                self.check_if(expr.span, condition, body, else_branch.as_ref(), None)
            }
            Expr::While { expr: condition, body } => {
                self.check_condition(condition);

                let was_in_loop = self.in_loop;
                self.in_loop = true;
                self.check_block(body);
                self.in_loop = was_in_loop;

                self.context.intern(Type::Unit)
            }
        };
        self.set_node_type(expr.id, type_id);
        type_id
    }

    fn check_expected_expression_type(
        &mut self,
        expr: &Expression<'src>,
        expected: TypeId
    ) -> TypeId {
        if let Some(type_id) = self.try_represent_literal(expr, expected) {
            return type_id;
        }

        if let Some(type_id) = self.deep_check_expected_expression_type(expr, expected) {
            return type_id;
        }

        self.check_expression(expr)
    }

    fn deep_check_expected_expression_type(&mut self, expr: &Expression<'src>, expected: TypeId) -> Option<TypeId> {
        let type_id = match &*expr.variant {
            Expr::If { expr: condition, body, else_branch } => {
                self.check_if(expr.span, condition, body, else_branch.as_ref(), Some(expected))
            }
            Expr::ArrayExpression { values_exprs } => {
                let Type::Array(elem_type) = self.context.types[expected.0 as usize] else {
                    return None;
                };

                self.check_array_expression(values_exprs, expr.span, Some(elem_type))
            }
            _ => return None,
        };

        self.set_node_type(expr.id, type_id);
        Some(type_id)
    }

    fn check_if(
        &mut self,
        span: Span<'src>,
        condition: &Expression<'src>,
        body: &Block<'src>,
        else_branch: Option<&Block<'src>>,
        expected: Option<TypeId>,
    ) -> TypeId {
        self.check_condition(condition);

        let then_type = self.check_block_value(body, expected);

        let Some(else_body) = else_branch else {
            let then_produces_value = !matches!(
                self.context.types[then_type.0 as usize],
                Type::Unit | Type::Never | Type::TypeError,
            );

            if then_produces_value {
                self.context.report(Diagnostic::if_without_else_as_value(span));
                return self.context.intern(Type::TypeError);
            }

            return self.context.intern(Type::Unit);
        };

        let else_type = self.check_block_value(else_body, expected);
        if !self.compatible(then_type, else_type) {
            self.report_mismatch(then_type, else_type, span);
        }

        if self.context.types[then_type.0 as usize] == Type::Never {
            else_type
        } else {
            then_type
        }
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
                let Some(symbol) = self.resolution_of(identifier.id) else {
                    return self.context.intern(Type::TypeError);
                };

                match self.symbol_type(symbol) {
                    Some(type_id) => type_id,
                    None => {
                        self.context.report(Diagnostic::not_a_value(identifier.ident, memory_target.span));
                        self.context.intern(Type::TypeError)
                    }
                }
            }
            MemTar::FieldAccess { target, field_name } => {
                self.check_field_access(target, field_name, memory_target.span)
            }
            MemTar::ArrayAccess { target, index_expression } => {
                self.check_array_access(target, index_expression, memory_target.span)
            }
            MemTar::Dereference(inner) => {
                let inner_type = self.check_expression(inner);
                if self.is_poisoned(inner_type) {
                    return inner_type;
                }

                match &self.context.types[inner_type.0 as usize] {
                    Type::Pointer(pointee) => *pointee,
                    _ => {
                        self.context.report(Diagnostic::not_a_pointer(memory_target.span));
                        self.context.intern(Type::TypeError)
                    }
                }
            }
        }
    }

    fn check_reference(&mut self, inner: &Expression<'src>, span: Span<'src>) -> TypeId {
        let inner_type = self.check_expression(inner);

        if self.is_poisoned(inner_type) {
            return inner_type;
        }

        if !self.is_addressable(inner, inner_type) {
            self.context.report(Diagnostic::not_addressable(span));
            return self.context.intern(Type::TypeError);
        }

        self.context.intern(Type::Pointer(inner_type))
    }

    fn is_addressable(&self, expr: &Expression<'src>, type_id: TypeId) -> bool {
        if matches!(self.context.types[type_id.0 as usize], Type::Fn { .. }) {
            return false;
        }

        matches!(
            &*expr.variant,
            Expr::MemoryTarget(_) | Expr::StructInit { .. } | Expr::ArrayExpression { .. }
        )
    }

    fn check_array_expression(
        &mut self,
        values_exprs: &Vec<Box<Expression<'src>>>,
        span: Span<'src>,
        expected_elem: Option<TypeId>,
    ) -> TypeId {
        let Some((first, remaining)) = values_exprs.split_first() else {
            self.context.report(Diagnostic::empty_array_literal(span));
            return self.context.intern(Type::TypeError);
        };

        let elem_type = match expected_elem {
            Some(expected) => self.check_expected_expression_type(first, expected),
            None => self.check_expression(first),
        };

        for value in remaining {
            let value_type = self.check_expected_expression_type(value, elem_type);
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

        if self.is_poisoned(target_type) {
            return target_type;
        }

        match &self.context.types[target_type.0 as usize] {
            Type::Array(inner) => *inner,
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
                            .expect("package must already be loaded, lookup above just succeeded against it");

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
        let target_type = match &self.context.types[target_type.0 as usize] {
            Type::Pointer(p) => *p,
            _ => target_type,
        };

        if self.is_poisoned(target_type) {
            return target_type;
        }

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
            let matching_field = declared_fields
                .iter()
                .find(|(name, _)| *name == field.name.ident)
                .copied();

            let Some((_, declared_type)) = matching_field else {
                self.check_expression(&field.value);
                self.context.report(Diagnostic::unknown_struct_field(struct_name, field.name.ident, field.name.span));
                continue;
            };

            let value_type = self.check_expected_expression_type(&field.value, declared_type);
            if !self.compatible(declared_type, value_type) {
                self.report_mismatch(declared_type, value_type, field.value.span);
            }

            known.push(field.name.ident);
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

        let Type::Fn { params, return_type } = self.context.types[callee_type.0 as usize].clone() else {
            self.check_arguments(args);

            if self.is_poisoned(callee_type) {
                return callee_type;
            }

            self.context.report(Diagnostic::not_callable(call_span));
            return self.context.intern(Type::TypeError);
        };

        if params.len() != args.list.len() {
            self.check_arguments(args);

            self.context.report(Diagnostic::wrong_arg_count(params.len(), args.list.len(), call_span));
            return return_type;
        }

        for (expected, arg) in params.iter().zip(args.list.iter()) {
            let found = self.check_expected_expression_type(arg, *expected);
            if !self.compatible(*expected, found) {
                self.report_mismatch(*expected, found, call_span);
            }
        }

        return_type
    }

    fn check_arguments(&mut self, args: &ExpressionList<'src>) {
        for arg in &args.list {
            self.check_expression(arg);
        }
    }

    fn is_poisoned(&self, type_id: TypeId) -> bool {
        matches!(self.context.types[type_id.0 as usize], Type::TypeError | Type::Never)
    }

    fn compatible(&self, a: TypeId, b: TypeId) -> bool {
        a == b || self.is_poisoned(a) || self.is_poisoned(b)
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
            Type::Int8 => "int8".to_string(),
            Type::Int16 => "int16".to_string(),
            Type::Int32 => "int32".to_string(),
            Type::Int64 => "int64".to_string(),
            Type::Uint => "uint".to_string(),
            Type::Uint8 => "uint8".to_string(),
            Type::Uint16 => "uint16".to_string(),
            Type::Uint32 => "uint32".to_string(),
            Type::Uint64 => "uint64".to_string(),
            Type::Float => "float".to_string(),
            Type::Float32 => "float32".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "string".to_string(),
            Type::Array(inner) => format!("[{}]", self.type_name(*inner)),
            Type::Pointer(inner) => format!("*{}", self.type_name(*inner)),
            Type::Struct(sym) => self.context.symbols[sym.0 as usize].name.to_string(),
            Type::Fn { .. } => "function".to_string(),
            Type::Never => "never".to_string(),
            Type::TypeError => "<error>".to_string(),
        }
    }
}
