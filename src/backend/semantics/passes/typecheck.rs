//! this compiler pass fills node_types and symbol types, consuming resolutions/definitions.
//! it walks no scopes. every reference/declaration lookup goes through the tables.

use std::collections::HashMap;
use std::rc::Rc;

use crate::{ast::{AstRoot, Block, Expression, Identifier, MemoryTarget, NodeId, Span, Statement, expression::{BinaryOperator, Expr, ExpressionList, FieldInit}, memory_target::MemTar, statement::Stmt, struct_definition::{ImplBlock, Method, MethodKind, Visibility}, type_expression::{TypeAnnotation, TypeExpression}}, backend::semantics::{context::{FreeFunctionMethodKey, MethodSignature, SemanticsContext}, diagnostic::Diagnostic, go_map, module::ModuleId, symbol::{Origin, SymbolId, SymbolKind}, r#type::{Type, TypeId, TypeParamId}}};

mod generics;
mod literal;
use generics::TypeParamBindings;
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
        current_impl_struct: None,
        in_loop: false,
        target_node: None,
        pending_generic_call: None,
    };

    for statement in &ast.statements {
        checker.declare_signature(statement);
    }

    for statement in &ast.statements {
        checker.check_statement(statement);
    }

    context.modules[module.0 as usize].ast = ast;
}

#[derive(Clone, Copy, PartialEq)]
enum Usage {
    Read,
    Write,
}

struct TypeChecker<'a, 'src> {
    context: &'a mut SemanticsContext<'src>,
    module: ModuleId,
    current_return_type: Option<TypeId>,
    current_impl_struct: Option<SymbolId>,
    in_loop: bool,
    target_node: Option<NodeId>,
    pending_generic_call: Option<PendingGenericCall>,
}

struct PendingGenericCall {
    node: NodeId,
    type_params: Rc<[TypeParamId]>,
    bound_type_arguments: Vec<TypeId>,
}

struct MethodAccess<'ast, 'src> {
    struct_symbol: SymbolId,
    field_name: &'ast Identifier<'src>,
    type_args: &'ast [TypeExpression<'src>],
    receiver_type: Option<TypeId>,
    bound_type_arguments: Vec<TypeId>,
    open_type_params: Rc<[TypeParamId]>,
    signature: MethodSignature,
}

impl<'a, 'src> TypeChecker<'a, 'src> {
    fn set_node_type(&mut self, node: NodeId, type_id: TypeId) {
        self.context.modules[self.module.0 as usize].node_types[node.0 as usize] = Some(type_id);
    }

    fn set_expr_type(&mut self, node: NodeId, type_: Type) -> TypeId {
        let type_id = self.context.intern(type_);
        self.set_node_type(node, type_id);

        type_id
    }

    fn first_type_param(&self, type_ids: &[TypeId]) -> Option<TypeParamId> {
        type_ids
            .iter()
            .find_map(|type_id| match &self.context.types[type_id.0 as usize] {
                Type::TypeParam(type_param) => Some(*type_param),
                Type::Struct(_, arguments) => self.first_type_param(arguments),
                Type::Tuple(elements) => self.first_type_param(elements),
                Type::Array(inner) => self.first_type_param(std::slice::from_ref(inner)),
                Type::Unit | Type::Int | Type::Int8 | Type::Int16
                | Type::Int32 | Type::Int64 | Type::Uint | Type::Uint8
                | Type::Uint16 | Type::Uint32 | Type::Uint64 | Type::Float
                | Type::Float32 | Type::Bool | Type::String
                | Type::Pointer(_) | Type::Fn { .. } | Type::Never | Type::TypeError => None,
            })
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

    fn type_id_from_type_annotation(&mut self, annotation: &TypeAnnotation<'src>) -> TypeId {
        match &annotation.annotation {
            None => self.context.intern(Type::Unit),
            Some(type_expr) => self.type_id_from_type_expr(type_expr),
        }
    }

    fn type_id_from_type_expr(&mut self, type_expr: &TypeExpression<'src>) -> TypeId {
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
            TypeExpression::Tuple(elements) => {
                let element_ids = elements
                    .iter()
                    .map(|element| self.type_id_from_type_expr(element))
                    .collect::<Vec<_>>();

                Type::Tuple(element_ids)
            }
            TypeExpression::Ident { name, type_args } => {
                return match self.resolution_of(name.id) {
                    Some(symbol) => self.named_type(symbol, type_args, name.span),
                    None => self.context.intern(Type::TypeError),
                };
            }
        };
        self.context.intern(type_)
    }

    fn named_type(
        &mut self,
        symbol: SymbolId,
        type_args: &[TypeExpression<'src>],
        span: Span<'src>,
    ) -> TypeId {
        let arguments = type_args
            .iter()
            .map(|type_arg| self.type_id_from_type_expr(type_arg))
            .collect::<Vec<_>>();

        let declared = match self.context.symbols[symbol.0 as usize].kind {
            SymbolKind::TypeParam => 0,
            _ => self.context.type_params_of(symbol).len(),
        };

        if declared != arguments.len() {
            self.context.report(Diagnostic::wrong_type_arg_count(declared, arguments.len(), span));
            return self.context.intern(Type::TypeError);
        }

        if matches!(self.context.symbols[symbol.0 as usize].kind, SymbolKind::TypeParam) {
            return self.symbol_type(symbol)
                .unwrap_or_else(|| self.context.intern(Type::TypeError));
        }

        self.context.intern(Type::Struct(symbol, arguments))
    }

    fn declare_signature(&mut self, statement: &Statement<'src>) {
        match &statement.variant {
            Stmt::FunctionDefinition { name, params, return_type, type_params: _, body: _ } => {
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
            Stmt::StructDefinition { name, fields, impl_block, .. } => {
                let field_types = fields
                    .iter()
                    .map(|field| (field.name.ident, self.type_id_from_type_annotation(&field.type_), field.visibility))
                    .collect::<Vec<_>>();

                let Some(symbol) = self.definition_of(name.id) else {
                    return;
                };

                self.context.struct_fields.insert(symbol, field_types);

                if let Some(impl_block) = impl_block {
                    self.declare_method_signatures(symbol, impl_block);
                }
            }
            _ => {}
        }
    }

    fn declare_method_signatures(&mut self, struct_symbol: SymbolId, impl_block: &ImplBlock<'src>) {
        let mut signatures = HashMap::new();

        for method in &impl_block.methods {
            let params = method
                .params
                .list
                .iter()
                .map(|param| self.type_id_from_type_annotation(&param.type_))
                .collect::<Vec<_>>();

            let return_type = self.type_id_from_type_annotation(&method.return_type);

            let name_taken = signatures.contains_key(method.name.ident)
                || self.struct_field(struct_symbol, method.name.ident).is_some();

            if name_taken {
                self.context.report(Diagnostic::already_defined(method.name.ident, method.name.span));
                continue;
            }

            signatures.insert(method.name.ident, MethodSignature {
                kind: method.kind,
                visibility: method.visibility,
                value_type: self.context.intern(Type::Fn { params, return_type }),
                type_params: self.method_type_params(method),
            });
        }

        self.context.struct_methods.insert(struct_symbol, signatures);
    }

    fn method_type_params(&self, method: &Method<'src>) -> Rc<[TypeParamId]> {
        self.context.modules[self.module.0 as usize]
            .method_type_params
            .get(&method.name.id)
            .map_or_else(|| Rc::from([]), |type_params| Rc::from(type_params.as_slice()))
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
                let target_type = self.check_memory_target(target, Usage::Write);
                let value_type = self.check_expected_expression_type(assign_expression, target_type);
                if !self.compatible(target_type, value_type) {
                    self.report_mismatch(target_type, value_type, assign_expression.span);
                }
            }
            Stmt::Expression { expr } => {
                self.check_unused_expression(expr);
            }
            Stmt::Use(_) => {}
            Stmt::StructDefinition { name, impl_block, fields: _, type_params: _ } => {
                let Some(impl_block) = impl_block else {
                    return;
                };

                let Some(struct_symbol) = self.definition_of(name.id) else {
                    return;
                };

                for method in &impl_block.methods {
                    self.check_method(struct_symbol, method);
                }
            }
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

    fn check_method(&mut self, struct_symbol: SymbolId, method: &Method<'src>) {
        for param in &method.params.list {
            let type_id = self.type_id_from_type_annotation(&param.type_);
            if let Some(symbol) = self.definition_of(param.name.id) {
                self.set_symbol_type(symbol, type_id);
            }
        }

        if let Some(symbol) = self.self_symbol_of(method) {
            let struct_type = self.declared_struct_type(struct_symbol);
            let self_type = self.context.intern(Type::Pointer(struct_type));
            self.set_symbol_type(symbol, self_type);
        }

        let return_type_id = self.type_id_from_type_annotation(&method.return_type);
        let previous_return_type = self.current_return_type.replace(return_type_id);
        let previous_impl_struct = self.current_impl_struct.replace(struct_symbol);

        self.check_block(&method.body);

        self.current_return_type = previous_return_type;
        self.current_impl_struct = previous_impl_struct;
    }

    fn self_symbol_of(&self, method: &Method<'src>) -> Option<SymbolId> {
        self.context.modules[self.module.0 as usize].self_symbols.get(&method.name.id).copied()
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
            Expr::MemoryTarget(memory_target) => self.check_memory_target(memory_target, Usage::Read),
            Expr::FunctionCall { target, type_args, args } => {
                self.check_function_call(target, type_args, args, expr.span)
            }
            Expr::ArrayExpression { values_exprs } => self.check_array_expression(values_exprs, expr.span, None),
            Expr::TupleExpression { values } => self.check_tuple_expression(values, None),
            Expr::StructInit { type_name, type_args, fields } => {
                self.check_struct_init(type_name, type_args, fields, expr.span)
            }
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

                if let Some(type_param) = self.first_type_param(&[left_type, right_type]) {
                    let name = self.context.type_param_name(type_param);
                    self.context.report(Diagnostic::operator_on_type_param(name, expr.span));
                    return self.set_expr_type(expr.id, Type::TypeError);
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
            Expr::Unary { expr: inner, .. } => {
                if let Some((literal, negated)) = strip_literal_op(expr) {
                    self.default_literal_type(literal, negated)
                } else {
                    let inner_type = self.check_expression(inner);

                    if let Some(type_param) = self.first_type_param(&[inner_type]) {
                        let name = self.context.type_param_name(type_param);
                        self.context.report(Diagnostic::operator_on_type_param(name, expr.span));
                        return self.set_expr_type(expr.id, Type::TypeError);
                    }

                    inner_type
                }
            }
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
            Expr::TupleExpression { values } => {
                let Type::Tuple(element_types) = self.context.types[expected.0 as usize].clone() else {
                    return None;
                };

                self.check_tuple_expression(values, Some(element_types))
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

    fn check_memory_target(&mut self, memory_target: &MemoryTarget<'src>, usage: Usage) -> TypeId {
        match &memory_target.variant {
            MemTar::Name(identifier) => self.check_name(identifier, memory_target.span),
            MemTar::FieldAccess { target, field_name, type_args } => {
                self.check_field_access(target, field_name, type_args, memory_target.span, usage)
            }
            MemTar::ArrayAccess { target, index_expression } => {
                self.check_array_access(target, index_expression, memory_target.span, usage)
            }
            MemTar::TupleIndex { target, index } => {
                self.check_tuple_index(target, *index, memory_target.span, usage)
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

    fn check_name(&mut self, identifier: &Identifier<'src>, span: Span<'src>) -> TypeId {
        let Some(symbol) = self.resolution_of(identifier.id) else {
            return self.context.intern(Type::TypeError);
        };

        let is_type_param = matches!(self.context.symbols[symbol.0 as usize].kind, SymbolKind::TypeParam);

        let value_type = match self.symbol_type(symbol) {
            Some(type_id) if !is_type_param => type_id,
            _ => {
                self.context.report(Diagnostic::not_a_value(identifier.ident, span));
                return self.context.intern(Type::TypeError);
            }
        };

        let type_params = self.context.type_params_of(symbol);
        if type_params.is_empty() {
            return value_type;
        }

        if self.target_node != Some(identifier.id) {
            self.context.report(Diagnostic::generic_function_must_be_called(identifier.ident, span));
            return self.context.intern(Type::TypeError);
        }

        self.pending_generic_call = Some(PendingGenericCall {
            node: identifier.id,
            type_params: Rc::from(type_params),
            bound_type_arguments: Vec::new(),
        });

        value_type
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

    fn check_tuple_expression(
        &mut self,
        values: &Vec<Box<Expression<'src>>>,
        expected_elements: Option<Vec<TypeId>>,
    ) -> TypeId {
        let element_types = match expected_elements.filter(|expected| expected.len() == values.len()) {
            Some(expected) => values
                .iter()
                .zip(expected)
                .map(|(value, expected)| self.check_expected_expression_type(value, expected))
                .collect::<Vec<_>>(),
            None => values
                .iter()
                .map(|value| self.check_expression(value))
                .collect::<Vec<_>>(),
        };

        self.context.intern(Type::Tuple(element_types))
    }

    fn check_tuple_index(
        &mut self,
        target: &MemoryTarget<'src>,
        index: usize,
        span: Span<'src>,
        usage: Usage,
    ) -> TypeId {
        let target_type = self.check_memory_target(target, usage);

        if self.is_poisoned(target_type) {
            return target_type;
        }

        let Type::Tuple(elements) = self.context.types[target_type.0 as usize].clone() else {
            self.context.report(Diagnostic::not_indexable(span));
            return self.context.intern(Type::TypeError);
        };

        match elements.get(index) {
            Some(element) => *element,
            None => {
                self.context.report(Diagnostic::tuple_index_out_of_range(index, elements.len(), span));
                self.context.intern(Type::TypeError)
            }
        }
    }

    fn check_array_access(
        &mut self,
        target: &MemoryTarget<'src>,
        index_expression: &Expression<'src>,
        span: Span<'src>,
        usage: Usage,
    ) -> TypeId {
        let target_type = self.check_memory_target(target, usage);
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
        type_args: &[TypeExpression<'src>],
        span: Span<'src>,
        usage: Usage,
    ) -> TypeId {
        if let MemTar::Name(identifier) = &target.variant {
            let go_package = self.resolution_of(identifier.id).and_then(|sym| {
                match &self.context.symbols[sym.0 as usize].origin {
                    Origin::GoPackage { path } => Some(*path),
                    Origin::Duck { .. } | Origin::GoType { .. } => None,
                }
            });

            if let Some(package) = go_package {
                self.reject_type_args(type_args, span);

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

            if let Some(struct_symbol) = self.named_struct_symbol(identifier) {
                return self.check_static_member(struct_symbol, field_name, type_args, span, usage);
            }
        }

        let receiver_type = self.check_memory_target(target, Usage::Read);
        let target_type = match &self.context.types[receiver_type.0 as usize] {
            Type::Pointer(pointee) => *pointee,
            _ => receiver_type,
        };

        if self.is_poisoned(target_type) {
            return target_type;
        }

        match self.context.types[target_type.0 as usize].clone() {
            Type::Struct(struct_symbol, arguments) => {
                self.check_struct_member(struct_symbol, arguments, receiver_type, field_name, type_args, span, usage)
            }
            _ => {
                self.context.report(Diagnostic::not_a_struct(span));
                self.context.intern(Type::TypeError)
            }
        }
    }

    fn check_struct_member(
        &mut self,
        struct_symbol: SymbolId,
        arguments: Vec<TypeId>,
        receiver_type: TypeId,
        field_name: &Identifier<'src>,
        type_args: &[TypeExpression<'src>],
        span: Span<'src>,
        usage: Usage,
    ) -> TypeId {
        if let Some((type_id, visibility)) = self.struct_field(struct_symbol, field_name.ident) {
            self.reject_type_args(type_args, span);
            self.check_visible(struct_symbol, field_name.ident, visibility, span);

            let bindings = self.struct_bindings(struct_symbol, &arguments);
            return self.substitute_type(type_id, &bindings);
        }

        let struct_name = self.context.symbols[struct_symbol.0 as usize].name;

        let Some(signature) = self.method_signature(struct_symbol, field_name.ident) else {
            self.context.report(Diagnostic::unknown_struct_field(struct_name, field_name.ident, span));
            return self.context.intern(Type::TypeError);
        };

        if matches!(signature.kind, MethodKind::Static) {
            self.context.report(Diagnostic::wrong_method_receiver(struct_name, field_name.ident, signature.kind, span));
            return self.context.intern(Type::TypeError);
        }

        self.method_value(MethodAccess {
            struct_symbol,
            field_name,
            type_args,
            receiver_type: Some(receiver_type),
            bound_type_arguments: arguments,
            open_type_params: Rc::clone(&signature.type_params),
            signature,
        }, span, usage)
    }

    fn check_static_member(
        &mut self,
        struct_symbol: SymbolId,
        field_name: &Identifier<'src>,
        type_args: &[TypeExpression<'src>],
        span: Span<'src>,
        usage: Usage,
    ) -> TypeId {
        let struct_name = self.context.symbols[struct_symbol.0 as usize].name;

        let Some(signature) = self.method_signature(struct_symbol, field_name.ident) else {
            let diagnostic = match self.struct_field(struct_symbol, field_name.ident) {
                Some(_) => Diagnostic::field_needs_instance(struct_name, field_name.ident, span),
                None => Diagnostic::not_a_value(struct_name, span),
            };

            self.context.report(diagnostic);
            return self.context.intern(Type::TypeError);
        };

        if matches!(signature.kind, MethodKind::Instance) {
            self.context.report(Diagnostic::wrong_method_receiver(struct_name, field_name.ident, signature.kind, span));
            return self.context.intern(Type::TypeError);
        }

        let struct_type_params = self.context.type_params_of(struct_symbol);
        let open_type_params = match struct_type_params.is_empty() {
            true => Rc::clone(&signature.type_params),
            false => struct_type_params.iter().chain(signature.type_params.iter()).copied().collect(),
        };

        self.method_value(MethodAccess {
            struct_symbol,
            field_name,
            type_args,
            receiver_type: None,
            bound_type_arguments: Vec::new(),
            open_type_params,
            signature,
        }, span, usage)
    }

    fn method_value(&mut self, access: MethodAccess<'_, 'src>, span: Span<'src>, usage: Usage) -> TypeId {
        let struct_name = self.context.symbols[access.struct_symbol.0 as usize].name;

        if usage == Usage::Write {
            self.context.report(Diagnostic::cannot_assign_to_method(struct_name, access.field_name.ident, span));
            return self.context.intern(Type::TypeError);
        }

        self.check_visible(access.struct_symbol, access.field_name.ident, access.signature.visibility, span);

        let mut bindings = self.struct_bindings(access.struct_symbol, &access.bound_type_arguments);

        if !access.type_args.is_empty() {
            self.bind_explicit_type_args(access.type_args, &access.open_type_params, span, &mut bindings);
        }

        let value_type = self.substitute_type(access.signature.value_type, &bindings);
        let open = access.open_type_params.iter().any(|type_param| !bindings.contains_key(type_param));

        if open && self.target_node != Some(access.field_name.id) {
            self.context.report(Diagnostic::generic_member_needs_type_args(access.field_name.ident, span));
            return self.context.intern(Type::TypeError);
        }

        if access.signature.is_free_function() {
            let name = self.free_function_name_of(access.struct_symbol, access.field_name.ident);
            self.store_free_function(access.field_name.id, name, access.receiver_type, value_type);
        }

        if open {
            self.pending_generic_call = Some(PendingGenericCall {
                node: access.field_name.id,
                type_params: access.open_type_params,
                bound_type_arguments: access.bound_type_arguments,
            });

            return value_type;
        }

        if access.signature.is_free_function() {
            let Some(resolved) = self.resolved_type_arguments(&access.open_type_params, &bindings, span) else {
                return self.context.intern(Type::TypeError);
            };

            let mut arguments = access.bound_type_arguments;
            arguments.extend(resolved);

            self.record_type_arguments(access.field_name.id, arguments);
        }

        value_type
    }

    fn reject_type_args(&mut self, type_args: &[TypeExpression<'src>], span: Span<'src>) {
        if type_args.is_empty() {
            return;
        }

        self.context.report(Diagnostic::wrong_type_arg_count(0, type_args.len(), span));
    }

    fn check_visible(
        &mut self,
        struct_symbol: SymbolId,
        member_name: &str,
        visibility: Visibility,
        span: Span<'src>,
    ) {
        if matches!(visibility, Visibility::Public) || self.current_impl_struct == Some(struct_symbol) {
            return;
        }

        let struct_name = self.context.symbols[struct_symbol.0 as usize].name;
        self.context.report(Diagnostic::private_member(struct_name, member_name, span));
    }

    fn struct_field(&self, struct_symbol: SymbolId, name: &str) -> Option<(TypeId, Visibility)> {
        self.context.struct_fields
            .get(&struct_symbol)?
            .iter()
            .find(|(field_name, _, _)| *field_name == name)
            .map(|(_, type_id, visibility)| (*type_id, *visibility))
    }

    fn method_signature(&self, struct_symbol: SymbolId, name: &str) -> Option<MethodSignature> {
        self.context.struct_methods.get(&struct_symbol)?.get(name).cloned()
    }

    fn free_function_name_of(&self, struct_symbol: SymbolId, method_name: &'src str) -> &'src str {
        let key = FreeFunctionMethodKey { struct_symbol, method_name };
        match self.context.mangled_free_function_names.get(&key) {
            Some(name) => name,
            None => self.context.free_function_method_name(
                self.context.symbols[struct_symbol.0 as usize].name,
                method_name,
            ),
        }
    }

    fn named_struct_symbol(&self, identifier: &Identifier<'src>) -> Option<SymbolId> {
        let symbol = self.resolution_of(identifier.id)?;
        matches!(self.context.symbols[symbol.0 as usize].kind, SymbolKind::Struct).then_some(symbol)
    }

    fn check_struct_init(
        &mut self,
        type_name: &Identifier<'src>,
        type_args: &[TypeExpression<'src>],
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
        let type_params = self.context.type_params_of(struct_symbol).to_vec();
        let mut bindings = TypeParamBindings::new();

        if !type_args.is_empty() {
            self.bind_explicit_type_args(type_args, &type_params, span, &mut bindings);
        }

        let mut known = Vec::with_capacity(fields.len());
        for field in fields {
            let matching_field = declared_fields
                .iter()
                .find(|(name, _, _)| *name == field.name.ident)
                .copied();

            let Some((_, declared_type, visibility)) = matching_field else {
                self.check_expression(&field.value);
                self.context.report(Diagnostic::unknown_struct_field(struct_name, field.name.ident, field.name.span));
                continue;
            };

            self.check_visible(struct_symbol, field.name.ident, visibility, field.name.span);
            self.check_argument(&field.value, declared_type, &type_params, field.value.span, &mut bindings);

            known.push(field.name.ident);
        }

        let missing = declared_fields
            .iter()
            .map(|(name, _, _)| *name)
            .filter(|name| !known.contains(name))
            .collect::<Vec<_>>();

        if !missing.is_empty() {
            self.context.report(Diagnostic::missing_struct_fields(struct_name, &missing, span));
        }

        let Some(arguments) = self.resolved_type_arguments(&type_params, &bindings, span) else {
            return self.context.intern(Type::TypeError);
        };

        if self.holds_type_error(&arguments) {
            return self.context.intern(Type::TypeError);
        }

        self.context.intern(Type::Struct(struct_symbol, arguments))
    }

    fn check_function_call(
        &mut self,
        target: &Expression<'src>,
        type_args: &[TypeExpression<'src>],
        args: &ExpressionList<'src>,
        call_span: Span<'src>,
    ) -> TypeId {
        let (target_type, pending) = self.check_target(target);

        let Type::Fn { params, return_type } = self.context.types[target_type.0 as usize].clone() else {
            self.check_arguments(args);

            if self.is_poisoned(target_type) {
                return target_type;
            }

            self.context.report(Diagnostic::not_callable(call_span));
            return self.context.intern(Type::TypeError);
        };

        let type_params = pending.as_ref().map_or(&[][..], |target| &target.type_params);
        let mut bindings = TypeParamBindings::new();

        if !type_args.is_empty() {
            match pending.is_none() && !target_type_args(target).is_empty() {
                true => self.context.report(Diagnostic::type_args_given_twice(call_span)),
                false => self.bind_explicit_type_args(type_args, type_params, call_span, &mut bindings),
            }
        }

        if params.len() != args.list.len() {
            self.check_arguments(args);

            self.context.report(Diagnostic::wrong_arg_count(params.len(), args.list.len(), call_span));
            self.poison_unbound_type_params(type_params, &mut bindings);

            return self.substitute_type(return_type, &bindings);
        }

        for (declared, arg) in params.iter().zip(args.list.iter()) {
            self.check_argument(arg, *declared, type_params, call_span, &mut bindings);
        }

        let Some(pending) = pending else {
            return return_type;
        };

        let Some(resolved) = self.resolved_type_arguments(&pending.type_params, &bindings, call_span) else {
            return self.context.intern(Type::TypeError);
        };

        let mut arguments = pending.bound_type_arguments;
        arguments.extend(resolved);

        self.record_type_arguments(pending.node, arguments);

        self.substitute_type(return_type, &bindings)
    }

    fn check_target(&mut self, target: &Expression<'src>) -> (TypeId, Option<PendingGenericCall>) {
        let target_node = target_identifier(target);

        let previous_target_node = self.target_node;
        self.target_node = target_node;

        let target_type = self.check_expression(target);

        self.target_node = previous_target_node;

        (target_type, self.take_generic_call_for(target_node))
    }

    fn take_generic_call_for(&mut self, target_node: Option<NodeId>) -> Option<PendingGenericCall> {
        self.pending_generic_call
            .take()
            .filter(|pending| Some(pending.node) == target_node)
    }

    fn check_argument(
        &mut self,
        arg: &Expression<'src>,
        declared: TypeId,
        type_params: &[TypeParamId],
        span: Span<'src>,
        bindings: &mut TypeParamBindings,
    ) {
        let expected = self.substitute_type(declared, bindings);

        if !self.contains_type_param(expected, type_params) {
            let found = self.check_expected_expression_type(arg, expected);
            if !self.compatible(expected, found) {
                self.report_mismatch(expected, found, span);
            }

            return;
        }

        let found = self.check_expression(arg);
        let bound = !self.holds_type_error(&[found])
            && self.bind_type_params(expected, found, arg.span, bindings);

        if !bound {
            self.poison_type_params_of(expected, type_params, bindings);
            return;
        }

        let expected = self.substitute_type(expected, bindings);
        if !self.contains_type_param(expected, type_params) && !self.compatible(expected, found) {
            self.report_mismatch(expected, found, span);
        }
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
            Type::Tuple(elements) => format!(
                "({})",
                elements.iter().map(|element| self.type_name(*element)).collect::<Vec<_>>().join(", "),
            ),
            Type::Struct(symbol, arguments) => {
                let name = self.context.symbols[symbol.0 as usize].name;

                match arguments.is_empty() {
                    true => name.to_string(),
                    false => format!(
                        "{}<{}>",
                        name,
                        arguments.iter().map(|argument| self.type_name(*argument)).collect::<Vec<_>>().join(", "),
                    ),
                }
            }
            Type::TypeParam(type_param) => self.context.type_param_name(*type_param).to_string(),
            Type::Fn { .. } => "function".to_string(),
            Type::Never => "never".to_string(),
            Type::TypeError => "<error>".to_string(),
        }
    }
}

fn target_type_args<'e, 'src>(target: &'e Expression<'src>) -> &'e [TypeExpression<'src>] {
    let Expr::MemoryTarget(memory_target) = &*target.variant else {
        return &[];
    };

    match &memory_target.variant {
        MemTar::FieldAccess { type_args, .. } => type_args,
        MemTar::Name(_) | MemTar::ArrayAccess { .. }
        | MemTar::TupleIndex { .. } | MemTar::Dereference(_) => &[],
    }
}

fn target_identifier<'src>(target: &Expression<'src>) -> Option<NodeId> {
    let Expr::MemoryTarget(memory_target) = &*target.variant else {
        return None;
    };

    match &memory_target.variant {
        MemTar::Name(identifier) => Some(identifier.id),
        MemTar::FieldAccess { field_name, .. } => Some(field_name.id),
        MemTar::ArrayAccess { .. } | MemTar::TupleIndex { .. } | MemTar::Dereference(_) => None,
    }
}
