use std::cell::Cell;

use crate::{ast::{Block, Expression, Identifier, MemoryTarget, NodeId, ParameterList, Statement, TypeExpression, expression::{Expr, ExpressionList}, memory_target::MemTar, statement::Stmt, struct_definition::{Method, MethodKind, SELF_NAME}, type_expression::{TypeAnnotation, TypeParam}}, backend::{gost::go_tree::{GoExpression, GoStatement, GoType, StructField, tuple_field_name}, semantics::{context::SemanticsContext, module::{FreeFunctionMember, ModuleId}, symbol::Origin, r#type::{Type, TypeId}}}};

fn type_param_names<'src>(type_params: &[TypeParam<'src>]) -> Vec<&'src str> {
    type_params.iter().map(|type_param| type_param.name.ident).collect()
}

fn member_access<'e, 'src>(
    expr: &'e Expression<'src>,
) -> Option<(&'e Expression<'src>, &'e Identifier<'src>)> {
    let Expr::MemoryTarget(memory_target) = &*expr.variant else {
        return None;
    };

    match &memory_target.variant {
        MemTar::FieldAccess { target, field_name, .. } => Some((target, field_name)),
        MemTar::Name(_) | MemTar::ArrayAccess { .. }
        | MemTar::TupleIndex { .. } | MemTar::Dereference(_) => None,
    }
}

pub struct GostTranslator<'a, 'src> {
    pub context: &'a SemanticsContext<'src>,
    pub module: ModuleId,
    temp_counter: Cell<u32>,
    tuple_counter: Cell<u32>,
    closure_counter: Cell<u32>,
}

impl<'a, 'src> GostTranslator<'a, 'src> {
    pub fn new(context: &'a SemanticsContext<'src>, module: ModuleId) -> Self {
        Self {
            context,
            module,
            temp_counter: Cell::new(0),
            tuple_counter: Cell::new(0),
            closure_counter: Cell::new(0),
        }
    }

    fn next_id(counter: &Cell<u32>) -> u32 {
        let id = counter.get();
        counter.set(id + 1);
        id
    }

    fn fresh_if_temp_name(&self) -> &'src str {
        self.context.alloc_str(&format!("__duck_if_{}", Self::next_id(&self.temp_counter)))
    }

    fn fresh_tuple_temp_name(&self) -> &'src str {
        self.context.alloc_str(&format!("__duck_tuple_{}", Self::next_id(&self.tuple_counter)))
    }

    fn fresh_closure_id(&self) -> u32 {
        Self::next_id(&self.closure_counter)
    }

    fn receiver_temp_name(&self, closure: u32) -> &'src str {
        self.context.alloc_str(&format!("__duck_receiver_{closure}"))
    }

    fn closure_param_name(&self, closure: u32, index: usize) -> &'src str {
        self.context.alloc_str(&format!("__duck_arg_{closure}_{index}"))
    }

    fn translate_name_reference(&self, node: NodeId, fallback: &'src str) -> GoExpression<'src> {
        let resolutions = &self.context.modules[self.module.0 as usize].resolutions;
        match resolutions[node.0 as usize] {
            Some(symbol) => GoExpression::Immediate(self.context.symbols[symbol.0 as usize].name),
            None => GoExpression::Immediate(fallback),
        }
    }

    pub fn translate_statement(&self, statement: &Statement<'src>) -> Vec<GoStatement<'src>> {
        match &statement.variant {
            Stmt::FunctionDefinition { name, type_params, params, return_type, body } => {
                vec![GoStatement::FuncDef {
                    receiver: None,
                    name: name.ident,
                    type_params: type_param_names(type_params),
                    params: self.translate_params(params),
                    return_type: self.translate_type_annotation(return_type),
                    body: self.translate_block(body),
                }]
            }
            Stmt::Expression { expr } => {
                match &*expr.variant {
                    Expr::If { expr: condition, body, else_branch } => {
                        let (mut prelude, condition_expr) = self.translate_expression(condition);
                        prelude.push(GoStatement::If {
                            condition: condition_expr,
                            body: self.translate_block(body),
                            else_body: else_branch.as_ref().map(|else_body| self.translate_block(else_body)),
                        });
                        prelude
                    }
                    Expr::While { expr: condition, body } => {
                        let (mut prelude, condition_expr) = self.translate_expression(condition);
                        prelude.push(GoStatement::While {
                            condition: condition_expr,
                            body: self.translate_block(body),
                        });
                        prelude
                    }
                    Expr::FunctionCall { target, args, type_args: _ } if self.go_multi_result_types(target).is_some() => {
                        let (mut prelude, call) = self.translate_call(target, args);
                        prelude.push(GoStatement::Expr { expr: call });
                        prelude
                    }
                    _ => {
                        let (mut prelude, translated) = self.translate_expression(expr);
                        prelude.push(GoStatement::Expr { expr: translated });
                        prelude
                    }
                }
            }
            Stmt::VariableAssignment { target, assign_expression } => {
                let (mut prelude, target_expr) = self.translate_memory_target(target);
                let (value_prelude, value_expr) = self.translate_expression(assign_expression);
                prelude.extend(value_prelude);
                prelude.push(GoStatement::Assign { target: target_expr, expr: value_expr });
                prelude
            }
            Stmt::VariableDeclaration { name, type_, init_expression } => {
                let mut prelude = Vec::new();

                let init = match init_expression {
                    Some(init_expr) => {
                        let (init_prelude, translated) = self.translate_expression(init_expr);
                        prelude.extend(init_prelude);
                        Some(translated)
                    }
                    None => None,
                };

                prelude.push(GoStatement::VarDecl {
                    name: name.ident,
                    type_: self.translate_type_annotation(type_),
                    init_expression: init,
                });

                prelude
            }
            Stmt::StructDefinition { name, type_params, fields, impl_block } => {
                let type_declaration = GoStatement::TypeDecl {
                    name: name.ident,
                    type_params: type_param_names(type_params),
                    type_: GoType::Struct {
                        fields: fields
                            .iter()
                            .map(|field| StructField {
                                name: field.name.ident,
                                tag: None,
                                type_: self.translate_type_expression(
                                    field.type_.annotation.as_ref()
                                        .expect("struct field must have a type annotation by the time typecheck has passed"),
                                ),
                            })
                            .collect::<Vec<_>>()
                    }
                };

                let methods = impl_block
                    .iter()
                    .flat_map(|impl_block| &impl_block.methods)
                    .map(|method| self.translate_method(name.ident, type_params, method));

                std::iter::once(type_declaration).chain(methods).collect()
            }
            Stmt::Return { value } => {
                match value {
                    Some(value_expr) => {
                        let (mut prelude, translated) = self.translate_expression(value_expr);
                        prelude.push(GoStatement::Return { value: Some(translated) });

                        prelude
                    }
                    None => vec![GoStatement::Return { value: None }],
                }
            }
            Stmt::Break => vec![GoStatement::Break],
            Stmt::Continue => vec![GoStatement::Continue],
            Stmt::Use(_) => unreachable!("Stmt::Use should already be filtered out before translation, see gost::translate"),
        }
    }

    fn translate_method(
        &self,
        struct_name: &'src str,
        struct_type_params: &[TypeParam<'src>],
        method: &Method<'src>,
    ) -> GoStatement<'src> {
        let struct_type = GoType::Named {
            name: struct_name,
            type_args: type_param_names(struct_type_params)
                .into_iter()
                .map(|name| GoType::Named { name, type_args: vec![] })
                .collect(),
        };

        let stays_a_method = matches!(method.kind, MethodKind::Instance) && method.type_params.is_empty();

        if stays_a_method {
            return GoStatement::FuncDef {
                receiver: Some((SELF_NAME, GoType::Pointer(Box::new(struct_type)))),
                name: method.name.ident,
                type_params: vec![],
                params: self.translate_params(&method.params),
                return_type: self.translate_type_annotation(&method.return_type),
                body: self.translate_block(&method.body),
            };
        }

        let mut params = Vec::with_capacity(method.params.list.len() + 1);
        if matches!(method.kind, MethodKind::Instance) {
            params.push((SELF_NAME, GoType::Pointer(Box::new(struct_type))));
        }
        params.extend(self.translate_params(&method.params));

        let mut type_params = type_param_names(struct_type_params);
        type_params.extend(type_param_names(&method.type_params));

        GoStatement::FuncDef {
            receiver: None,
            name: self.context.free_function_method_name(struct_name, method.name.ident),
            type_params,
            params,
            return_type: self.translate_type_annotation(&method.return_type),
            body: self.translate_block(&method.body),
        }
    }

    fn free_function_member(&self, node: NodeId) -> Option<&'a FreeFunctionMember<'src>> {
        self.context.modules[self.module.0 as usize].free_function_members.get(&node)
    }

    fn type_arguments(&self, node: NodeId) -> Vec<GoType<'src>> {
        self.context.modules[self.module.0 as usize]
            .type_arguments
            .get(&node)
            .map(|arguments| arguments.iter().map(|argument| self.go_type_from_type_id(*argument)).collect())
            .unwrap_or_default()
    }

    fn instantiated(&self, base: GoExpression<'src>, node: NodeId) -> GoExpression<'src> {
        let type_args = self.type_arguments(node);

        match type_args.is_empty() {
            true => base,
            false => GoExpression::Instantiate { base: Box::new(base), type_args },
        }
    }

    fn translate_type_annotation(&self, type_annotation: &TypeAnnotation<'src>) -> Option<GoType<'src>> {
        type_annotation.annotation.as_ref().map(|type_expr| self.translate_type_expression(type_expr))
    }

    fn translate_type_expression(&self, expr: &TypeExpression<'src>) -> GoType<'src> {
        match expr {
            TypeExpression::String => GoType::String,
            TypeExpression::Int => GoType::Int,
            TypeExpression::Int8 => GoType::Int8,
            TypeExpression::Int16 => GoType::Int16,
            TypeExpression::Int32 => GoType::Int32,
            TypeExpression::Int64 => GoType::Int64,
            TypeExpression::Uint => GoType::Uint,
            TypeExpression::Uint8 => GoType::Uint8,
            TypeExpression::Uint16 => GoType::Uint16,
            TypeExpression::Uint32 => GoType::Uint32,
            TypeExpression::Uint64 => GoType::Uint64,
            TypeExpression::Float => GoType::Float64,
            TypeExpression::Float32 => GoType::Float32,
            TypeExpression::Bool => GoType::Bool,
            TypeExpression::Array { inner } => GoType::Array(Box::new(self.translate_type_expression(inner))),
            TypeExpression::Pointer { inner } => GoType::Pointer(Box::new(self.translate_type_expression(inner))),
            TypeExpression::Tuple(elements) => GoType::Tuple(
                elements.iter().map(|element| self.translate_type_expression(element)).collect(),
            ),
            TypeExpression::Ident { name, type_args } => GoType::Named {
                name: name.ident,
                type_args: type_args.iter().map(|type_arg| self.translate_type_expression(type_arg)).collect(),
            },
        }
    }

    fn node_type(&self, node: NodeId) -> TypeId {
        self.context.modules[self.module.0 as usize].node_types[node.0 as usize]
            .expect("expression should be typechecked before translation")
    }

    fn go_type_from_type_id(&self, type_id: TypeId) -> GoType<'src> {
        match &self.context.types[type_id.0 as usize] {
            Type::Int => GoType::Int,
            Type::Int8 => GoType::Int8,
            Type::Int16 => GoType::Int16,
            Type::Int32 => GoType::Int32,
            Type::Int64 => GoType::Int64,
            Type::Uint => GoType::Uint,
            Type::Uint8 => GoType::Uint8,
            Type::Uint16 => GoType::Uint16,
            Type::Uint32 => GoType::Uint32,
            Type::Uint64 => GoType::Uint64,
            Type::Float => GoType::Float64,
            Type::Float32 => GoType::Float32,
            Type::Bool => GoType::Bool,
            Type::String => GoType::String,
            Type::Array(inner) => GoType::Array(Box::new(self.go_type_from_type_id(*inner))),
            Type::Pointer(inner) => GoType::Pointer(Box::new(self.go_type_from_type_id(*inner))),
            Type::Tuple(elements) => GoType::Tuple(
                elements.iter().map(|element| self.go_type_from_type_id(*element)).collect(),
            ),
            Type::Struct(symbol, arguments) => GoType::Named {
                name: self.context.symbols[symbol.0 as usize].name,
                type_args: arguments.iter().map(|argument| self.go_type_from_type_id(*argument)).collect(),
            },
            Type::TypeParam(type_param) => GoType::Named {
                name: self.context.type_param_name(*type_param),
                type_args: vec![],
            },
            Type::Unit | Type::Never => GoType::Struct { fields: vec![] },
            Type::Fn { params, return_type } => {
                let return_type = *return_type;
                GoType::Func {
                    params: params.iter().map(|param| self.go_type_from_type_id(*param)).collect(),
                    return_type: match &self.context.types[return_type.0 as usize] {
                        Type::Unit => None,
                        _ => Some(Box::new(self.go_type_from_type_id(return_type))),
                    },
                }
            }
            Type::TypeError => unreachable!(
                "type error should always come with diagnostic and should be stopped by drivber"
            ),
        }
    }

    fn translate_params(&self, params: &ParameterList<'src>) -> Vec<(&'src str, GoType<'src>)> {
        params
            .list
            .iter()
            .map(|param| {
                (param.name.ident, self.translate_type_annotation(&param.type_).unwrap())
            })
            .collect()
    }

    fn translate_block(&self, body: &Block<'src>) -> Vec<GoStatement<'src>> {
        body
            .statements
            .iter()
            .flat_map(|stmt| self.translate_statement(&stmt))
            .collect()
    }
    fn translate_block_as_value(&self, block: &Block<'src>, target_name: &'src str) -> Vec<GoStatement<'src>> {
        let mut go_statements = Vec::new();

        for (index, statement) in block.statements.iter().enumerate() {
            let is_last = index == block.statements.len() - 1;

            if is_last {
                if let Stmt::Expression { expr } = &statement.variant {
                    let (prelude, value) = self.translate_expression(expr);

                    go_statements.extend(prelude);
                    go_statements.push(GoStatement::Assign {
                        target: GoExpression::Immediate(target_name),
                        expr: value,
                    });

                    continue;
                }

                if matches!(
                    statement.variant,
                    Stmt::Return { .. } | Stmt::Break | Stmt::Continue
                ) {
                    go_statements.extend(self.translate_statement(statement));
                    continue;
                }
            }

            go_statements.extend(self.translate_statement(statement));
        }

        go_statements
    }

    fn translate_expression(&self, expr: &Expression<'src>) -> (Vec<GoStatement<'src>>, GoExpression<'src>) {
        match &*expr.variant {
            Expr::StringLiteral(str) => {
                (vec![], GoExpression::String(str))
            },
            Expr::IntLiteral(value) => {
                (vec![], GoExpression::Int(*value))
            },
            Expr::FloatLiteral(value) => {
                (vec![], GoExpression::Float64(*value))
            },
            Expr::BoolLiteral(value) => {
                (vec![], GoExpression::Bool(*value))
            },
            Expr::Binary { left, op, right } => {
                let (mut prelude, left_expr) = self.translate_expression(left);
                let (right_prelude, right_expr) = self.translate_expression(right);

                prelude.extend(right_prelude);

                (prelude, GoExpression::BinaryOp {
                    left: Box::new(left_expr),
                    op: *op,
                    right: Box::new(right_expr),
                })
            },
            Expr::Unary { op, expr: inner } => {
                let (prelude, inner_expr) = self.translate_expression(inner);
                (prelude, GoExpression::UnaryOp { op: *op, expr: Box::new(inner_expr) })
            },
            Expr::Reference { expr: inner } => {
                let (prelude, inner_expr) = self.translate_expression(inner);
                (prelude, GoExpression::AddressOf(Box::new(inner_expr)))
            },
            Expr::FunctionCall { target, type_args: _, args } => {
                let (mut prelude, call) = self.translate_call(target, args);

                let Some(result_types) = self.go_multi_result_types(target) else {
                    return (prelude, call);
                };

                let names = result_types.iter().map(|_| self.fresh_tuple_temp_name()).collect::<Vec<_>>();
                let values = names.iter().map(|name| GoExpression::Immediate(*name)).collect::<Vec<_>>();

                prelude.push(GoStatement::MultiVarDecl { names, expr: call });

                (prelude, GoExpression::TupleInit { elem_types: result_types, values })
            },
            Expr::MemoryTarget(memory_target) => {
                self.translate_memory_target(memory_target)
            },
            Expr::ArrayExpression { values_exprs } => {
                let elem_type_id = match &self.context.types[self.node_type(expr.id).0 as usize] {
                    Type::Array(inner) => *inner,
                    case => unreachable!("array expression should have array type, found {:?}", case),
                };

                let mut prelude = Vec::new();

                let mut values = Vec::new();
                for value in values_exprs {
                    let (value_prelude, value_expr) = self.translate_expression(value);
                    prelude.extend(value_prelude);
                    values.push(value_expr);
                }

                (prelude, GoExpression::Array {
                    elem_type: self.go_type_from_type_id(elem_type_id),
                    values,
                })
            },
            Expr::TupleExpression { values } => {
                let elem_types = match self.go_type_from_type_id(self.node_type(expr.id)) {
                    GoType::Tuple(elem_types) => elem_types,
                    case => unreachable!("tuple expression should have tuple type, found {:?}", case),
                };

                let mut prelude = Vec::new();

                let mut translated_values = Vec::new();
                for value in values {
                    let (value_prelude, value_expr) = self.translate_expression(value);
                    prelude.extend(value_prelude);
                    translated_values.push(value_expr);
                }

                (prelude, GoExpression::TupleInit { elem_types, values: translated_values })
            },
            Expr::StructInit { type_name, type_args: _, fields } => {
                let mut prelude = Vec::new();

                let mut translated_fields = Vec::new();
                for field in fields {
                    let (field_prelude, field_expr) = self.translate_expression(&field.value);
                    prelude.extend(field_prelude);
                    translated_fields.push((field.name.ident, field_expr));
                }

                let type_args = match self.go_type_from_type_id(self.node_type(expr.id)) {
                    GoType::Named { type_args, .. } => type_args,
                    case => unreachable!("struct init should have a named type, found {:?}", case),
                };

                (prelude, GoExpression::StructInit {
                    type_name: type_name.ident,
                    type_args,
                    fields: translated_fields,
                })
            },
            Expr::If { expr: condition, body, else_branch } => {
                let temp_name = self.fresh_if_temp_name();
                let go_type = self.go_type_from_type_id(self.node_type(expr.id));

                let (mut prelude, condition_expr) = self.translate_expression(condition);

                let then_statements = self.translate_block_as_value(body, temp_name);
                let else_statements = else_branch.as_ref()
                    .map(|else_body| self.translate_block_as_value(else_body, temp_name));

                prelude.push(GoStatement::VarDecl {
                    name: temp_name,
                    type_: Some(go_type),
                    init_expression: None
                });

                prelude.push(GoStatement::If {
                    condition: condition_expr,
                    body: then_statements,
                    else_body: else_statements,
                });

                (prelude, GoExpression::Immediate(temp_name))
            },
            Expr::While { expr: condition, body } => {
                let (mut prelude, condition_expr) = self.translate_expression(condition);

                prelude.push(GoStatement::While {
                    condition: condition_expr,
                    body: self.translate_block(body),
                });

                (prelude, GoExpression::StructInit { type_name: "struct{}", type_args: vec![], fields: vec![] })
            },
        }
    }

    fn translate_call(
        &self,
        target: &Expression<'src>,
        args: &ExpressionList<'src>,
    ) -> (Vec<GoStatement<'src>>, GoExpression<'src>) {
        let (mut prelude, call_target, receiver) = self.translate_target(target);
        let (args_prelude, arg_exprs) = self.translate_expression_list(args);

        prelude.extend(args_prelude);

        (prelude, GoExpression::FuncCall {
            target: Box::new(call_target),
            args: receiver.into_iter().chain(arg_exprs).collect(),
        })
    }

    fn translate_target(
        &self,
        target: &Expression<'src>,
    ) -> (Vec<GoStatement<'src>>, GoExpression<'src>, Option<GoExpression<'src>>) {
        let member = member_access(target).and_then(|(receiver_target, field_name)| {
            let member = self.free_function_member(field_name.id)?;
            Some((receiver_target, field_name, member))
        });

        let Some((receiver_target, field_name, member)) = member else {
            let (prelude, call_target) = self.translate_expression(target);
            return (prelude, call_target, None);
        };

        let call_target = self.instantiated(GoExpression::Immediate(member.name), field_name.id);

        let Some(receiver_type) = member.receiver_type else {
            return (vec![], call_target, None);
        };

        let (prelude, receiver) = self.translate_expression(receiver_target);

        (prelude, call_target, Some(self.receiver_argument(receiver, receiver_type)))
    }

    fn receiver_argument(&self, receiver: GoExpression<'src>, receiver_type: TypeId) -> GoExpression<'src> {
        match self.context.types[receiver_type.0 as usize] {
            Type::Pointer(_) => receiver,
            _ => GoExpression::AddressOf(Box::new(receiver)),
        }
    }

    fn receiver_pointer_type(&self, receiver_type: TypeId) -> GoType<'src> {
        match self.context.types[receiver_type.0 as usize] {
            Type::Pointer(_) => self.go_type_from_type_id(receiver_type),
            _ => GoType::Pointer(Box::new(self.go_type_from_type_id(receiver_type))),
        }
    }

    fn translate_member_closure(
        &self,
        receiver_target: &Expression<'src>,
        field_name: &Identifier<'src>,
        member: &FreeFunctionMember<'src>,
        receiver_type: TypeId,
    ) -> (Vec<GoStatement<'src>>, GoExpression<'src>) {
        let Type::Fn { params, return_type } = &self.context.types[member.value_type.0 as usize] else {
            unreachable!("a method value should have a function type, found {:?}", self.context.types[member.value_type.0 as usize]);
        };

        let closure = self.fresh_closure_id();
        let receiver_name = self.receiver_temp_name(closure);

        let (mut prelude, receiver) = self.translate_expression(receiver_target);
        prelude.push(GoStatement::VarDecl {
            name: receiver_name,
            type_: Some(self.receiver_pointer_type(receiver_type)),
            init_expression: Some(self.receiver_argument(receiver, receiver_type)),
        });

        let closure_params = params
            .iter()
            .enumerate()
            .map(|(index, param)| (self.closure_param_name(closure, index), self.go_type_from_type_id(*param)))
            .collect::<Vec<_>>();

        let call = GoExpression::FuncCall {
            target: Box::new(self.instantiated(GoExpression::Immediate(member.name), field_name.id)),
            args: std::iter::once(GoExpression::Immediate(receiver_name))
                .chain(closure_params.iter().map(|(name, _)| GoExpression::Immediate(name)))
                .collect(),
        };

        let returns_value = !matches!(self.context.types[return_type.0 as usize], Type::Unit);

        (prelude, GoExpression::FuncLiteral {
            params: closure_params,
            return_type: returns_value.then(|| self.go_type_from_type_id(*return_type)),
            body: match returns_value {
                true => vec![GoStatement::Return { value: Some(call) }],
                false => vec![GoStatement::Expr { expr: call }],
            },
        })
    }

    fn go_multi_result_types(&self, target: &Expression<'src>) -> Option<Vec<GoType<'src>>> {
        let Expr::MemoryTarget(memory_target) = &*target.variant else {
            return None;
        };

        let MemTar::FieldAccess { target: package_target, .. } = &memory_target.variant else {
            return None;
        };

        let Expr::MemoryTarget(package_place) = &*package_target.variant else {
            return None;
        };

        let MemTar::Name(identifier) = &package_place.variant else {
            return None;
        };

        let resolutions = &self.context.modules[self.module.0 as usize].resolutions;
        let symbol = resolutions[identifier.id.0 as usize]?;

        if !matches!(self.context.symbols[symbol.0 as usize].origin, Origin::GoPackage { .. }) {
            return None;
        }

        let Type::Fn { return_type, .. } = &self.context.types[self.node_type(target.id).0 as usize] else {
            return None;
        };

        let Type::Tuple(elements) = &self.context.types[return_type.0 as usize] else {
            return None;
        };

        Some(elements.iter().map(|element| self.go_type_from_type_id(*element)).collect())
    }

    fn translate_memory_target(&self, memory_target: &MemoryTarget<'src>) -> (Vec<GoStatement<'src>>, GoExpression<'src>) {
        match &memory_target.variant {
            MemTar::Name(identifier) => {
                let reference = self.translate_name_reference(identifier.id, identifier.ident);
                (vec![], self.instantiated(reference, identifier.id))
            }
            MemTar::FieldAccess { target, field_name, type_args: _ } => {
                let Some(member) = self.free_function_member(field_name.id) else {
                    let (prelude, base) = self.translate_expression(target);
                    return (prelude, GoExpression::Selector { base: Box::new(base), field: field_name.ident });
                };

                let Some(receiver_type) = member.receiver_type else {
                    return (vec![], self.instantiated(GoExpression::Immediate(member.name), field_name.id));
                };

                self.translate_member_closure(target, field_name, member, receiver_type)
            }
            MemTar::ArrayAccess { target, index_expression } => {
                let (mut prelude, base) = self.translate_expression(target);
                let (index_prelude, index) = self.translate_expression(index_expression);
                prelude.extend(index_prelude);

                (prelude, GoExpression::ArrayIndex { base: Box::new(base), index: Box::new(index) })
            }
            MemTar::TupleIndex { target, index } => {
                let (prelude, base) = self.translate_expression(target);
                let field = self.context.alloc_str(&tuple_field_name(*index));
                (prelude, GoExpression::Selector { base: Box::new(base), field })
            }
            MemTar::Dereference(inner) => {
                let (prelude, inner_expr) = self.translate_expression(inner);
                (prelude, GoExpression::Dereference(Box::new(inner_expr)))
            }
        }
    }

    fn translate_expression_list(&self, expr_list: &ExpressionList<'src>) -> (Vec<GoStatement<'src>>, Vec<GoExpression<'src>>) {
        let mut prelude = Vec::new();
        let mut values = Vec::new();

        for expr in &expr_list.list {
            let (expr_prelude, value) = self.translate_expression(expr);
            prelude.extend(expr_prelude);
            values.push(value);
        }

        (prelude, values)
    }
}
