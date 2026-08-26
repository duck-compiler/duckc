use std::cell::Cell;

use crate::{ast::{Block, Expression, MemoryTarget, NodeId, ParameterList, Statement, TypeExpression, expression::{Expr, ExpressionList}, memory_target::MemTar, statement::Stmt, struct_definition::{Method, MethodKind, SELF_NAME}, type_expression::TypeAnnotation}, backend::{gost::go_tree::{GoExpression, GoStatement, GoType, StructField}, semantics::{context::SemanticsContext, module::ModuleId, symbol::static_method_name, r#type::{Type, TypeId}}}};

fn leak<'src>(s: String) -> &'src str {
    Box::leak(s.into_boxed_str())
}

pub struct Translator<'a, 'src> {
    pub context: &'a SemanticsContext<'src>,
    pub module: ModuleId,
    temp_counter: Cell<u32>,
}

impl<'a, 'src> Translator<'a, 'src> {
    pub fn new(context: &'a SemanticsContext<'src>, module: ModuleId) -> Self {
        Self { context, module, temp_counter: Cell::new(0) }
    }

    fn fresh_temp_name(&self) -> &'src str {
        let id = self.temp_counter.get();
        self.temp_counter.set(id + 1);
        leak(format!("__duck_if_{id}"))
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
            Stmt::FunctionDefinition { name, params, return_type, body } => {
                vec![GoStatement::FuncDef {
                    receiver: None,
                    name: name.ident,
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
            Stmt::StructDefinition { name, fields, impl_block } => {
                let type_declaration = GoStatement::TypeDecl {
                    name: name.ident,
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
                    .map(|method| self.translate_method(name.ident, method));

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

    fn translate_method(&self, struct_name: &'src str, method: &Method<'src>) -> GoStatement<'src> {
        let (receiver, name) = match method.kind {
            MethodKind::Instance => (
                Some((SELF_NAME, GoType::Pointer(Box::new(GoType::TypeName(struct_name))))),
                method.name.ident,
            ),
            MethodKind::Static => (None, static_method_name(struct_name, method.name.ident)),
        };

        GoStatement::FuncDef {
            receiver,
            name,
            params: self.translate_params(&method.params),
            return_type: self.translate_type_annotation(&method.return_type),
            body: self.translate_block(&method.body),
        }
    }

    fn static_method_owner_name(&self, target: &MemoryTarget<'src>, method_name: &str) -> Option<&'src str> {
        let MemTar::Name(identifier) = &target.variant else {
            return None;
        };

        let resolutions = &self.context.modules[self.module.0 as usize].resolutions;
        let symbol = resolutions[identifier.id.0 as usize]?;
        let signature = self.context.struct_methods.get(&symbol)?.get(method_name)?;

        matches!(signature.kind, MethodKind::Static).then(|| self.context.symbols[symbol.0 as usize].name)
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
            TypeExpression::Ident(identifier) => GoType::TypeName(identifier.ident),
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
            Type::Struct(sym) => GoType::TypeName(self.context.symbols[sym.0 as usize].name),
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
            Expr::FunctionCall { target, args } => {
                let (mut prelude, callee_expr) = self.translate_expression(target);
                let (args_prelude, arg_exprs) = self.translate_expression_list(args);

                prelude.extend(args_prelude);

                (prelude, GoExpression::FuncCall {
                    callee: Box::new(callee_expr),
                    args: arg_exprs,
                })
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
            Expr::StructInit { type_name, fields } => {
                let mut prelude = Vec::new();

                let mut translated_fields = Vec::new();
                for field in fields {
                    let (field_prelude, field_expr) = self.translate_expression(&field.value);
                    prelude.extend(field_prelude);
                    translated_fields.push((field.name.ident, field_expr));
                }

                (prelude, GoExpression::StructInit {
                    type_name: type_name.ident,
                    fields: translated_fields,
                })
            },
            Expr::If { expr: condition, body, else_branch } => {
                let temp_name = self.fresh_temp_name();
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

                (prelude, GoExpression::StructInit { type_name: "struct{}", fields: vec![] })
            },
        }
    }

    fn translate_memory_target(&self, memory_target: &MemoryTarget<'src>) -> (Vec<GoStatement<'src>>, GoExpression<'src>) {
        match &memory_target.variant {
            MemTar::Name(identifier) => {
                (vec![], self.translate_name_reference(identifier.id, identifier.ident))
            }
            MemTar::FieldAccess { target, field_name } => {
                if let Some(struct_name) = self.static_method_owner_name(target, field_name.ident) {
                    return (vec![], GoExpression::Immediate(static_method_name(struct_name, field_name.ident)));
                }

                let (prelude, base) = self.translate_memory_target(target);
                (prelude, GoExpression::Selector { base: Box::new(base), field: field_name.ident })
            }
            MemTar::ArrayAccess { target, index_expression } => {
                let (mut prelude, base) = self.translate_memory_target(target);
                let (index_prelude, index) = self.translate_expression(index_expression);
                prelude.extend(index_prelude);

                (prelude, GoExpression::ArrayIndex { base: Box::new(base), index: Box::new(index) })
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
