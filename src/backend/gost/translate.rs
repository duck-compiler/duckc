use crate::{ast::{Block, Expression, MemoryTarget, NodeId, ParameterList, Statement, TypeExpression, expression::{Expr, ExpressionList}, memory_target::MemTar, statement::Stmt, type_expression::TypeAnnotation}, backend::{gost::go_tree::{GoExpression, GoStatement, GoType, StructField}, semantics::{context::SemanticsContext, module::ModuleId, r#type::{Type, TypeId}}}};

pub struct Translator<'a, 'src> {
    pub context: &'a SemanticsContext<'src>,
    pub module: ModuleId
}

impl<'a, 'src> Translator<'a, 'src> {
    fn translate_name_reference(&self, node: NodeId, fallback: &'src str) -> GoExpression<'src> {
        let resolutions = &self.context.modules[self.module.0 as usize].resolutions;
        match resolutions[node.0 as usize] {
            Some(symbol) => GoExpression::Immediate(self.context.symbols[symbol.0 as usize].name),
            None => GoExpression::Immediate(fallback),
        }
    }

    pub fn translate_statement(&self, statement: &Statement<'src>) -> GoStatement<'src> {
        match &statement.variant {
            Stmt::FunctionDefinition { name, params, return_type, body } => {
                GoStatement::FuncDef {
                    name: name.ident,
                    params: self.translate_params(params),
                    return_type: self.translate_type_annotation(return_type),
                    body: self.translate_block(body)
                }
            }
            Stmt::Expression { expr } => {
                GoStatement::Expr { expr: self.translate_expression(expr) }
            }
            Stmt::VariableDeclaration { name, type_, init_expression } => {
                GoStatement::VarDecl {
                    name: name.ident,
                    type_: self.translate_type_annotation(type_),
                    init_expression: if init_expression.is_some() {
                        Some(self.translate_expression(init_expression.as_ref().expect("should never be none")))
                    } else {
                        None
                    },
                }
            }
            Stmt::StructDefinition { name, fields } => {
                GoStatement::TypeDecl {
                    name: name.ident,
                    type_: GoType::Struct {
                        fields: fields
                            .list
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
                }
            }
            case => {
                unimplemented!("translate_statement: {:?}", case)
            }
        }
    }

    fn translate_type_annotation(&self, type_annotation: &TypeAnnotation<'src>) -> Option<GoType<'src>> {
        type_annotation.annotation.as_ref().map(|type_expr| self.translate_type_expression(type_expr))
    }

    fn translate_type_expression(&self, expr: &TypeExpression<'src>) -> GoType<'src> {
        match expr {
            TypeExpression::String => GoType::String,
            TypeExpression::Int => GoType::Int,
            TypeExpression::Float => GoType::Float64,
            TypeExpression::Bool => GoType::Bool,
            TypeExpression::Array { inner } => GoType::Array(Box::new(self.translate_type_expression(inner))),
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
            Type::Float => GoType::Float64,
            Type::Bool => GoType::Bool,
            Type::String => GoType::String,
            Type::Array(inner) => GoType::Array(Box::new(self.go_type_from_type_id(*inner))),
            Type::Struct(sym) => GoType::TypeName(self.context.symbols[sym.0 as usize].name),
            case => unimplemented!("go_type_from_type_id: {:?}", case),
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
            .map(|stmt| self.translate_statement(&stmt))
            .collect()
    }

    fn translate_expression(&self, expr: &Expression<'src>) -> GoExpression<'src> {
        match &*expr.variant {
            Expr::StringLiteral(str) => {
                GoExpression::String(str)
            },
            Expr::FunctionCall { target, args } => {
                GoExpression::FuncCall {
                    callee: Box::new(self.translate_expression(target)),
                    args: self.translate_expression_list(args)
                }
            },
            Expr::MemoryTarget(memory_target) => {
                self.translate_memory_target(memory_target)
            },
            Expr::ArrayExpression { values_exprs } => {
                let elem_type_id = match &self.context.types[self.node_type(expr.id).0 as usize] {
                    Type::Array(inner) => *inner,
                    case => unreachable!("array expression should have array type, found {:?}", case),
                };

                GoExpression::Array {
                    elem_type: self.go_type_from_type_id(elem_type_id),
                    values: values_exprs.iter().map(|value| self.translate_expression(value)).collect(),
                }
            },
            Expr::StructInit { type_name, fields } => {
                GoExpression::StructInit {
                    type_name: type_name.ident,
                    fields: fields.iter().map(|field| (field.name.ident, self.translate_expression(&field.value))).collect(),
                }
            },
            case => unimplemented!("translate_expression: {:?}", case)
        }
    }

    fn translate_memory_target(&self, memory_target: &MemoryTarget<'src>) -> GoExpression<'src> {
        match &memory_target.variant {
            MemTar::Name(identifier) => {
                self.translate_name_reference(identifier.id, identifier.ident)
            }
            MemTar::FieldAccess { target, field_name } => {
                GoExpression::Selector {
                    base: Box::new(self.translate_memory_target(target)),
                    field: field_name.ident,
                }
            }
            MemTar::ArrayAccess { target, index_expression } => {
                GoExpression::ArrayIndex {
                    base: Box::new(self.translate_memory_target(target)),
                    index: Box::new(self.translate_expression(index_expression)),
                }
            }
            case => unimplemented!("translate_memory_target: {:?}", case)
        }
    }

    fn translate_expression_list(&self, expr_list: &ExpressionList<'src>) -> Vec<GoExpression<'src>> {
        expr_list
            .list
            .iter()
            .map(|expr| self.translate_expression(expr))
            .collect()
    }
}
