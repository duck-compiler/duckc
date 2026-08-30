use serde::{Deserialize, Serialize};

use crate::ast::{AstRoot, Block, Expr, Expression, Identifier, MemoryTarget, ParameterList, Statement, Stmt, TypeExpression, expression::{ExpressionList, FieldInit}, memory_target::MemTar, struct_definition::Method, type_expression::{TypeAnnotation, TypeParam}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct NodeId(pub u32);

impl NodeId {
    pub const DUMMY: NodeId = NodeId(u32::MAX);
}

pub fn assign_generate_node_ids(ast: &mut AstRoot) -> usize {
    let mut generator = NodeIdGenerator::new();
    for statement in &mut ast.statements {
        generator.generate_in_statement(statement);
    }

    generator.count()
}

pub struct NodeIdGenerator {
    next_id: u32,
}

impl NodeIdGenerator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn fresh(&mut self) -> NodeId {
        let id = NodeId(self.next_id);
        self.next_id += 1;
        id
    }

    pub fn count(&self) -> usize {
        self.next_id as usize
    }

    fn generate_in_statement(&mut self, statement: &mut Statement) {
        statement.id = self.fresh();
        match &mut statement.variant {
            Stmt::FunctionDefinition { name, type_params, params, body, return_type } => {
                self.generate_in_identifier(name);
                self.generate_in_type_params(type_params);
                self.generate_in_parameter_list(params);
                self.generate_in_block(body);
                self.generate_in_type_annotation(return_type);
            },
            Stmt::StructDefinition { name, type_params, fields, impl_block } => {
                self.generate_in_identifier(name);
                self.generate_in_type_params(type_params);

                for field in fields {
                    self.generate_in_type_annotation(&mut field.type_);
                    self.generate_in_identifier(&mut field.name);
                }

                if let Some(impl_block) = impl_block {
                    for method in &mut impl_block.methods {
                        self.generate_in_method(method);
                    }
                }
            },
            Stmt::Expression { expr } => {
                self.generate_in_expression(expr);
            },
            Stmt::VariableDeclaration { name, type_, init_expression } => {
                self.generate_in_identifier(name);
                self.generate_in_type_annotation(type_);
                if let Some(init_expression) = init_expression {
                    self.generate_in_expression(init_expression);
                }
            },
            Stmt::VariableAssignment { target, assign_expression } => {
                self.generate_in_memory_target(target);
                self.generate_in_expression(assign_expression);
            }
            Stmt::Use(use_statement) => {
                self.generate_in_identifier(&mut use_statement.path);
                if let Some(alias) = &mut use_statement.alias {
                    self.generate_in_identifier(alias);
                }
            }
            Stmt::Return { value } => {
                if let Some(value) = value {
                    self.generate_in_expression(value);
                }
            }
            Stmt::Break | Stmt::Continue => {}
        }
    }

    fn generate_in_method(&mut self, method: &mut Method) {
        self.generate_in_identifier(&mut method.name);
        self.generate_in_type_params(&mut method.type_params);
        self.generate_in_parameter_list(&mut method.params);
        self.generate_in_block(&mut method.body);
        self.generate_in_type_annotation(&mut method.return_type);
    }

    fn generate_in_identifier(&mut self, identifier: &mut Identifier) {
        identifier.id = self.fresh();
    }

    fn generate_in_type_params(&mut self, type_params: &mut Vec<TypeParam>) {
        for type_param in type_params {
            self.generate_in_identifier(&mut type_param.name);
        }
    }

    fn generate_in_type_args(&mut self, type_args: &mut Vec<TypeExpression>) {
        for type_arg in type_args {
            self.generate_in_type_expr(type_arg);
        }
    }

    fn generate_in_type_annotation(&mut self, type_: &mut TypeAnnotation) {
        if let Some(type_) = &mut type_.annotation {
            self.generate_in_type_expr(type_);
        }
    }

    fn generate_in_type_expr(&mut self, type_: &mut TypeExpression) {
        match type_ {
            TypeExpression::Ident { name, type_args } => {
                self.generate_in_identifier(name);
                self.generate_in_type_args(type_args);
            },
            TypeExpression::Array { inner } | TypeExpression::Pointer { inner } => {
                self.generate_in_type_expr(inner);
            },
            TypeExpression::Tuple(elements) => {
                for element in elements {
                    self.generate_in_type_expr(element);
                }
            },
            TypeExpression::String | TypeExpression::Bool
            | TypeExpression::Int | TypeExpression::Int8
            | TypeExpression::Int16 | TypeExpression::Int32
            | TypeExpression::Int64 | TypeExpression::Uint
            | TypeExpression::Uint8 | TypeExpression::Uint16
            | TypeExpression::Uint32 | TypeExpression::Uint64
            | TypeExpression::Float | TypeExpression::Float32 => {},
        }
    }

    fn generate_in_parameter_list(&mut self, params: &mut ParameterList) {
        for param in &mut params.list {
            self.generate_in_type_annotation(&mut param.type_);
            self.generate_in_identifier(&mut param.name);
        }
    }

    fn generate_in_block(&mut self, block: &mut Block) {
        for statement in &mut block.statements {
            self.generate_in_statement(statement);
        }
    }

    fn generate_in_memory_target(&mut self, target: &mut MemoryTarget) {
        match &mut target.variant {
            MemTar::Name(ident) => {
                self.generate_in_identifier(ident);
            },
            MemTar::ArrayAccess { target, index_expression } => {
                self.generate_in_memory_target(target);
                self.generate_in_expression(index_expression);
            }
            MemTar::Dereference(expr) => {
                self.generate_in_expression(expr);
            }
            MemTar::FieldAccess { target, field_name, type_args } => {
                self.generate_in_memory_target(target);
                self.generate_in_identifier(field_name);
                self.generate_in_type_args(type_args);
            }
            MemTar::TupleIndex { target, index: _ } => {
                self.generate_in_memory_target(target);
            }
        }
    }

    fn generate_in_expression_list(&mut self, exprs: &mut ExpressionList) {
        for expr in &mut exprs.list {
            self.generate_in_expression(expr);
        }
    }

    fn generate_in_expression(&mut self, expr: &mut Expression) {
        expr.id = self.fresh();
        match &mut *expr.variant {
            Expr::Binary { left, op: _, right } => {
                self.generate_in_expression(left);
                self.generate_in_expression(right);
            },
            Expr::Unary { op: _, expr } => {
                self.generate_in_expression(expr);
            },
            Expr::Reference { expr } => {
                self.generate_in_expression(expr);
            },
            Expr::While { expr, body } => {
                self.generate_in_expression(expr);
                self.generate_in_block(body);
            },
            Expr::MemoryTarget(memory_target) => {
                self.generate_in_memory_target(memory_target);
            }
            Expr::FunctionCall { target, type_args, args } => {
                self.generate_in_expression(target);
                self.generate_in_type_args(type_args);
                self.generate_in_expression_list(args);
            }
            Expr::If { expr, body, else_branch } => {
                self.generate_in_expression(expr);
                self.generate_in_block(body);
                if let Some(else_branch) = else_branch {
                    self.generate_in_block(else_branch);
                }
            }
            Expr::ArrayExpression { values_exprs } => {
                for expr in values_exprs {
                    self.generate_in_expression(expr);
                }
            }
            Expr::TupleExpression { values } => {
                for expr in values {
                    self.generate_in_expression(expr);
                }
            }
            Expr::StructInit { type_name, type_args, fields } => {
                self.generate_in_identifier(type_name);
                self.generate_in_type_args(type_args);
                for field in fields {
                    self.generate_in_field_init(field);
                }
            }
            | Expr::StringLiteral(..) | Expr::BoolLiteral(..)
            | Expr::IntLiteral(..) | Expr::FloatLiteral(..) => {}
        }
    }

    fn generate_in_field_init(&mut self, field: &mut FieldInit) {
        self.generate_in_identifier(&mut field.name);
        self.generate_in_expression(&mut field.value);
    }
}
