use serde::{Deserialize, Serialize};

use crate::ast::{AstRoot, Block, Expr, Expression, Identifier, MemoryTarget, ParameterList, Statement, Stmt, TypeExpression, expression::ExpressionList, memory_target::MemTar, type_expression::TypeAnnotation};

#[derive(Debug, Clone, Copy, Eq, Hash, Deserialize, Serialize)]
pub struct NodeId(pub u32);

impl NodeId {
    pub const DUMMY: NodeId = NodeId(u32::MAX);
}

impl PartialEq for NodeId {
    fn eq(&self, other: &Self) -> bool {
        if cfg!(test) && std::env::var("DUCKC_TEST_NODE_ID").is_err() {
            true
        } else {
            self.0 == other.0
        }
    }
}

pub fn assign_generate_node_ids(ast: &mut AstRoot) -> usize {
    let mut generator = NodeIdGenerator { next_id: 0 };
    for statement in &mut ast.statements {
        generator.generate_in_statement(statement);
    }

    generator.count()
}

pub struct NodeIdGenerator {
    next_id: u32,
}

impl NodeIdGenerator {
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
            Stmt::FunctionDefinition { name, params, body, return_type } => {
                self.generate_in_identifier(name);
                self.generate_in_parameter_list(params);
                self.generate_in_block(body);
                self.generate_in_type_annotation(return_type);
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
            Stmt::Use(_use_statement) => {}
        }
    }

    fn generate_in_identifier(&mut self, identifier: &mut Identifier) {
        identifier.id = self.fresh();
    }

    fn generate_in_type_annotation(&mut self, type_: &mut TypeAnnotation) {
        if let Some(type_) = &mut type_.annotation {
            self.generate_in_type_expr(type_);
        }
    }

    fn generate_in_type_expr(&mut self, type_: &mut TypeExpression) {
        match type_ {
            TypeExpression::Ident(identifier) => {
                self.generate_in_identifier(identifier);
            },
            TypeExpression::Array { inner } => {
                self.generate_in_type_expr(inner);
            },
            TypeExpression::String | TypeExpression::Bool
            | TypeExpression::Int | TypeExpression::Float => {},
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
            MemTar::FieldAccess { target, field_name } => {
                self.generate_in_memory_target(target);
                self.generate_in_identifier(field_name);
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
            Expr::While { expr, body } => {
                self.generate_in_expression(expr);
                self.generate_in_block(body);
            },
            Expr::MemoryTarget(memory_target) => {
                self.generate_in_memory_target(memory_target);
            }
            Expr::FunctionCall { name, args } => {
                self.generate_in_identifier(name);
                self.generate_in_expression_list(args);
            }
            Expr::If { expr, body } => {
                self.generate_in_expression(expr);
                self.generate_in_block(body);
            }
            Expr::GoImmediateSource { .. }
            | Expr::StringLiteral(..) | Expr::BoolLiteral(..)
            | Expr::IntLiteral(..) | Expr::FloatLiteral(..) => {}
        }
    }
}
