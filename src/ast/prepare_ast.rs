use crate::{ast::{AstRoot, Block, Expression, Identifier, MemoryTarget, ParameterList, Statement, TypeExpression, expression::{Expr, ExpressionList}, memory_target::{self, MemTar}, statement::Stmt, type_expression::TypeAnnotation, use_statement}, type_resolve::type_env::IdGen};

pub fn generate_node_ids(ast: &mut AstRoot) -> usize {
    let mut generator = IdGen::new();
    for statement in &mut ast.statements {
        generate_in_statement(statement, &mut generator);
    }

    generator.count()
}
