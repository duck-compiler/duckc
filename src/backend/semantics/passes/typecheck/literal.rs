//! int / float literals are untyped
//!
//! this module does three things while typechecking
//!     check is the given expression even of the literal type required and is it possibly negated (strip_literal_op)
//!     can the value be represented in given type and does it fit (literal_representable_as)
//!     and committing the decision to the semantics context once made (commit_literal_type)
//!
//! try_represent_literal and default_literal_type arte the two entrypoints for the typechecker

use crate::{
    ast::{Expression, expression::{Expr, UnaryOperator}},
    backend::semantics::{diagnostic::Diagnostic, r#type::{Type, TypeId}},
};

use super::TypeChecker;

pub(super) fn strip_literal_op<'e, 'src>(
    expr: &'e Expression<'src>
) -> Option<(&'e Expression<'src>, bool)> {
    match &*expr.variant {
        Expr::IntLiteral(_) | Expr::FloatLiteral(_) => Some((expr, false)),
        Expr::Unary { op: UnaryOperator::Neg, expr: inner } => match &*inner.variant {
            Expr::IntLiteral(_) | Expr::FloatLiteral(_) => Some((inner, true)),
            _ => None,
        },
        _ => None,
    }
}

impl<'a, 'src> TypeChecker<'a, 'src> {
    pub(super) fn try_represent_literal(
        &mut self,
        expr: &Expression<'src>,
        expected: TypeId
    ) -> Option<TypeId> {
        let (literal, negated) = strip_literal_op(expr)?;

        let in_range = self.literal_representable_as(literal, expected, negated)?;

        self.commit_literal_type(literal, expected, in_range);
        self.set_node_type(expr.id, expected);

        Some(expected)
    }

    pub(super) fn default_literal_type(
        &mut self,
        literal: &Expression<'src>,
        negated: bool
    ) -> TypeId {
        let default = match &*literal.variant {
            Expr::FloatLiteral(_) => self.context.intern(Type::Float),
            _ => self.context.intern(Type::Int),
        };

        let in_range = self.literal_representable_as(literal, default, negated).unwrap_or(true);

        self.commit_literal_type(literal, default, in_range)
    }

    fn commit_literal_type(
        &mut self,
        literal: &Expression<'src>,
        expected: TypeId,
        in_range: bool
    ) -> TypeId {
        if !in_range {
            let expected_name = self.type_name(expected);
            self.context.report(Diagnostic::literal_out_of_range(&expected_name, literal.span));
        }

        self.set_node_type(literal.id, expected);

        expected
    }

    fn literal_representable_as(
        &self,
        literal: &Expression<'src>,
        expected: TypeId,
        negated: bool
    ) -> Option<bool> {
        let fits = |value: u64, max: u64| if negated { value <= max + 1 } else { value <= max };

        match (&*literal.variant, &self.context.types[expected.0 as usize]) {
            (Expr::IntLiteral(value), Type::Int | Type::Int64) => Some(fits(*value, i64::MAX as u64)),
            (Expr::IntLiteral(value), Type::Int8) => Some(fits(*value, i8::MAX as u64)),
            (Expr::IntLiteral(value), Type::Int16) => Some(fits(*value, i16::MAX as u64)),
            (Expr::IntLiteral(value), Type::Int32) => Some(fits(*value, i32::MAX as u64)),
            (Expr::IntLiteral(value), Type::Uint8) => Some(!negated && *value <= u8::MAX as u64),
            (Expr::IntLiteral(value), Type::Uint16) => Some(!negated && *value <= u16::MAX as u64),
            (Expr::IntLiteral(value), Type::Uint32) => Some(!negated && *value <= u32::MAX as u64),
            (Expr::IntLiteral(_), Type::Uint | Type::Uint64) => Some(!negated),
            (Expr::IntLiteral(_), Type::Float) => Some(true),
            (Expr::IntLiteral(value), Type::Float32) => Some((*value as f32).is_finite()),
            (Expr::FloatLiteral(value), Type::Float32) => Some((*value as f32).is_finite()),
            _ => None,
        }
    }
}
