use crate::ast::expression::{BinaryOperator, Expr, ExpressionList, FieldInit, UnaryOperator};
use crate::ast::memory_target::MemTar;
use crate::ast::{Block, Expression, Identifier, MemoryTarget, Span, Statement, Stmt, TypeExpression};
use crate::frontend::lexer::{StrPart, Tok};
use crate::frontend::parser::Parser;
use crate::frontend::parser::error::ParseError;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum OpPrecedence {
    Or = 1,
    And = 2,
    Equality = 3,
    Comparison = 4,
    Additive = 5,
    Multiplicative = 6,
}

// go rejects everything above
const MAX_OCTAL_ESCAPE: u32 = 0o377;

impl<'src> Parser<'src> {
    pub fn parse_expr(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        self.parse_binary_expr(0)
    }

    fn parse_binary_expr(&mut self, min_precedence: u8) -> Result<Expression<'src>, ParseError<'src>> {
        self.recurse_parse(|parser| parser.parse_binary_operators(min_precedence))
    }

    fn parse_binary_operators(&mut self, min_precedence: u8) -> Result<Expression<'src>, ParseError<'src>> {
        let start_span = self.tokens.current_span(); // todo: tests that the span actually works correctly and points to the correct item in source
        let mut left_expr = self.parse_prefix_expr()?;

        loop {
            let Some((operator, precedence)) = binary_op(self.tokens.current()) else {
                if let Some(message) = unsupported_infix(self.tokens.current()) {
                    return Err(ParseError::new(message, self.tokens.current_span()));
                }

                break;
            };

            let precedence = precedence as u8;
            if precedence < min_precedence {
                break;
            }

            self.tokens.advance();
            let right_expr = self.parse_binary_expr(precedence + 1)?;

            left_expr = self.build_expr(
                Expr::Binary {
                    left: Box::new(left_expr),
                    op: operator,
                    right: Box::new(right_expr)
                },
                start_span,
            );
        }

        Ok(left_expr)
    }

    fn parse_prefix_expr(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        self.recurse_parse(|parser| parser.parse_prefix_operators())
    }

    fn parse_prefix_operators(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let start = self.tokens.current_span();

        let unary_operator = match self.tokens.current() {
            Tok::Bang => Some(UnaryOperator::Bang),
            Tok::Minus => Some(UnaryOperator::Neg),
            _ => None,
        };

        if let Some(op) = unary_operator {
            self.tokens.advance();

            let expr = self.parse_prefix_expr()?;
            return Ok(self.build_expr(Expr::Unary {
                op,
                expr: Box::new(expr)
            }, start));
        }

        if self.tokens.take_if(Tok::Ampersand) {
            let expr = self.parse_prefix_expr()?;
            return Ok(self.build_expr(Expr::Reference { expr: Box::new(expr) }, start));
        }

        if self.tokens.take_if(Tok::Star) {
            let expr = self.parse_prefix_expr()?;
            return Ok(self.build_mem_target_expr(MemTar::Dereference(Box::new(expr)), start));
        }

        self.parse_postfix_expr()
    }

    fn parse_postfix_expr(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let start_span = self.tokens.current_span();
        let mut expr = self.parse_value_expr()?;

        loop {
            match self.tokens.current() {
                Tok::Dot => {
                    expr = self.parse_field_access_expr(expr, start_span)?
                }
                Tok::LeftSquare => {
                    expr = self.parse_index_access_expr(expr, start_span)?
                }
                Tok::LeftParen =>{
                    expr = self.parse_call_expr(expr, Vec::new(), start_span)?;
                }
                Tok::LeftBrace if self.allow_struct_init && is_name_target(&expr) => {
                    expr = self.parse_struct_init(expr, Vec::new(), start_span)?;
                }
                Tok::Less if can_take_type_arguments(&expr) => {
                    let mark = self.tokens.mark();

                    let Ok(type_args) = self.parse_type_args() else {
                        self.tokens.reset(mark);
                        break;
                    };

                    if !self.accepts_type_args(&expr) {
                        if is_name_target(&expr) && self.at_fun_value_termnator() {
                            return Err(ParseError::new(
                                "type arguments on a function value are not supported yet",
                                self.span_from(start_span),
                            ));
                        }

                        self.tokens.reset(mark);
                        break;
                    }

                    expr = self.apply_type_args(expr, type_args, start_span)?;
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn accepts_type_args(&self, expr: &Expression<'src>) -> bool {
        let is_call = matches!(self.tokens.current(), Tok::LeftParen);

        let is_struct_init = self.allow_struct_init
            && is_name_target(expr)
            && matches!(self.tokens.current(), Tok::LeftBrace);

        let is_fun_value = is_raw_field_access(expr) && self.at_fun_value_termnator();

        is_call || is_struct_init || is_fun_value
    }

    fn at_fun_value_termnator(&self) -> bool {
        matches!(
            self.tokens.current(),
            Tok::Semicolon
                | Tok::Comma
                | Tok::RightParen
                | Tok::RightSquare
                | Tok::RightBrace
                | Tok::EOF
        )
    }

    fn apply_type_args(
        &mut self,
        expr: Expression<'src>,
        type_args: Vec<TypeExpression<'src>>,
        start: Span<'src>,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        if matches!(self.tokens.current(), Tok::LeftParen) {
            return self.parse_call_expr(expr, type_args, start);
        }

        if matches!(self.tokens.current(), Tok::LeftBrace) {
            return self.parse_struct_init(expr, type_args, start);
        }

        let span = self.span_from(start);
        Ok(apply_type_args_to_field_access(expr, type_args, span))
    }

    fn parse_field_access_expr(
        &mut self,
        expr: Expression<'src>,
        start: Span<'src>,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        self.tokens.advance();

        let target = expr;

        match self.tokens.current() {
            Tok::Identifier(_) => {
                let field_name = self.parse_ident("a field or method name")?;

                Ok(self.build_mem_target_expr(
                    MemTar::FieldAccess {
                        target: Box::new(target),
                        field_name,
                        type_args: Vec::new(),
                    },
                    start,
                ))
            }
            Tok::IntLiteral(index) => {
                let index = *index;
                let span = self.tokens.advance();

                let Ok(index) = usize::try_from(index) else {
                    return Err(ParseError::new("tuple index is out of range", span));
                };

                Ok(self.build_mem_target_expr(
                    MemTar::TupleIndex {
                        target: Box::new(target),
                        index
                    },
                    start,
                ))
            }
            Tok::FloatLiteral(_) => self.parse_chained_tuple_index_expr(target, start),
            _ => Err(self.tokens.unexpected("a field name or a tuple index")),
        }
    }

    fn parse_chained_tuple_index_expr(
        &mut self,
        target: Expression<'src>,
        start: Span<'src>,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        let literal_span = self.tokens.current_span();
        let literal = self.tokens.source_slice(literal_span);
        self.tokens.advance();

        let Some((outer_digits, inner_digits)) = literal.split_once('.') else {
            return Err(ParseError::new("expected a tuple index", literal_span));
        };

        let (Ok(outer), Ok(inner)) = (outer_digits.parse::<usize>(), inner_digits.parse::<usize>()) else {
            return Err(ParseError::new("tuple index is out of range", literal_span));
        };

        let outer_span = Span {
            file_path: start.file_path,
            start: start.start,
            end: literal_span.start + outer_digits.len(),
        };

        let outer_target = Expression {
            id: self.fresh_id(),
            variant: Box::new(Expr::MemoryTarget(MemoryTarget {
                variant: MemTar::TupleIndex { target: Box::new(target), index: outer },
                span: outer_span,
            })),
            span: outer_span,
        };

        Ok(self.build_mem_target_expr(
            MemTar::TupleIndex { target: Box::new(outer_target), index: inner },
            start,
        ))
    }

    fn parse_index_access_expr(
        &mut self,
        expr: Expression<'src>,
        start: Span<'src>,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        self.tokens.advance();

        let target = expr;

        let index_expr = self.parse_with_struct_init(true, |parser| parser.parse_expr())?;

        self.tokens.expect(Tok::RightSquare, "`]`")?;

        Ok(self.build_mem_target_expr(
            MemTar::ArrayAccess {
                target: Box::new(target),
                index_expression: Box::new(index_expr),
            },
            start,
        ))
    }

    fn parse_call_expr(
        &mut self,
        expr: Expression<'src>,
        type_args: Vec<TypeExpression<'src>>,
        start: Span<'src>,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        let target_span = self.span_from(start);
        let (target, type_args) = split_call_type_args(expr, type_args, target_span);

        let args_start = self.tokens.expect(Tok::LeftParen, "`(`")?;

        let mut exprs = Vec::new();
        while !matches!(self.tokens.current(), Tok::RightParen) {
            exprs.push(self.parse_with_struct_init(true, |parser| parser.parse_expr())?);
            if !self.tokens.take_if(Tok::Comma) {
                break;
            }
        }

        self.tokens.expect(Tok::RightParen, "`)`")?;

        let args = ExpressionList {
            list: exprs,
            span: self.span_from(args_start)
        };

        Ok(self.build_expr(
            Expr::FunctionCall {
                target: Box::new(target),
                type_args,
                args
            },
            start,
        ))
    }

    fn parse_struct_init(
        &mut self,
        expr: Expression<'src>,
        type_args: Vec<TypeExpression<'src>>,
        start: Span<'src>,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        let Some(type_name) = take_name_target(expr) else {
            return Err(ParseError::new("expected struct name", start));
        };

        self.tokens.expect(Tok::LeftBrace, "`{`")?;

        let mut fields = Vec::new();
        while !matches!(self.tokens.current(), Tok::RightBrace) {
            let field_start = self.tokens.current_span();
            let name = self.parse_ident("a field name")?;

            self.tokens.expect(Tok::Colon, "`:`")?;

            let value = self.parse_with_struct_init(true, |parser| parser.parse_expr())?;
            fields.push(FieldInit {
                name,
                value,
                span: self.span_from(field_start)
            });

            if !self.tokens.take_if(Tok::Comma) {
                break;
            }
        }

        self.tokens.expect(Tok::RightBrace, "`}`")?;

        Ok(self.build_expr(Expr::StructInit { type_name, type_args, fields }, start))
    }

    fn parse_value_expr(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let start_span = self.tokens.current_span();

        match self.tokens.current() {
            Tok::IntLiteral(value) => {
                let value = *value;
                self.tokens.advance();

                Ok(self.build_expr(Expr::IntLiteral(value), start_span))
            }
            Tok::FloatLiteral(value) => {
                let value = *value;
                self.tokens.advance();

                Ok(self.build_expr(Expr::FloatLiteral(value), start_span))
            }
            Tok::BoolLiteral(value) => {
                let value = *value;
                self.tokens.advance();

                Ok(self.build_expr(Expr::BoolLiteral(value), start_span))
            }
            Tok::StringLiteral(_) => {
                let text = self.parse_string_literal()?;
                Ok(self.build_expr(Expr::StringLiteral(text), start_span))
            }
            Tok::Identifier(_) => {
                let name = self.parse_ident("an expression")?;
                Ok(self.build_mem_target_expr(MemTar::Name(name), start_span))
            }
            Tok::LeftParen => self.parse_expr_in_parens(),
            Tok::LeftSquare => self.parse_array_expr(),
            Tok::If => self.parse_if_expr(),
            Tok::While => self.parse_while_expr(),
            Tok::LeftBrace => Err(ParseError::new("block expressions are not supported yet", start_span)),
            _ => Err(self.tokens.unexpected("an expression")),
        }
    }

    pub(super) fn parse_string_literal(&mut self) -> Result<&'src str, ParseError<'src>> {
        let start_span = self.tokens.current_span();
        let src_text = self.tokens.source_slice(start_span);

        if src_text.starts_with('f') {
            return Err(ParseError::new("f-strings are not supported yet", start_span));
        }

        if let Some(error) = octal_esc_out_of_range(self.tokens.current()) {
            return Err(error);
        }

        self.tokens.advance();
        Ok(&src_text[1..src_text.len() - 1])
    }

    fn parse_expr_in_parens(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let span = self.tokens.expect(Tok::LeftParen, "`(`")?;

        if self.tokens.take_if(Tok::RightParen) {
            return Ok(self.build_expr(Expr::TupleExpression {
                values: Vec::new()
            }, span));
        }

        let first_expr = self.parse_with_struct_init(true, |parser| parser.parse_expr())?;
        if self.tokens.take_if(Tok::RightParen) {
            return Ok(first_expr);
        }

        let mut values = vec![Box::new(first_expr)];
        while self.tokens.take_if(Tok::Comma) {
            if matches!(self.tokens.current(), Tok::RightParen) {
                break;
            }

            values.push(Box::new(self.parse_with_struct_init(true, |parser| parser.parse_expr())?));
        }

        self.tokens.expect(Tok::RightParen, "`)`")?;

        Ok(self.build_expr(Expr::TupleExpression { values }, span))
    }

    fn parse_array_expr(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let start_span = self.tokens.expect(Tok::LeftSquare, "`[`")?;

        let mut values_exprs = Vec::new();
        while !matches!(self.tokens.current(), Tok::RightSquare) {
            values_exprs.push(Box::new(self.parse_with_struct_init(true, |parser| parser.parse_expr())?));
            if !self.tokens.take_if(Tok::Comma) {
                break;
            }
        }

        self.tokens.expect(Tok::RightSquare, "`]`")?;

        Ok(self.build_expr(Expr::ArrayExpression { values_exprs }, start_span))
    }

    fn parse_if_expr(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let start_span = self.tokens.expect(Tok::If, "`if`")?;

        let condition_expr = self.parse_with_struct_init(false, |parser| parser.parse_expr())?;
        let body_expr = self.parse_block()?;

        let else_branch = match self.tokens.take_if(Tok::Else) {
            true => Some(self.parse_else_branch()?),
            false => None,
        };

        Ok(self.build_expr(
            Expr::If { expr: Box::new(condition_expr), body: body_expr, else_branch },
            start_span,
        ))
    }

    fn parse_else_branch(&mut self) -> Result<Block<'src>, ParseError<'src>> {
        if !matches!(self.tokens.current(), Tok::If) {
            return self.parse_block();
        }

        let nested_expr = self.recurse_parse(|parser| parser.parse_if_expr())?;
        let span = nested_expr.span;

        let statement = Statement {
            id: self.fresh_id(),
            variant: Stmt::Expression { expr: nested_expr },
            span,
        };

        Ok(Block { statements: vec![statement], span })
    }

    fn parse_while_expr(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let start_span = self.tokens.expect(Tok::While, "`while`")?;
        let condition_expr = self.parse_with_struct_init(false, |parser| parser.parse_expr())?;
        let body_expr = self.parse_block()?;

        Ok(self.build_expr(Expr::While {
            expr: Box::new(condition_expr),
            body: body_expr
        }, start_span))
    }

    fn build_expr(&mut self, variant: Expr<'src>, start: Span<'src>) -> Expression<'src> {
        Expression {
            id: self.fresh_id(),
            variant: Box::new(variant),
            span: self.span_from(start),
        }
    }

    fn build_mem_target_expr(&mut self, variant: MemTar<'src>, start: Span<'src>) -> Expression<'src> {
        let span = self.span_from(start);
        self.build_expr(Expr::MemoryTarget(MemoryTarget { variant, span }), start)
    }

    pub(super) fn memory_target_from(
        &self,
        expr: Expression<'src>,
        err_msg: &str,
    ) -> Result<MemoryTarget<'src>, ParseError<'src>> {
        let span = expr.span;

        match *expr.variant {
            Expr::MemoryTarget(target) => Ok(target),
            _ => Err(ParseError::new(err_msg, span)),
        }
    }
}

fn octal_esc_out_of_range<'src>(variant: &Tok<'src>) -> Option<ParseError<'src>> {
    let Tok::StringLiteral(parts) = variant else {
        return None;
    };

    parts.iter().find_map(|part| {
        let StrPart::Octal(digits) = part.variant else {
            return None;
        };

        let value = u32::from_str_radix(digits, 8).ok()?;

        match value > MAX_OCTAL_ESCAPE {
            true => Some(ParseError::new(
                format!("octal escape `\\{digits}` is out of range"),
                part.span,
            )),
            false => None,
        }
    })
}

fn binary_op(variant: &Tok<'_>) -> Option<(BinaryOperator, OpPrecedence)> {
    Some(match variant {
        Tok::Or => (BinaryOperator::Or, OpPrecedence::Or),
        Tok::And => (BinaryOperator::And, OpPrecedence::And),
        Tok::DoubleEquals => (BinaryOperator::Eq, OpPrecedence::Equality),
        Tok::NotEquals => (BinaryOperator::NotEq, OpPrecedence::Equality),
        Tok::Less => (BinaryOperator::Less, OpPrecedence::Comparison),
        Tok::Greater => (BinaryOperator::Greater, OpPrecedence::Comparison),
        Tok::LessEquals => (BinaryOperator::LessEq, OpPrecedence::Comparison),
        Tok::GreaterEquals => (BinaryOperator::GreaterEq, OpPrecedence::Comparison),
        Tok::Plus => (BinaryOperator::Add, OpPrecedence::Additive),
        Tok::Minus => (BinaryOperator::Sub, OpPrecedence::Additive),
        Tok::Star => (BinaryOperator::Mul, OpPrecedence::Multiplicative),
        Tok::Slash => (BinaryOperator::Div, OpPrecedence::Multiplicative),
        _ => return None,
    })
}

fn unsupported_infix(variant: &Tok<'_>) -> Option<&'static str> {
    Some(match variant {
        Tok::Percent => "the `%` is not supported yet",
        Tok::ShiftLeft => "the `<<` is not supported yet",
        Tok::ShiftRight => "the `>>` is not supported yet",
        Tok::Ampersand => "the `&` is not supported yet",
        Tok::Bar => "the `|` is not supported yet",
        Tok::Tilde => "`~` is not supported yet",
        Tok::As => "`as` not supported yet",
        Tok::PlusAssign
        | Tok::MinusAssign
        | Tok::MulAssign
        | Tok::DivAssign
        | Tok::PercentAssign
        | Tok::ShiftLeftAssign
        | Tok::ShiftRightAssign
        | Tok::AmpersandAssign
        | Tok::BarAssign => "operator assignments are not supported yet",
        _ => return None,
    })
}

fn is_name_target(expr: &Expression<'_>) -> bool {
    let Expr::MemoryTarget(target) = &*expr.variant else {
        return false;
    };

    matches!(target.variant, MemTar::Name(_))
}

fn is_raw_field_access(expr: &Expression<'_>) -> bool {
    let Expr::MemoryTarget(target) = &*expr.variant else {
        return false;
    };

    matches!(&target.variant, MemTar::FieldAccess { type_args, .. } if type_args.is_empty())
}

fn can_take_type_arguments(expr: &Expression<'_>) -> bool {
    is_name_target(expr) || is_raw_field_access(expr)
}

fn take_name_target<'src>(expr: Expression<'src>) -> Option<Identifier<'src>> {
    let Expr::MemoryTarget(target) = *expr.variant else {
        return None;
    };

    match target.variant {
        MemTar::Name(name) => Some(name),
        _ => None,
    }
}

fn apply_type_args_to_field_access<'src>(
    mut expr: Expression<'src>,
    type_args: Vec<TypeExpression<'src>>,
    span: Span<'src>,
) -> Expression<'src> {
    if let Expr::MemoryTarget(target) = &mut *expr.variant
        && let MemTar::FieldAccess { type_args: field_type_args, .. } = &mut target.variant
    {
        *field_type_args = type_args;
        target.span = span;
        expr.span = span;
    }

    expr
}

fn split_call_type_args<'src>(
    expr: Expression<'src>,
    type_args: Vec<TypeExpression<'src>>,
    span: Span<'src>,
) -> (Expression<'src>, Vec<TypeExpression<'src>>) {
    if type_args.is_empty() || !is_raw_field_access(&expr) {
        return (expr, type_args);
    }

    (apply_type_args_to_field_access(expr, type_args, span), Vec::new())
}
