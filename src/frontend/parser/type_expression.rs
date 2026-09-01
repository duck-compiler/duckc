use crate::ast::TypeExpression;
use crate::ast::type_expression::{TypeAnnotation, TypeParam};
use crate::frontend::lexer::Tok;
use crate::frontend::parser::Parser;
use crate::frontend::parser::error::ParseError;

impl<'src> Parser<'src> {
    pub fn parse_type(&mut self) -> Result<TypeExpression<'src>, ParseError<'src>> {
        self.recurse_parse(|parser| parser.parse_type_expr())
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpression<'src>, ParseError<'src>> {
        if self.tokens.take_if(Tok::Star) {
            return Ok(TypeExpression::Pointer {
                inner: Box::new(self.parse_type()?)
            });
        }

        let mut type_expr = self.parse_primitive_type()?;
        while self.tokens.take_if(Tok::LeftSquare) {
            self.tokens.expect(Tok::RightSquare, "`]`")?;
            type_expr = TypeExpression::Array {
                inner: Box::new(type_expr)
            };
        }

        Ok(type_expr)
    }

    fn parse_primitive_type(&mut self) -> Result<TypeExpression<'src>, ParseError<'src>> {
        let primitive = match self.tokens.current() {
            Tok::Int => TypeExpression::Int,
            Tok::Int8 => TypeExpression::Int8,
            Tok::Int16 => TypeExpression::Int16,
            Tok::Int32 => TypeExpression::Int32,
            Tok::Int64 => TypeExpression::Int64,
            Tok::Uint => TypeExpression::Uint,
            Tok::Uint8 => TypeExpression::Uint8,
            Tok::Uint16 => TypeExpression::Uint16,
            Tok::Uint32 => TypeExpression::Uint32,
            Tok::Uint64 => TypeExpression::Uint64,
            Tok::Float => TypeExpression::Float,
            Tok::Float32 => TypeExpression::Float32,
            Tok::Bool => TypeExpression::Bool,
            Tok::String => TypeExpression::String,
            Tok::LeftParen => return self.parse_tuple_type(),
            Tok::Identifier(_) => {
                let name = self.parse_ident("a type")?;
                let type_args = match self.tokens.current() {
                    Tok::Less => self.parse_type_args()?,
                    _ => Vec::new(),
                };

                return Ok(TypeExpression::Ident { name, type_args });
            }
            _ => return Err(self.tokens.unexpected("a type")),
        };

        self.tokens.advance();
        Ok(primitive)
    }

    fn parse_tuple_type(&mut self) -> Result<TypeExpression<'src>, ParseError<'src>> {
        self.tokens.expect(Tok::LeftParen, "`(`")?;

        if self.tokens.take_if(Tok::RightParen) {
            return Ok(TypeExpression::Tuple(Vec::new()));
        }

        let first = self.parse_type()?;
        if self.tokens.take_if(Tok::RightParen) {
            return Ok(first);
        }

        let mut elements = vec![first];
        while self.tokens.take_if(Tok::Comma) {
            if matches!(self.tokens.current(), Tok::RightParen) {
                break;
            }

            elements.push(self.parse_type()?);
        }

        self.tokens.expect(Tok::RightParen, "`)`")?;
        Ok(TypeExpression::Tuple(elements))
    }

    pub fn parse_type_args(&mut self) -> Result<Vec<TypeExpression<'src>>, ParseError<'src>> {
        self.tokens.expect(Tok::Less, "`<`")?;

        let mut type_args = Vec::new();
        loop {
            type_args.push(self.parse_type()?);

            if !self.tokens.take_if(Tok::Comma) {
                break;
            }

            if self.tokens.take_closing_angle() {
                return Ok(type_args);
            }
        }

        if !self.tokens.take_closing_angle() {
            return Err(self.tokens.unexpected("`>`"));
        }

        Ok(type_args)
    }

    pub fn parse_type_params(&mut self) -> Result<Vec<TypeParam<'src>>, ParseError<'src>> {
        if !self.tokens.take_if(Tok::Less) {
            return Ok(Vec::new());
        }

        let mut type_params = Vec::new();
        loop {
            let start_span = self.tokens.current_span();
            let name = self.parse_ident("a type parameter name")?;

            type_params.push(TypeParam {
                name,
                span: self.span_from(start_span)
            });

            if !self.tokens.take_if(Tok::Comma) {
                break;
            }

            if self.tokens.take_closing_angle() {
                return Ok(type_params);
            }
        }

        if !self.tokens.take_closing_angle() {
            return Err(self.tokens.unexpected("`>`"));
        }

        Ok(type_params)
    }

    pub fn parse_type_annotation(&mut self) -> Result<TypeAnnotation<'src>, ParseError<'src>> {
        let start = self.tokens.current_span();

        if !self.tokens.take_if(Tok::Colon) {
            return Ok(TypeAnnotation {
                annotation: None,
                span: start
            });
        }

        let anno = self.parse_type()?;
        Ok(TypeAnnotation {
            annotation: Some(anno),
            span: self.span_from(start)
        })
    }

    pub fn parse_expected_type_annotation(&mut self) -> Result<TypeAnnotation<'src>, ParseError<'src>> {
        let start = self.tokens.expect(Tok::Colon, "`:`")?;
        let annotation = self.parse_type()?;

        Ok(TypeAnnotation {
            annotation: Some(annotation),
            span: self.span_from(start)
        })
    }

    pub fn parse_return_type(&mut self) -> Result<TypeAnnotation<'src>, ParseError<'src>> {
        let start_span = self.tokens.current_span();

        if !self.tokens.take_if(Tok::Arrow) {
            return Ok(TypeAnnotation {
                annotation: None,
                span: start_span
            });
        }

        let anno = self.parse_type()?;
        Ok(TypeAnnotation {
            annotation: Some(anno),
            span: self.span_from(start_span)
        })
    }
}
