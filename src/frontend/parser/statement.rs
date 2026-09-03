use crate::ast::struct_definition::{ImplBlock, Method, MethodKind, StructField, Visibility};
use crate::ast::use_statement::UseStatement;
use crate::ast::{Block, Identifier, Parameter, ParameterList, Span, Statement, Stmt};
use crate::frontend::lexer::Tok;
use crate::frontend::parser::error::ParseError;
use crate::frontend::parser::Parser;

impl<'src> Parser<'src> {
    pub fn parse_stmt(&mut self) -> Result<Statement<'src>, ParseError<'src>> {
        match self.tokens.current() {
            Tok::Use => self.parse_use_stmt(),
            Tok::Func => self.parse_function_def(),
            Tok::Struct => self.parse_struct_def(),
            Tok::Let => self.parse_var_decl(),
            Tok::Return => self.parse_return_stmt(),
            Tok::Break => self.parse_keyword_stmt(Stmt::Break),
            Tok::Continue => self.parse_keyword_stmt(Stmt::Continue),
            Tok::Const => Err(ParseError::new(
                "`const` not supported yet",
                self.tokens.current_span(),
            )),
            Tok::Impl => Err(ParseError::new(
                "impl blocks are only allowed after struct def",
                self.tokens.current_span(),
            )),
            _ => self.parse_expr_stmt(),
        }
    }

    pub fn parse_block(&mut self) -> Result<Block<'src>, ParseError<'src>> {
        let start_span = self.tokens.expect(Tok::LeftBrace, "`{`")?;

        let mut statements = Vec::new();
        while !self.tokens.take_if(Tok::RightBrace) {
            if self.tokens.at_end() {
                return Err(self.tokens.unexpected("`}`"));
            }

            statements.push(self.parse_with_struct_init(true, |parser| parser.parse_stmt())?);
        }

        Ok(Block {
            statements,
            span: self.span_from(start_span)
        })
    }

    fn parse_use_stmt(&mut self) -> Result<Statement<'src>, ParseError<'src>> {
        let start_span = self.tokens.expect(Tok::Use, "`use`")?;
        let path = self.parse_use_path()?;

        let alias = match self.tokens.take_if(Tok::As) {
            true => Some(self.parse_ident("an import alias")?),
            false => None,
        };

        self.tokens.expect(Tok::Semicolon, "`;`")?;

        let span = self.span_from(start_span);
        Ok(self.build_stmt(Stmt::Use(UseStatement {
            path,
            alias,
            span
        }), start_span))
    }

    fn parse_use_path(&mut self) -> Result<Identifier<'src>, ParseError<'src>> {
        if !matches!(self.tokens.current(), Tok::StringLiteral(_)) {
            return self.parse_ident("an import path");
        }

        let span = self.tokens.current_span();
        let ident = self.parse_string_literal()?;

        Ok(Identifier {
            id: self.fresh_id(),
            ident,
            span
        })
    }

    fn parse_function_def(&mut self) -> Result<Statement<'src>, ParseError<'src>> {
        let start_span = self.tokens.expect(Tok::Func, "`fn`")?;

        let name = self.parse_ident("a function name")?;
        let type_params = self.parse_type_params()?;
        let params = self.parse_param_list()?;
        let return_type = self.parse_return_type()?;
        let body = self.parse_block()?;

        Ok(self.build_stmt(
            Stmt::FunctionDefinition {
                name,
                type_params,
                params,
                body,
                return_type
            },
            start_span,
        ))
    }

    fn parse_struct_def(&mut self) -> Result<Statement<'src>, ParseError<'src>> {
        let struct_token = self.tokens.expect(Tok::Struct, "`struct`")?;

        let name = self.parse_ident("a struct name")?;
        let type_params = self.parse_type_params()?;

        self.tokens.expect(Tok::LeftBrace, "`{`")?;

        let mut fields = Vec::new();
        while !matches!(self.tokens.current(), Tok::RightBrace) {
            fields.push(self.parse_struct_field()?);
            if !self.tokens.take_if(Tok::Comma) {
                break;
            }
        }

        self.tokens.expect(Tok::RightBrace, "`}`")?;

        let impl_block = if matches!(self.tokens.current(), Tok::Impl) {
            Some(self.parse_impl_block()?)
        } else {
            None
        };

        Ok(self.build_stmt(
            Stmt::StructDefinition {
                name,
                type_params,
                fields,
                impl_block
            },
            struct_token,
        ))
    }

    fn parse_struct_field(&mut self) -> Result<StructField<'src>, ParseError<'src>> {
        let start = self.tokens.current_span();
        let visibility = self.parse_visibility();
        let name = self.parse_ident("a field name")?;
        let type_ = self.parse_expected_type_annotation()?;

        Ok(StructField {
            visibility,
            name,
            type_,
            span: self.span_from(start)
        })
    }

    fn parse_impl_block(&mut self) -> Result<ImplBlock<'src>, ParseError<'src>> {
        let start = self.tokens.expect(Tok::Impl, "`impl`")?;

        self.tokens.expect(Tok::LeftBrace, "`{`")?;

        let mut methods = Vec::new();
        while !self.tokens.take_if(Tok::RightBrace) {
            if self.tokens.at_end() {
                return Err(self.tokens.unexpected("`}`"));
            }

            methods.push(self.parse_method()?);
        }

        Ok(ImplBlock {
            methods,
            span: self.span_from(start)
        })
    }

    fn parse_method(&mut self) -> Result<Method<'src>, ParseError<'src>> {
        let start_span = self.tokens.current_span();
        let visibility = self.parse_visibility();

        let token_type = match self.tokens.take_if(Tok::Static) {
            true => MethodKind::Static,
            false => MethodKind::Instance,
        };

        self.tokens.expect(Tok::Func, "`fn`")?;

        let name = self.parse_ident("a method name")?;
        let type_params = self.parse_type_params()?;
        let params = self.parse_param_list()?;
        let return_type = self.parse_return_type()?;
        let body = self.parse_block()?;

        Ok(Method {
            visibility: visibility,
            kind: token_type,
            name,
            type_params,
            params,
            return_type,
            body,
            span: self.span_from(start_span),
        })
    }

    fn parse_visibility(&mut self) -> Visibility {
        match self.tokens.take_if(Tok::Pub) {
            true => Visibility::Public,
            false => Visibility::Private,
        }
    }

    fn parse_param_list(&mut self) -> Result<ParameterList<'src>, ParseError<'src>> {
        let start_span = self.tokens.expect(Tok::LeftParen, "`(`")?;

        let mut list = Vec::new();
        while !matches!(self.tokens.current(), Tok::RightParen) {
            let parameter_start_span = self.tokens.current_span();
            let name = self.parse_ident("a parameter name")?;
            let type_ = self.parse_expected_type_annotation()?;

            list.push(Parameter {
                name,
                type_,
                span: self.span_from(parameter_start_span)
            });

            if !self.tokens.take_if(Tok::Comma) {
                break;
            }
        }

        self.tokens.expect(Tok::RightParen, "`)`")?;

        Ok(ParameterList {
            list,
            span: self.span_from(start_span)
        })
    }

    fn parse_var_decl(&mut self) -> Result<Statement<'src>, ParseError<'src>> {
        let start = self.tokens.expect(Tok::Let, "`let`")?;
        let name = self.parse_ident("a variable name")?;
        let type_ = self.parse_type_annotation()?;

        let init_expression = match self.tokens.take_if(Tok::SingleEquals) {
            true => Some(self.parse_expr()?),
            false => None,
        };

        self.end_statement(false)?;

        Ok(self.build_stmt(
            Stmt::VariableDeclaration { name, type_, init_expression },
            start,
        ))
    }

    fn parse_return_stmt(&mut self) -> Result<Statement<'src>, ParseError<'src>> {
        let start = self.tokens.expect(Tok::Return, "`return`")?;

        let returns_nothing = matches!(self.tokens.current(), Tok::Semicolon | Tok::RightBrace);
        let value = match returns_nothing {
            true => None,
            false => Some(self.parse_expr()?),
        };

        self.end_statement(false)?;

        Ok(self.build_stmt(Stmt::Return { value }, start))
    }

    fn parse_keyword_stmt(&mut self, variant: Stmt<'src>) -> Result<Statement<'src>, ParseError<'src>> {
        let start_span = self.tokens.advance();
        self.end_statement(false)?;

        Ok(self.build_stmt(variant, start_span))
    }

    fn parse_expr_stmt(&mut self) -> Result<Statement<'src>, ParseError<'src>> {
        let start_span = self.tokens.current_span();

        let block_like = matches!(self.tokens.current(), Tok::If | Tok::While);
        let expr = self.parse_expr()?;

        let (variant, optional_semicolon) = match self.tokens.take_if(Tok::SingleEquals) {
            true => {
                let target = self.memory_target_from(expr, "invalid assignment target")?;
                let assign_expr = self.parse_expr()?;
                (Stmt::VariableAssignment { target, assign_expression: assign_expr }, false)
            }
            false => {
                let ends_the_block = matches!(self.tokens.current(), Tok::RightBrace);
                (Stmt::Expression { expr }, block_like || ends_the_block)
            }
        };

        self.end_statement(optional_semicolon)?;

        Ok(self.build_stmt(variant, start_span))
    }

    fn end_statement(&mut self, optional_semicolon: bool) -> Result<(), ParseError<'src>> {
        if self.tokens.take_if(Tok::Semicolon) || optional_semicolon {
            return Ok(());
        }

        Err(self.tokens.unexpected("`;`"))
    }

    fn build_stmt(&mut self, variant: Stmt<'src>, start: Span<'src>) -> Statement<'src> {
        Statement {
            id: self.fresh_id(),
            variant,
            span: self.span_from(start),
        }
    }
}
