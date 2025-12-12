use chumsky::{input::BorrowInput, prelude::*};

use crate::{
    parse::{
        SS, Spanned,
    },
};

use super::{
    lexer::Token,
    type_parser::{TypeExpr, type_expression_parser},
    value_parser::{ValueExpr, value_expr_parser},
};

#[derive(Debug, Clone, PartialEq)]
pub struct IfBranch {
    pub condition: Spanned<ValueExpr>,
    pub value_expr: Option<Spanned<ValueExpr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemaField {
    pub name: String,
    pub type_expr: Spanned<TypeExpr>,
    pub if_branch: Option<Spanned<IfBranch>>,
    pub else_branch_value_expr: Option<Spanned<ValueExpr>>,
    pub span: SS,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaDefinition {
    pub name: String,
    pub fields: Vec<SchemaField>,
    pub comments: Vec<Spanned<String>>,
    pub span: SS,
    pub out_type: Option<Spanned<TypeExpr>>,
    pub schema_fn_type: Option<Spanned<TypeExpr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaFunctionExpr {
    pub is_mut: bool,
    pub params: Vec<(String, Option<Spanned<TypeExpr>>)>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub value_expr: Spanned<ValueExpr>,
}

pub fn schema_definition_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, SchemaDefinition, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    let doc_comments_parser = select_ref! { Token::DocComment(comment) => comment.to_string() }
        .map_with(|comment, ctx| (comment, ctx.span()))
        .repeated()
        .collect()
        .or_not();

    let if_branch_parser = just(Token::If)
        .ignore_then(value_expr_parser(make_input.clone()))
        .then(value_expr_parser(make_input.clone()).or_not())
        .map_with(|(condition, maybe_value_expr), ctx| (IfBranch {
            condition,
            value_expr: maybe_value_expr
        }, ctx.span()));

    let else_branch_parser = just(Token::Else)
        .ignore_then(value_expr_parser(make_input))
        .map(|value_expr| value_expr);

    let field_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser())
        .then(if_branch_parser.or_not())
        .then(else_branch_parser.or_not())
        .map_with(|(((identifier, type_expr), if_branch), else_branch), ctx| SchemaField {
            name: identifier,
            type_expr,
            if_branch,
            else_branch_value_expr: else_branch,
            span: ctx.span(),
        });

    let fields_parser = field_parser
        .separated_by(just(Token::ControlChar(',')))
        .allow_trailing()
        .at_least(1)
        .collect::<Vec<SchemaField>>();

    doc_comments_parser
        .then_ignore(just(Token::Schema))
        .then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then_ignore(just(Token::ControlChar('=')))
        .then_ignore(just(Token::ControlChar('{')))
        .then(fields_parser)
        .then_ignore(just(Token::ControlChar('}')))
        .map_with(
            |((doc_comments, identifier), fields), ctx| {
                // todo: do a check if all fields if's value_exprs have a block for the value expr
                // value_expr = match value_expr {
                //     (ValueExpr::Duck(x), loc) if x.is_empty() => (ValueExpr::Block(vec![]), loc),
                //     x @ (ValueExpr::Block(_), _) => x,
                //     _ => panic!("Function must be block"),

                SchemaDefinition {
                    name: identifier,
                    fields,
                    span: ctx.span(),
                    comments: doc_comments.unwrap_or_else(Vec::new),
                    out_type: None,
                    schema_fn_type: None,
                }
            },
        )
}

#[cfg(test)]
pub mod tests {
    use crate::parse::{lexer::lex_parser, make_input, value_parser::empty_range};

    use super::*;

    #[test]
    fn test_schema_parser() {
        let valid_schema_defs = vec![
            "schema X = { name: String }",
            "schema Y = { name: String else .not }",
            "schema Z = { name: String if !name.empty() }",
            "schema Z = { name: String if !name.empty() else .not }",
            "schema Z = { name: String if !name.empty() { 5 } else .not }",
        ];

        for valid_schema_def in valid_schema_defs {
            println!("lexing {valid_schema_def}");
            let lexer_parse_result = lex_parser("test", "").parse(valid_schema_def);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("schema parsing {valid_schema_def}");
            let typedef_parse_result =
                schema_definition_parser(make_input).parse(make_input(empty_range(), &tokens));
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }

        let invalid_schema_def = vec![];

        for invalid_schema_def in invalid_schema_def {
            println!("lexing {invalid_schema_def}");
            let lexer_parse_result = lex_parser("test", "").parse(invalid_schema_def);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("schema parsing {invalid_schema_def}");
            let typedef_parse_result = schema_definition_parser(make_input)
                .parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }

    // #[test]
    // fn test_detailed_function_definitions() {
    //     let test_cases = vec![
    //         (
    //             "fn y<TYPENAME>() {}",
    //             SchemaDefinition {
    //                 name: "y".to_string(),
    //                 params: Some(vec![]),
    //                 out_type: None,
    //                 generics: Some(vec![(
    //                     Generic {
    //                         name: "TYPENAME".to_string(),
    //                         constraint: None,
    //                     },
    //                     empty_range(),
    //                 )]),
    //                 value_expr: ValueExpr::Block(vec![]).into_empty_span(),
    //                 span: empty_range(),
    //                 comments: Vec::new(),
    //             },
    //         ),
    //         (
    //             "fn y<TYPENAME, TYPENAME2>() {}",
    //             SchemaDefinition {
    //                 name: "y".to_string(),
    //                 params: Some(vec![]),
    //                 out_type: None,
    //                 generics: Some(vec![
    //                     (
    //                         Generic {
    //                             name: "TYPENAME".to_string(),
    //                             constraint: None,
    //                         },
    //                         empty_range(),
    //                     ),
    //                     (
    //                         Generic {
    //                             name: "TYPENAME2".to_string(),
    //                             constraint: None,
    //                         },
    //                         empty_range(),
    //                     ),
    //                 ]),
    //                 value_expr: ValueExpr::Block(vec![]).into_empty_span(),
    //                 span: empty_range(),
    //                 comments: Vec::new(),
    //             },
    //         ),
    //         (
    //             "fn y<TYPENAME, TYPENAME2, TYPENAME3>() {}",
    //             SchemaDefinition {
    //                 name: "y".to_string(),
    //                 params: Some(vec![]),
    //                 out_type: None,
    //                 generics: Some(vec![
    //                     (
    //                         Generic {
    //                             name: "TYPENAME".to_string(),
    //                             constraint: None,
    //                         },
    //                         empty_range(),
    //                     ),
    //                     (
    //                         Generic {
    //                             name: "TYPENAME2".to_string(),
    //                             constraint: None,
    //                         },
    //                         empty_range(),
    //                     ),
    //                     (
    //                         Generic {
    //                             name: "TYPENAME3".to_string(),
    //                             constraint: None,
    //                         },
    //                         empty_range(),
    //                     ),
    //                 ]),
    //                 value_expr: ValueExpr::Block(vec![]).into_empty_span(),
    //                 span: empty_range(),
    //                 comments: Vec::new(),
    //             },
    //         ),
    //     ];

    //     for (i, (src, expected_fns)) in test_cases.into_iter().enumerate() {
    //         let lex_result = lex_parser("test", "").parse(src).into_result().expect(&src);
    //         let parse_result = schema_definition_parser(make_input)
    //             .parse(make_input(empty_range(), &lex_result));

    //         assert_eq!(
    //             parse_result.has_errors(),
    //             false,
    //             "{i}: {} {:?} {:?}",
    //             src,
    //             lex_result,
    //             parse_result
    //         );

    //         assert_eq!(parse_result.has_output(), true, "{i}: {}", src);

    //         let mut output = parse_result.into_result().expect(&src);

    //         output
    //             .generics
    //             .as_mut()
    //             .unwrap()
    //             .iter_mut()
    //             .for_each(|generic| {
    //                 *generic = (generic.0.clone(), empty_range());
    //             });

    //         output.span = empty_range();
    //         output.value_expr = ValueExpr::Block(vec![]).into_empty_span();

    //         assert_eq!(output, expected_fns, "{i}: {}", src);
    //     }
    // }
}
