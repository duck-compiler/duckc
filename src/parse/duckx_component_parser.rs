use std::collections::HashSet;

use chumsky::{input::BorrowInput, prelude::*};
use tree_sitter::{Node, Parser as TSParser};

use crate::{
    parse::{
        SS, Spanned,
        type_parser::{Duck, TypeExpr, type_expression_parser},
        value_parser::{ValHtmlStringContents, ValueExpr, value_expr_parser},
    },
    semantics::type_resolve::TypeEnv,
};

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct DuckxComponent {
    pub name: String,
    pub props_type: Spanned<TypeExpr>,
    pub value_expr: Spanned<ValueExpr>,
}

#[derive(Debug, Clone, Default)]
pub struct DuckxComponentDependencies {
    pub client_components: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum HtmlStringSourceUnit {
    JsxOpen,
    JsxClose,
    Ident,
}

// pub fn transform_html_string(html_string: &ValueExpr) -> ValueExpr {
//     let ValueExpr::HtmlString(contents) = html_string else {
//         panic!("not a html string")
//     };

//     let mut out = Vec::new();
// }

pub fn find_client_components(
    obj: &Vec<ValHtmlStringContents>,
    out: &mut HashSet<String>,
    type_env: &mut TypeEnv,
) {
    fn trav(n: &Node, t: &[u8], out: &mut HashSet<String>, type_env: &mut TypeEnv) {
        if n.grammar_name() == "self_closing_tag" {
            for comp in type_env.get_full_component_dependencies(
                &n.child(1).unwrap().utf8_text(t).unwrap().to_string(),
            ) {
                out.insert(comp);
            }
        } else {
            for i in 0..n.child_count() {
                trav(&n.child(i).unwrap(), t, out, type_env);
            }
        }
    }
    let mut s = String::new();
    for c in obj {
        match c {
            ValHtmlStringContents::Expr((e, _)) => {
                if let ValueExpr::HtmlString(contents) = e {
                    find_client_components(contents, out, type_env);
                }
                s.push_str("\"\"");
            }
            ValHtmlStringContents::String(s_) => {
                let mut x = s.as_str();
                while let Some(idx) = x.find("<") {
                    x = &x[idx + 1..];
                    let end = x.find([' ', '>']).unwrap_or(x.len());
                    let mut between = &x[..end];
                    if between.ends_with("/") {
                        between = &between[..between.len() - 1];
                    }
                    if type_env.get_component(between).is_some() {
                        type_env
                            .get_full_component_dependencies(&between.to_string())
                            .into_iter()
                            .for_each(|s| {
                                out.insert(s);
                            })
                    }
                }

                s.push_str(s_.as_str());
            }
        }
    }

    let mut parser = TSParser::new();
    parser
        .set_language(&tree_sitter_html::LANGUAGE.into())
        .expect("Couldn't set js grammar");

    let src = parser.parse(s.as_bytes(), None).unwrap();
    let root_node = src.root_node();
    trav(&root_node, s.as_bytes(), out, type_env);
}

pub fn duckx_component_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, DuckxComponent, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    // component Name {
    //   %javascript source
    // }
    just(Token::Template)
        .ignore_then(
            select_ref! { Token::Ident(identifier) => identifier.clone() }
                .map_with(|ident, e| (ident, e.span())),
        )
        .then(
            just(Token::Ident("props".to_string()))
                .ignore_then(just(Token::ControlChar(':')))
                .ignore_then(type_expression_parser())
                .or_not()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')'))),
        )
        .then(value_expr_parser(make_input))
        .map(
            |(((ident, ident_span), props_type), src_tokens)| DuckxComponent {
                name: ident.clone(),
                props_type: props_type
                    .unwrap_or((TypeExpr::Duck(Duck { fields: Vec::new() }), ident_span)),
                value_expr: if let ValueExpr::Duck(fields) = &src_tokens.0
                    && fields.is_empty()
                {
                    (ValueExpr::Block(vec![]), src_tokens.1)
                } else {
                    src_tokens
                },
            },
        )
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        lexer::lex_parser,
        make_input,
        value_parser::{empty_range, type_expr_into_empty_range, value_expr_into_empty_range},
    };

    use super::*;

    #[test]
    fn test_component_parser() {
        let src_and_expected_ast = vec![(
            "template T() duckx {}",
            DuckxComponent {
                name: "T".to_string(),
                props_type: TypeExpr::Duck(Duck { fields: Vec::new() }).into_empty_span(),
                value_expr: ValueExpr::Block(vec![]).into_empty_span(),
            },
        )];

        for (src, expected_ast) in src_and_expected_ast {
            println!("lexing {src}");
            let lexer_parse_result = lex_parser("test", "").parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("parsing component statement {src}");
            let component_parse_result = duckx_component_parser(make_input)
                .parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(component_parse_result.has_errors(), false);
            assert_eq!(component_parse_result.has_output(), true);

            let Some(mut ast) = component_parse_result.into_output() else {
                unreachable!()
            };
            value_expr_into_empty_range(&mut ast.value_expr);
            type_expr_into_empty_range(&mut ast.props_type);
            assert_eq!(ast, expected_ast);
        }

        let invalid_component_statements = vec![
            "use x::;",
            "use y::{};",
            "use std::{}",
            "use ::;",
            "use :std:;",
            "use :std::{};",
            "use go x;",
            "use go;",
            "use go \"fmt\" as;",
            "use go fmt as x",
            "use go::x;",
            "use go::x;",
            "use go as;",
        ];

        for invalid_component_statement in invalid_component_statements {
            println!("lexing {invalid_component_statement}");

            let lexer_parse_result = lex_parser("test", "").parse(invalid_component_statement);

            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("component parser try invalid {invalid_component_statement}");

            let component_parse_result = duckx_component_parser(make_input)
                .parse(make_input(empty_range(), tokens.as_slice()));

            assert_eq!(component_parse_result.has_errors(), true);
            assert_eq!(component_parse_result.has_output(), false);
        }
    }
}
