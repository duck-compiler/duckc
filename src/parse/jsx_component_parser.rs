use chumsky::{input::BorrowInput, prelude::*};
use tree_sitter::{Node, Parser as TSParser};

use crate::parse::{
    SS, Spanned,
    type_parser::{Duck, TypeExpr, type_expression_parser},
};

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct JsxComponent {
    pub name: String,
    pub props_type: Spanned<TypeExpr>,
    pub javascript_source: Spanned<String>,
}

#[derive(Debug, Clone, Default)]
pub struct JsxComponentDependencies {
    pub client_components: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum JsxSourceUnit {
    Jsx,
    OpeningJsx,
    ClosingJsx,
    Expression,
    Ident,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Edit {
    Insert(String),
    Delete(usize),
    // Replace(String),
}

pub fn do_edits(to_edit: &mut String, edits: &mut [(usize, Edit)]) {
    edits.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

    let mut shift: isize = 0;

    for (pos, edit) in edits.iter() {
        let pos = *pos as isize;
        let pos = pos + shift;

        match edit {
            Edit::Insert(s) => to_edit.insert_str(pos as usize, s.as_str()),
            Edit::Delete(amount) => {
                to_edit
                    .drain((pos as usize)..((pos as usize) + *amount))
                    .for_each(drop);
            }
        }

        match edit {
            Edit::Insert(s) => shift += s.len() as isize,
            Edit::Delete(amount) => shift -= *amount as isize,
        }
    }
}

impl JsxComponent {
    pub fn find_units(&self) -> Vec<(tree_sitter::Range, JsxSourceUnit)> {
        let mut parser = TSParser::new();
        parser
            .set_language(&tree_sitter_javascript::LANGUAGE.into())
            .expect("Couldn't set js grammar");

        let src = parser
            .parse(self.javascript_source.0.as_bytes(), None)
            .unwrap();
        let root_node = src.root_node();

        fn trav(
            node: &Node,
            text: &[u8],
            already_in_jsx: bool,
            out: &mut Vec<(tree_sitter::Range, JsxSourceUnit)>,
        ) {
            if node.grammar_name() == "identifier" {
                out.push((node.range(), JsxSourceUnit::Ident));
            } else if node.grammar_name() == "jsx_opening_element"
                && node.utf8_text(text).is_ok_and(|x| x == "<>")
            {
                out.push((node.range(), JsxSourceUnit::OpeningJsx));
            } else if node.grammar_name() == "jsx_closing_element"
                && node.utf8_text(text).is_ok_and(|x| x == "</>")
            {
                out.push((node.range(), JsxSourceUnit::ClosingJsx));
            } else if node.grammar_name() == "jsx_expression" {
                out.push((node.range(), JsxSourceUnit::Expression));
                for i in 0..node.child_count() {
                    trav(node.child(i).as_ref().unwrap(), text, false, out);
                }
                return;
            }

            if node.grammar_name().starts_with("jsx_") {
                if !already_in_jsx {
                    out.push((node.range(), JsxSourceUnit::Jsx));
                }
                for i in 0..node.child_count() {
                    trav(node.child(i).as_ref().unwrap(), text, true, out);
                }
            } else {
                for i in 0..node.child_count() {
                    trav(node.child(i).as_ref().unwrap(), text, already_in_jsx, out);
                }
            }
        }

        let mut out = Vec::new();
        trav(
            &root_node,
            self.javascript_source.0.as_bytes(),
            false,
            &mut out,
        );
        out
    }
}

pub fn jsx_component_parser<'src, I>()
-> impl Parser<'src, I, JsxComponent, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    // component Name {
    //   %javascript source
    // }
    just(Token::Component)
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
        .then(
            select_ref! { Token::InlineJsx(jsx_source) => jsx_source.clone() }
                .map_with(|x, e| (x, e.span())),
        )
        .map(
            |(((ident, ident_span), props_type), jsx_source)| JsxComponent {
                name: ident.clone(),
                props_type: props_type
                    .unwrap_or((TypeExpr::Duck(Duck { fields: Vec::new() }), ident_span)),
                javascript_source: jsx_source,
            },
        )
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        lexer::lex_parser,
        make_input,
        value_parser::{empty_range, type_expr_into_empty_range},
    };

    use super::*;

    #[test]
    fn test_component_parser() {
        let src_and_expected_ast = vec![(
            "component T() jsx {useState()}",
            JsxComponent {
                name: "T".to_string(),
                props_type: TypeExpr::Duck(Duck { fields: Vec::new() }).into_empty_span(),
                javascript_source: ("useState()".to_string(), empty_range()),
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
            let component_parse_result =
                jsx_component_parser().parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(component_parse_result.has_errors(), false);
            assert_eq!(component_parse_result.has_output(), true);

            let Some(ast) = component_parse_result.into_output() else {
                unreachable!()
            };

            let mut ast = ast;
            ast.javascript_source.1 = empty_range();
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

            let component_parse_result =
                jsx_component_parser().parse(make_input(empty_range(), tokens.as_slice()));

            assert_eq!(component_parse_result.has_errors(), true);
            assert_eq!(component_parse_result.has_output(), false);
        }
    }
}
