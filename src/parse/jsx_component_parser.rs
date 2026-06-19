use chumsky::{input::BorrowInput, prelude::*};
use tree_sitter::{Node, Parser as TSParser};

use crate::parse::{
    SS, Spanned,
    type_parser::{Duck, TypeExpr, type_expression_parser},
};

use super::lexer::Token;

#[cfg(test)]
#[path = "jsx_component_parser_test.rs"]
mod tests;

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
}

pub fn do_edits(to_edit: &mut String, edits: &mut [(usize, Edit)]) {
    edits.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

    for (pos, edit) in edits.iter().rev() {
        let pos = *pos;
        match edit {
            Edit::Insert(s) => to_edit.insert_str(pos, s.as_str()),
            Edit::Delete(amount) => to_edit.drain(pos..(pos + *amount)).for_each(drop),
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
