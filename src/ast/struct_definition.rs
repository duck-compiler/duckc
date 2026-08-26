use duckc_macros::ast_derive;

use crate::ast::type_expression::TypeAnnotation;
use crate::ast::{Block, Identifier, ParameterList, Span};

pub const SELF_NAME: &str = "self";

#[derive(Clone, Copy)]
#[ast_derive]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Clone, Copy)]
#[ast_derive]
pub enum MethodKind {
    Instance,
    Static,
}

#[ast_derive]
pub struct StructField<'src> {
    pub visibility: Visibility,
    #[serde(borrow)]
    pub name: Identifier<'src>,
    pub type_: TypeAnnotation<'src>,
    pub span: Span<'src>,
}

#[ast_derive]
pub struct Method<'src> {
    pub visibility: Visibility,
    pub kind: MethodKind,
    #[serde(borrow)]
    pub name: Identifier<'src>,
    pub params: ParameterList<'src>,
    pub return_type: TypeAnnotation<'src>,
    pub body: Block<'src>,
    pub span: Span<'src>,
}

#[ast_derive]
pub struct ImplBlock<'src> {
    #[serde(borrow)]
    pub methods: Vec<Method<'src>>,
    pub span: Span<'src>,
}
