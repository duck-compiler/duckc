use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::parse::{
    Spanned, generics_parser::Generic, type_parser::TypeExpr, value_parser::ValueExpr,
};

#[derive(Debug, Clone)]
pub struct FunHeader {
    pub generics: Vec<Generic>,
    pub params: Vec<Spanned<TypeExpr>>,
    pub return_type: Spanned<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct MethodHeader {
    pub is_mut: bool,
    pub generics: Vec<Generic>,
    pub params: Vec<Spanned<TypeExpr>>,
    pub return_type: Spanned<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct StructHeader {
    pub fields: HashMap<String, Spanned<TypeExpr>>,
    pub methods: HashMap<String, MethodHeader>,
    pub generics: Vec<Generic>,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub typ: Spanned<TypeExpr>,
    pub is_const: bool,
}

#[derive(Debug, Clone, Default)]
pub struct HeadersEnv {
    pub structs: HashMap<String, StructHeader>,
    pub functions: HashMap<String, StructHeader>,
    pub global_variables: HashMap<String, VariableInfo>,
    pub variables: Vec<HashMap<String, VariableInfo>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(bound(deserialize = "'de: 'static"))]
pub struct ValueExprWithType {
    pub expr: Spanned<ValueExpr>,
    pub typ: Spanned<TypeExpr>,
}

pub trait NeverTypeExt {
    fn replace_if_not_never(&mut self, t: &Spanned<TypeExpr>);
    fn replace_if_other_never(&mut self, t: &Spanned<TypeExpr>);
}

impl NeverTypeExt for Spanned<TypeExpr> {
    fn replace_if_not_never(&mut self, t: &Spanned<TypeExpr>) {
        if !self.0.is_never() {
            self.0 = t.0.clone();
            self.1 = t.1;
        }
    }

    fn replace_if_other_never(&mut self, t: &Spanned<TypeExpr>) {
        if t.0.is_never() {
            self.0 = t.0.clone();
            self.1 = t.1;
        }
    }
}

impl ValueExprWithType {
    pub fn n(expr: Spanned<ValueExpr>) -> Self {
        let s = expr.1;
        Self {
            expr,
            typ: (TypeExpr::Uninit, s),
        }
    }
    pub fn new(expr: Spanned<ValueExpr>, typ: Spanned<TypeExpr>) -> Self {
        Self { expr, typ }
    }
}

pub fn resolve_types(v: Spanned<ValueExpr>, headers: &HeadersEnv) -> ValueExprWithType {
    let span = v.1;
    let typ = (
        match &v.0 {
            ValueExpr::Int(..) => TypeExpr::Int,
            ValueExpr::Float(..) => TypeExpr::Float,
            ValueExpr::Bool(..) => TypeExpr::Bool(None),
            ValueExpr::Char(..) => TypeExpr::Char,
            ValueExpr::String(..) => TypeExpr::String(None),
            ValueExpr::Tag(t) => TypeExpr::Tag(t.clone()),
            ValueExpr::Async(..) => TypeExpr::Statement,
            _ => {
                todo!()
            }
        },
        span,
    );
    ValueExprWithType::new(v, typ)
}
