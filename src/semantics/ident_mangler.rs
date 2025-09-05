use std::collections::HashMap;

use tree_sitter::{Node, Parser};

use crate::parse::{
    function_parser::LambdaFunctionExpr,
    type_parser::{Duck, TypeExpr},
    value_parser::{ValFmtStringContents, ValueExpr},
};

#[derive(Debug, Clone, PartialEq)]
pub struct MangleEnv {
    pub imports: HashMap<String, (bool, Vec<String>)>,
    pub sub_mods: Vec<String>,
    pub global_prefix: Vec<String>,
    pub names: Vec<Vec<String>>,
    pub types: Vec<Vec<String>>,
}

pub const MANGLE_SEP: &str = "_____";

pub fn mangle(p: &[impl AsRef<str>]) -> String {
    p.iter()
        .map(|x| x.as_ref().to_string())
        .collect::<Vec<_>>()
        .join(MANGLE_SEP)
}

pub fn unmangle(s: &str) -> Vec<String> {
    s.split(MANGLE_SEP).map(String::from).collect()
}

impl MangleEnv {
    pub fn is_imported_name(&self, x: &String) -> bool {
        self.is_top_level_ident(x) && self.imports.contains_key(x)
    }

    pub fn is_imported_type(&self, x: &String) -> bool {
        self.is_top_level_type(x) && self.imports.contains_key(x)
    }

    pub fn local_defined(&self, n: &String) -> bool {
        self.names.last().filter(|x| x.contains(n)).is_some()
    }

    pub fn resolve_import(&self, mut sym: String) -> Option<(bool, Vec<String>)> {
        let mut result = None;

        if self.sub_mods.contains(&sym) {
            return Some((false, vec![]));
        }

        while let Some((is_glob, import_path)) = self.imports.get(&sym) {
            result = result
                .map(|(g, p)| {
                    (g || *is_glob, {
                        let mut import_path = import_path.to_owned();
                        import_path.extend(p);
                        import_path
                    })
                })
                .or(Some((*is_glob, import_path.to_owned())));
            if import_path.is_empty() {
                break;
            }
            sym = import_path.first().unwrap().clone();
        }

        result
    }

    pub fn mangle_type(
        &self,
        is_global: bool,
        prefix: &[String],
        ident: &[String],
    ) -> Option<Vec<String>> {
        let prefix = if is_global { &[] } else { prefix };

        if !is_global
            && let Some((is_glob, import_path)) = self.resolve_import(ident.first()?.clone())
        {
            let mut res = Vec::new();

            if !is_glob {
                res.extend_from_slice(prefix);
            } else {
                // res.extend_from_slice(&self.global_prefix);
            }

            res.extend(import_path);
            res.extend_from_slice(ident);

            return Some(res);
        }

        if self.is_top_level_type(ident.first()?) {
            let mut x = Vec::new();
            if is_global {
                // x.extend_from_slice(&self.global_prefix);
            } else {
                x.extend_from_slice(prefix);
            }
            x.extend_from_slice(ident);
            return Some(x);
        }

        None
    }

    pub fn mangle_ident(
        &self,
        is_global: bool,
        prefix: &[String],
        ident: &[String],
    ) -> Option<Vec<String>> {
        if self.local_defined(ident.first()?) {
            return None;
        }

        let prefix = if is_global { &[] } else { prefix };

        if let Some((is_glob, import_path)) = self.resolve_import(ident.first()?.clone()) {
            let mut res = Vec::new();

            if !is_glob {
                res.extend_from_slice(prefix);
            } else {
                res.extend_from_slice(&self.global_prefix);
            }

            res.extend(import_path);
            res.extend_from_slice(ident);

            return Some(res);
        }

        if self.is_top_level_ident(ident.first()?) {
            let mut x = Vec::new();
            x.extend_from_slice(prefix);
            x.extend_from_slice(ident);
            return Some(x);
        }

        None
    }

    pub fn is_top_level_type(&self, ident: &String) -> bool {
        for i in 1..self.types.len() {
            if self.types[i].contains(ident) {
                return false;
            }
        }

        self.types
            .first()
            .map(|x| x.contains(ident))
            .unwrap_or(false)
    }

    pub fn is_top_level_ident(&self, ident: &String) -> bool {
        for i in 1..self.names.len() {
            if self.names[i].contains(ident) {
                return false;
            }
        }

        self.names
            .first()
            .map(|x| x.contains(ident))
            .unwrap_or(false)
    }

    pub fn insert_ident(&mut self, ident: String) {
        let n = self.names.last_mut().unwrap();
        if !n.contains(&ident) {
            n.push(ident);
        }
    }

    pub fn insert_type(&mut self, type_name: String) {
        let t = self.types.last_mut().unwrap();
        if !t.contains(&type_name) {
            t.push(type_name);
        }
    }

    pub fn push_idents(&mut self) {
        self.names.push(vec![]);
    }

    pub fn pop_idents(&mut self) {
        if self.names.len() == 1 {
            panic!("Cant pop last env");
        }
        self.names.pop();
    }

    pub fn push_types(&mut self) {
        self.types.push(vec![]);
    }

    pub fn pop_types(&mut self) {
        if self.types.len() == 1 {
            panic!("Cant pop last env");
        }
        self.types.pop();
    }
}

pub fn mangle_type_expression(
    type_expr: &mut TypeExpr,
    prefix: &Vec<String>,
    mangle_env: &mut MangleEnv,
) {
    match type_expr {
        TypeExpr::TypeName(..) => panic!("type name shouldn't be here"),
        TypeExpr::RawTypeName(is_global, path, type_params) => {
            // TODO: type params

            if let Some(mangled) = mangle_env.mangle_type(*is_global, prefix, path) {
                *path = mangle_env.global_prefix.clone();
                path.extend(mangled);
            }

            if let Some(type_params) = type_params {
                for (type_param, _) in type_params {
                    mangle_type_expression(type_param, prefix, mangle_env);
                }
            }

            *type_expr = TypeExpr::TypeName(true, mangle(path), type_params.clone());
        }
        TypeExpr::Duck(Duck { fields }) => {
            for f in fields {
                mangle_type_expression(&mut f.type_expr.0, prefix, mangle_env);
            }
        }
        TypeExpr::Tuple(fields) => {
            for f in fields {
                mangle_type_expression(&mut f.0, prefix, mangle_env);
            }
        }
        TypeExpr::Fun(params, return_type) => {
            for (_, param_type) in params {
                mangle_type_expression(&mut param_type.0, prefix, mangle_env);
            }

            if let Some(return_type) = return_type {
                mangle_type_expression(&mut return_type.0, prefix, mangle_env);
            }
        }
        TypeExpr::Or(s) => {
            for t in s {
                mangle_type_expression(&mut t.0, prefix, mangle_env);
            }
        }
        TypeExpr::Array(t) => {
            mangle_type_expression(&mut t.0, prefix, mangle_env);
        }
        _ => {}
    }
}

pub fn mangle_value_expr(
    value_expr: &mut ValueExpr,
    global_prefix: &Vec<String>,
    prefix: &Vec<String>,
    mangle_env: &mut MangleEnv,
) {
    match value_expr {
        ValueExpr::Int(..)
        | ValueExpr::String(..)
        | ValueExpr::Bool(..)
        | ValueExpr::Float(..)
        | ValueExpr::Return(None)
        | ValueExpr::Tag(..)
        | ValueExpr::Char(..) => {}
        ValueExpr::Continue => {}
        ValueExpr::Break => {}
        ValueExpr::ArrayAccess(target, idx) => {
            mangle_value_expr(&mut target.0, global_prefix, prefix, mangle_env);
            mangle_value_expr(&mut idx.0, global_prefix, prefix, mangle_env);
        }
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
        } => {
            mangle_value_expr(&mut value_expr.0, global_prefix, prefix, mangle_env);
            for arm in arms {
                mangle_type_expression(&mut arm.type_case.0, prefix, mangle_env);
                mangle_env.push_idents();
                if let Some(identifier) = &arm.identifier_binding {
                    mangle_env.insert_ident(identifier.clone());
                }
                mangle_value_expr(&mut arm.value_expr.0, global_prefix, prefix, mangle_env);
                mangle_env.pop_idents();
            }

            if let Some(arm) = else_arm {
                mangle_type_expression(&mut arm.type_case.0, prefix, mangle_env);
                mangle_env.push_idents();
                if let Some(identifier) = &arm.identifier_binding {
                    mangle_env.insert_ident(identifier.clone());
                }
                mangle_value_expr(&mut arm.value_expr.0, global_prefix, prefix, mangle_env);
                mangle_env.pop_idents();
            }
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                if let ValFmtStringContents::Expr(e) = c {
                    mangle_value_expr(&mut e.0, global_prefix, prefix, mangle_env);
                }
            }
        }
        ValueExpr::Array(ty, exprs) => {
            if let Some(ty) = ty {
                mangle_type_expression(&mut ty.0, prefix, mangle_env);
            }

            for expr in exprs {
                mangle_value_expr(&mut expr.0, global_prefix, prefix, mangle_env);
            }
        }
        ValueExpr::InlineGo(t) => {
            let mut parser = Parser::new();
            parser
                .set_language(&tree_sitter_go::LANGUAGE.into())
                .expect("Couldn't set go grammar");

            let src = parser.parse(t.as_bytes(), None).unwrap();
            let root_node = src.root_node();

            fn trav(
                s: &Node,
                t: &[u8],
                e: &mut MangleEnv,
                out: &mut Vec<(tree_sitter::Range, String)>,
            ) {
                fn extract_all_ident(t: &[u8], n: &Node) -> Vec<(tree_sitter::Range, String)> {
                    if n.grammar_name() == "selector_expression" {
                        return vec![(n.range(), n.utf8_text(t).unwrap().to_string())];
                    }

                    if n.grammar_name() == "identifier" {
                        return vec![(n.range(), n.utf8_text(t).unwrap().to_string())];
                    }

                    let mut res = Vec::new();
                    for i in 0..n.child_count() {
                        let x = extract_all_ident(t, &n.child(i).unwrap().clone());
                        res.extend(x);
                    }

                    res
                }

                let declared_var_ident = match s.grammar_name() {
                    "short_var_declaration" => {
                        Some(s.child(0).unwrap().utf8_text(t).unwrap().to_string())
                    }
                    "var_declaration" => Some(
                        s.child(1)
                            .unwrap()
                            .child(0)
                            .unwrap()
                            .utf8_text(t)
                            .unwrap()
                            .to_string(),
                    ),
                    _ => None,
                };

                if s.grammar_name() == "expression_statement" {
                    let i = extract_all_ident(t, s);
                    out.extend(i);
                }

                // TODO: respect additional identifer scopes like blocks and lambdas
                if let Some(i) = declared_var_ident {
                    e.insert_ident(i);
                }

                for i in 0..s.child_count() {
                    trav(&s.child(i).unwrap(), t, e, out);
                }
            }

            let mut o = Vec::new();
            trav(&root_node, t.as_bytes(), mangle_env, &mut o);

            let mut translation = 0;
            for (range, ident) in o {
                let mangled_ident =
                    mangle_env.mangle_ident(false, prefix, std::slice::from_ref(&ident));

                if let Some(mangled_ident) = mangled_ident {
                    let mangled = mangle(&mangled_ident);
                    let size_diff = mangled.len() - ident.len();

                    t.drain((range.start_byte + translation)..(range.end_byte + translation));
                    t.insert_str(range.start_byte + translation, &mangled);

                    translation += size_diff;
                }
            }
        }
        ValueExpr::Lambda(lambda_expr) => {
            let LambdaFunctionExpr {
                params,
                return_type,
                value_expr,
            } = &mut **lambda_expr;
            for (_, param_type) in params {
                mangle_type_expression(&mut param_type.0, prefix, mangle_env);
            }
            if let Some(return_type) = return_type {
                mangle_type_expression(&mut return_type.0, prefix, mangle_env);
            }
            mangle_env.push_idents();
            mangle_value_expr(&mut value_expr.0, global_prefix, prefix, mangle_env);
            mangle_env.pop_idents();
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
        } => {
            // TODO: type params
            mangle_value_expr(&mut target.0, global_prefix, prefix, mangle_env);
            params.iter_mut().for_each(|param| {
                mangle_value_expr(&mut param.0, global_prefix, prefix, mangle_env)
            });
            if let Some(type_params) = type_params {
                for param in type_params {
                    mangle_type_expression(&mut param.0, prefix, mangle_env);
                }
            }
        }
        ValueExpr::RawVariable(is_global, path) => {
            if let Some(mangled) = mangle_env.mangle_ident(*is_global, prefix, path) {
                *path = mangled;
            }
            *value_expr = ValueExpr::Variable(true, mangle(path), None);
        }
        ValueExpr::Variable(..) => panic!("variable shouldn't be here. {value_expr:?}"),
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            mangle_value_expr(&mut condition.0, global_prefix, prefix, mangle_env);

            mangle_env.push_idents();
            mangle_value_expr(&mut then.0, global_prefix, prefix, mangle_env);
            mangle_env.pop_idents();

            if let Some(r#else) = r#else {
                mangle_env.push_idents();
                mangle_value_expr(&mut r#else.0, global_prefix, prefix, mangle_env);
            }
        }
        ValueExpr::While { condition, body } => {
            mangle_value_expr(&mut condition.0, global_prefix, prefix, mangle_env);
            mangle_env.push_idents();
            mangle_value_expr(&mut body.0, global_prefix, prefix, mangle_env);
            mangle_env.pop_idents();
        }
        ValueExpr::Tuple(value_exprs) => value_exprs.iter_mut().for_each(|value_expr| {
            mangle_value_expr(&mut value_expr.0, global_prefix, prefix, mangle_env)
        }),
        ValueExpr::Block(value_exprs) => {
            mangle_env.push_idents();
            value_exprs.iter_mut().for_each(|value_expr| {
                mangle_value_expr(&mut value_expr.0, global_prefix, prefix, mangle_env)
            });
            mangle_env.pop_idents();
        }
        ValueExpr::Duck(items) => items.iter_mut().for_each(|(_, value_expr)| {
            mangle_value_expr(&mut value_expr.0, global_prefix, prefix, mangle_env)
        }),
        ValueExpr::Struct {
            name,
            fields,
            type_params,
        } => {
            if let Some(mangled) = mangle_env.mangle_type(false, prefix, std::slice::from_ref(name))
            {
                let mut m = mangle_env.global_prefix.clone();
                m.extend(mangled);
                *name = mangle(&m);
            }

            fields.iter_mut().for_each(|(_, value_expr)| {
                mangle_value_expr(&mut value_expr.0, global_prefix, prefix, mangle_env)
            });

            if let Some(type_params) = type_params {
                for (g, _) in type_params {
                    mangle_type_expression(g, prefix, mangle_env);
                }
            }
        }
        ValueExpr::FieldAccess { target_obj, .. } => {
            mangle_value_expr(&mut target_obj.0, global_prefix, prefix, mangle_env);
        }
        ValueExpr::Return(Some(value_expr)) => {
            mangle_value_expr(&mut value_expr.0, global_prefix, prefix, mangle_env)
        }
        ValueExpr::VarAssign(assignment) => {
            mangle_value_expr(
                &mut assignment.0.target.0,
                global_prefix,
                prefix,
                mangle_env,
            );
            mangle_value_expr(
                &mut assignment.0.value_expr.0,
                global_prefix,
                prefix,
                mangle_env,
            );
        }
        ValueExpr::VarDecl(declaration) => {
            let declaration = &mut declaration.0;
            if let Some(type_expr) = &mut declaration.type_expr {
                mangle_type_expression(&mut type_expr.0, prefix, mangle_env);
            }

            mangle_env.insert_ident(declaration.name.clone());

            mangle_value_expr(&mut declaration.initializer.0, global_prefix, prefix, mangle_env);
        }
        ValueExpr::Add(lhs, rhs) => {
            mangle_value_expr(&mut lhs.0, global_prefix, prefix, mangle_env);
            mangle_value_expr(&mut rhs.0, global_prefix, prefix, mangle_env);
        }
        ValueExpr::Mul(lhs, rhs) => {
            mangle_value_expr(&mut lhs.0, global_prefix, prefix, mangle_env);
            mangle_value_expr(&mut rhs.0, global_prefix, prefix, mangle_env);
        }
        ValueExpr::Equals(lhs, rhs) => {
            mangle_value_expr(&mut lhs.0, global_prefix, prefix, mangle_env);
            mangle_value_expr(&mut rhs.0, global_prefix, prefix, mangle_env);
        }
        ValueExpr::BoolNegate(value_expr) => {
            mangle_value_expr(&mut value_expr.0, global_prefix, prefix, mangle_env);
        }
    }
}
