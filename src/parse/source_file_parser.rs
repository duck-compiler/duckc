use std::{collections::HashMap, fs::File, path::PathBuf};

use chumsky::{input::BorrowInput, prelude::*};
use tree_sitter::{Node, Parser as TSParser};

use crate::{
    parse::{
        duckx_component_parser::{duckx_component_parser, DuckxComponent}, extensions_def_parser::{extensions_def_parser, ExtensionsDef}, function_parser::{function_definition_parser, FunctionDefintion, LambdaFunctionExpr}, lexer::{lex_parser, Token}, make_input, parse_failure, schema_def_parser::{self, SchemaDefinition}, struct_parser::{struct_definition_parser, StructDefinition}, test_parser::{test_parser, TestCase}, tsx_component_parser::{tsx_component_parser, TsxComponent}, type_parser::{type_definition_parser, Duck, TypeDefinition, TypeExpr}, use_statement_parser::{use_statement_parser, Indicator, UseStatement}, value_parser::{ValFmtStringContents, ValHtmlStringContents, ValueExpr}, Context, Spanned, SS
    },
    semantics::ident_mangler::{
        mangle, mangle_duckx_component, mangle_tsx_component, mangle_type_expression, mangle_value_expr, unmangle, MangleEnv
    },
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceFile {
    pub function_definitions: Vec<FunctionDefintion>,
    pub type_definitions: Vec<TypeDefinition>,
    pub struct_definitions: Vec<StructDefinition>,
    pub use_statements: Vec<UseStatement>,
    pub extensions_defs: Vec<ExtensionsDef>,
    pub sub_modules: Vec<(String, SourceFile)>,
    pub tsx_components: Vec<TsxComponent>,
    pub duckx_components: Vec<DuckxComponent>,
    pub test_cases: Vec<TestCase>,
    pub schema_defs: Vec<SchemaDefinition>
}

#[derive(Debug, Clone)]
pub enum SourceUnit {
    Func(FunctionDefintion),
    Schema(SchemaDefinition),
    Type(TypeDefinition),
    Extensions(ExtensionsDef),
    Component(TsxComponent),
    Template(DuckxComponent),
    Struct(StructDefinition),
    Use(UseStatement),
    Module(String, SourceFile),
    Test(Spanned<TestCase>),
}

impl SourceFile {
    pub fn push_use(&mut self, s: &UseStatement) {
        if !self.use_statements.contains(s) {
            self.use_statements.push(s.to_owned());
        }
    }

    pub fn flatten(&self, global_prefix: &Vec<String>, with_std: bool) -> SourceFile {
        fn flatten0(
            s: &SourceFile,
            global_prefix: &Vec<String>,
            prefix: &Vec<String>,
            with_std: bool,
        ) -> SourceFile {
            let mut mangle_env = MangleEnv {
                sub_mods: s.sub_modules.iter().map(|x| x.0.clone()).collect(),
                global_prefix: global_prefix.clone(),
                tsx_components: s.tsx_components.iter().map(|x| x.name.clone()).collect(),
                duckx_components: s.duckx_components.iter().map(|x| x.name.clone()).collect(),
                imports: {
                    let mut imports = HashMap::new();
                    if with_std {
                        imports.insert("std".into(), (true, vec![]));
                    }
                    for use_statement in &s.use_statements {
                        if let UseStatement::Regular(glob, segments) = use_statement {
                            let pre = segments
                                .iter()
                                .take_while(|x| matches!(x, Indicator::Module(_)))
                                .map(|x| {
                                    let Indicator::Module(x) = x else { panic!() };
                                    x.to_string()
                                })
                                .collect::<Vec<_>>();

                            let last = segments.last();
                            if let Some(Indicator::Symbols(symbols)) = last {
                                for symbol in symbols {
                                    imports.insert(symbol.clone(), (*glob, pre.clone()));
                                }
                            }
                        }
                    }

                    imports
                },
                names: vec![
                    s.function_definitions
                        .iter()
                        .map(|x| x.name.clone())
                        .collect::<Vec<_>>(),
                ],
                types: vec![
                    s.type_definitions
                        .iter()
                        .map(|x| x.name.clone())
                        .chain(s.struct_definitions.iter().map(|x| x.name.clone()))
                        .collect::<Vec<_>>(),
                ],
            };

            let mut result = SourceFile::default();

            for (name, sub_module) in &s.sub_modules {
                let mut p = Vec::new();
                p.extend_from_slice(prefix);
                p.push(name.to_owned());
                let src = flatten0(sub_module, global_prefix, &p, with_std);

                for function_definition in src.function_definitions {
                    mangle_env.insert_ident(function_definition.name[prefix.len()..].to_string());
                    result.function_definitions.push(function_definition);
                }

                for type_definition in src.type_definitions {
                    mangle_env.insert_type(type_definition.name[prefix.len()..].to_string());
                    result.type_definitions.push(type_definition);
                }

                for struct_definition in src.struct_definitions {
                    mangle_env.insert_type(struct_definition.name[prefix.len()..].to_string());
                    result.struct_definitions.push(struct_definition);
                }

                for schema_def in src.schema_defs {
                    mangle_env.insert_type(schema_def.name[prefix.len()..].to_string());
                    result.schema_defs.push(schema_def);
                }

                for tsx_component in src.tsx_components {
                    mangle_env.insert_ident(tsx_component.name[prefix.len()..].to_string());
                    result.tsx_components.push(tsx_component);
                }

                for duck_component in src.duckx_components {
                    mangle_env.insert_ident(duck_component.name[prefix.len()..].to_string());
                    result.duckx_components.push(duck_component);
                }

                for test_case in src.test_cases {
                    result.test_cases.push(test_case);
                }

                for extensions_def in src.extensions_defs {
                    result.extensions_defs.push(extensions_def.clone())
                }

                for use_statement in &src.use_statements {
                    if matches!(use_statement, UseStatement::Go(..)) {
                        result.push_use(use_statement);
                    }
                }
            }

            for use_statement in &s.use_statements {
                if matches!(use_statement, UseStatement::Go(..)) {
                    result.push_use(use_statement);
                }
            }

            for func in &s.function_definitions {
                let mut func = func.clone();

                let mut p = Vec::new();
                p.extend_from_slice(prefix);
                p.push(func.name.clone());
                func.name = mangle(&p);

                if let Some(return_type) = &mut func.return_type {
                    mangle_type_expression(&mut return_type.0, prefix, &mut mangle_env);
                }

                mangle_env.push_idents();
                if let Some(params) = &mut func.params {
                    for (name, type_expr) in params {
                        mangle_type_expression(&mut type_expr.0, prefix, &mut mangle_env);
                        mangle_env.insert_ident(name.clone());
                    }
                }
                mangle_value_expr(
                    &mut func.value_expr.0,
                    global_prefix,
                    prefix,
                    &mut mangle_env,
                );
                mangle_env.pop_idents();
                result.function_definitions.push(func);
            }

            for extensions_def in &s.extensions_defs {
                let mut extensions_result = extensions_def.clone();
                extensions_result.function_definitions = vec![];
                for func in &extensions_def.function_definitions {
                    let mut func = func.clone();

                    if let Some(return_type) = &mut func.0.return_type {
                        mangle_type_expression(&mut return_type.0, prefix, &mut mangle_env);
                    }

                    mangle_env.push_idents();
                    if let Some(params) = &mut func.0.params {
                        for (name, type_expr) in params {
                            mangle_type_expression(&mut type_expr.0, prefix, &mut mangle_env);
                            mangle_env.insert_ident(name.clone());
                        }
                    }
                    mangle_value_expr(
                        &mut func.0.value_expr.0,
                        global_prefix,
                        prefix,
                        &mut mangle_env,
                    );
                    mangle_env.pop_idents();
                    extensions_result.function_definitions.push(func)
                }

                result.extensions_defs.push(extensions_result)
            }

            for type_definition in &s.type_definitions {
                let mut ty = type_definition.clone();

                let mut p = Vec::new();
                p.extend_from_slice(prefix);
                p.push(ty.name.clone());

                ty.name = mangle(&p);
                mangle_type_expression(&mut ty.type_expression.0, prefix, &mut mangle_env);
                result.type_definitions.push(ty);
            }

            // todo(@Apfelfrosch): implement flatten for struct definitions
            // can this be deleted?
            for struct_def in &s.struct_definitions {
                let mut struct_def = struct_def.clone();

                let mut new_name = Vec::new();
                new_name.extend_from_slice(prefix);
                new_name.push(struct_def.name.clone());

                struct_def.name = mangle(&new_name);

                for field in &mut struct_def.fields {
                    mangle_type_expression(&mut field.type_expr.0, prefix, &mut mangle_env);
                }

                for func in &mut struct_def.methods {
                    if let Some(return_type) = &mut func.return_type {
                        mangle_type_expression(&mut return_type.0, prefix, &mut mangle_env);
                    }
                    mangle_env.push_idents();
                    if let Some(params) = &mut func.params {
                        for (name, type_expr) in params {
                            mangle_type_expression(&mut type_expr.0, prefix, &mut mangle_env);
                            mangle_env.insert_ident(name.clone());
                        }
                    }
                    mangle_value_expr(
                        &mut func.value_expr.0,
                        global_prefix,
                        prefix,
                        &mut mangle_env,
                    );
                    mangle_env.pop_idents();
                }

                result.struct_definitions.push(struct_def);
            }

            for schema_def in &s.schema_defs {
                let mut schema_def = schema_def.clone();

                let mut new_name = Vec::new();
                new_name.extend_from_slice(prefix);
                new_name.push(schema_def.name.clone());

                // schema_def.name = mangle(&new_name);

                for schema_field in &mut schema_def.fields {
                    mangle_type_expression(&mut schema_field.type_expr.0, prefix, &mut mangle_env);
                    if let Some(branch) = &mut schema_field.if_branch {
                        mangle_value_expr(
                            &mut branch.0.condition.0,
                            global_prefix,
                            prefix,
                            &mut mangle_env
                        );

                        if let Some(value_expr) = &mut branch.0.value_expr {
                            mangle_value_expr(
                                &mut value_expr.0,
                                global_prefix,
                                prefix,
                                &mut mangle_env
                            );
                        }
                    }

                    if let Some(value_expr) = &mut schema_field.else_branch_value_expr {
                        mangle_value_expr(
                            &mut value_expr.0,
                            global_prefix,
                            prefix,
                            &mut mangle_env
                        );
                    }
                }

                result.schema_defs.push(schema_def);
            }


            for component in &s.tsx_components {
                // todo: mangle components in tsx
                let mut component = component.clone();
                mangle_tsx_component(&mut component, global_prefix, prefix, &mut mangle_env);
                result.tsx_components.push(component.clone());
            }

            for c in &s.duckx_components {
                // todo: mangle components in tsx
                let mut c = c.clone();
                mangle_duckx_component(&mut c, global_prefix, prefix, &mut mangle_env);
                result.duckx_components.push(c.clone());
            }

            for test_case in &s.test_cases {
                let mut test_case = test_case.clone();
                mangle_value_expr(
                    &mut test_case.body.0,
                    global_prefix,
                    prefix,
                    &mut mangle_env,
                );
                result.test_cases.push(test_case)
            }

            result
        }

        let mut flattened_source_file = flatten0(self, global_prefix, &vec![], with_std);

        let mut mangle_env = MangleEnv {
            sub_mods: Vec::new(),
            global_prefix: global_prefix.clone(),
            tsx_components: flattened_source_file
                .tsx_components
                .iter()
                .map(|x| x.name.clone())
                .collect(),
            duckx_components: flattened_source_file
                .duckx_components
                .iter()
                .map(|x| x.name.clone())
                .collect(),
            imports: HashMap::new(),
            names: vec![
                flattened_source_file
                    .function_definitions
                    .iter()
                    .map(|x| x.name.clone())
                    .collect::<Vec<_>>(),
            ],
            types: vec![
                flattened_source_file
                    .type_definitions
                    .iter()
                    .map(|x| x.name.clone())
                    .chain(
                        flattened_source_file
                            .struct_definitions
                            .iter()
                            .map(|x| x.name.clone()),
                    )
                    .collect::<Vec<_>>(),
            ],
        };

        for function_definition in &mut flattened_source_file.function_definitions {
            let mut c = global_prefix.clone();
            c.extend(unmangle(&function_definition.name));
            function_definition.name = mangle(&c);

            for type_expr in function_definition
                .return_type
                .iter_mut()
                .map(|type_expr| &mut type_expr.0)
                .chain(
                    function_definition
                        .params
                        .iter_mut()
                        .flat_map(|x| x.iter_mut().map(|x| &mut x.1.0)),
                )
            {
                append_global_prefix_type_expr(type_expr, &mut mangle_env);
            }

            append_global_prefix_value_expr(&mut function_definition.value_expr.0, &mut mangle_env);
        }

        for type_definition in &mut flattened_source_file.type_definitions {
            let mut p = global_prefix.clone();
            p.extend(unmangle(&type_definition.name));
            type_definition.name = mangle(&p);

            append_global_prefix_type_expr(&mut type_definition.type_expression.0, &mut mangle_env);
        }

        for struct_definition in &mut flattened_source_file.struct_definitions {
            let mut c = global_prefix.clone();
            c.extend(unmangle(&struct_definition.name));
            struct_definition.name = mangle(&c);

            for method in &mut struct_definition.methods {
                for type_expr in method
                    .return_type
                    .iter_mut()
                    .map(|type_expr| &mut type_expr.0)
                    .chain(
                        method
                            .params
                            .iter_mut()
                            .flat_map(|x| x.iter_mut().map(|x| &mut x.1.0)),
                    )
                {
                    append_global_prefix_type_expr(type_expr, &mut mangle_env);
                }

                append_global_prefix_value_expr(&mut method.value_expr.0, &mut mangle_env);
            }
        }

        for component in &mut flattened_source_file.duckx_components {
            let mut p = global_prefix.clone();

            append_global_prefix_type_expr(&mut component.props_type.0, &mut mangle_env);
            append_global_prefix_value_expr(&mut component.value_expr.0, &mut mangle_env);

            p.extend(unmangle(&component.name));
            component.name = mangle(&p);
        }

        for tsx_component in &mut flattened_source_file.tsx_components {
            let mut c = global_prefix.clone();
            c.extend(unmangle(&tsx_component.name));
            tsx_component.name = mangle(&c);
        }

        for test_case in &mut flattened_source_file.test_cases {
            append_global_prefix_value_expr(&mut test_case.body.0, &mut mangle_env);
        }

        for ext_def in &mut flattened_source_file.extensions_defs {
            for def in &mut ext_def.function_definitions {
                for t in def
                    .0
                    .return_type
                    .as_mut()
                    .into_iter()
                    .map(|(x, _)| x)
                    .chain(
                        def.0
                            .params
                            .as_mut()
                            .into_iter()
                            .flat_map(|v| v.iter_mut())
                            .map(|(_, y)| &mut y.0),
                    )
                {
                    append_global_prefix_type_expr(t, &mut mangle_env);
                }

                append_global_prefix_value_expr(&mut def.0.value_expr.0, &mut mangle_env);
            }
        }

        flattened_source_file
    }
}

fn append_global_prefix_type_expr(type_expr: &mut TypeExpr, mangle_env: &mut MangleEnv) {
    match type_expr {
        TypeExpr::TypeName(_, name, _) => {
            if mangle_env.is_top_level_type(name) {
                let mut v = Vec::new();
                v.extend_from_slice(&mangle_env.global_prefix);
                v.extend(unmangle(name));
                *name = mangle(&v);
            }
        }
        TypeExpr::RawTypeName(..) => panic!("raw type name shouldnt be here"),
        TypeExpr::Duck(Duck { fields }) => {
            for f in fields {
                append_global_prefix_type_expr(&mut f.type_expr.0, mangle_env);
            }
        }
        TypeExpr::Tuple(fields) => {
            for f in fields {
                append_global_prefix_type_expr(&mut f.0, mangle_env);
            }
        }
        TypeExpr::Fun(params, return_type, _) => {
            for (_, param_type) in params {
                append_global_prefix_type_expr(&mut param_type.0, mangle_env);
            }

            if let Some(return_type) = return_type {
                append_global_prefix_type_expr(&mut return_type.0, mangle_env);
            }
        }
        TypeExpr::Or(s) => {
            for t in s {
                append_global_prefix_type_expr(&mut t.0, mangle_env);
            }
        }
        TypeExpr::Array(t) | TypeExpr::Ref(t) | TypeExpr::RefMut(t) => {
            append_global_prefix_type_expr(&mut t.0, mangle_env);
        }
        _ => {}
    }
}

fn append_global_prefix_value_expr(value_expr: &mut ValueExpr, mangle_env: &mut MangleEnv) {
    match value_expr {
        ValueExpr::Defer(d) => append_global_prefix_value_expr(&mut d.0, mangle_env),
        ValueExpr::As(v, t) => {
            append_global_prefix_type_expr(&mut t.0, mangle_env);
            append_global_prefix_value_expr(&mut v.0, mangle_env);
        }
        ValueExpr::For {
            ident: _,
            target,
            block,
        } => {
            append_global_prefix_value_expr(&mut target.0, mangle_env);
            append_global_prefix_value_expr(&mut block.0, mangle_env);
        }
        ValueExpr::Deref(v) | ValueExpr::Ref(v) | ValueExpr::RefMut(v) => {
            append_global_prefix_value_expr(&mut v.0, mangle_env)
        }
        ValueExpr::HtmlString(contents) => {
            for c in contents {
                if let ValHtmlStringContents::Expr(e) = c {
                    append_global_prefix_value_expr(&mut e.0, mangle_env);
                }
            }
        }
        ValueExpr::Int(..)
        | ValueExpr::String(..)
        | ValueExpr::Bool(..)
        | ValueExpr::Float(..)
        | ValueExpr::Tag(..)
        | ValueExpr::Return(None)
        | ValueExpr::Char(..) => {}
        ValueExpr::Continue => {}
        ValueExpr::Break => {}
        ValueExpr::ArrayAccess(target, idx) => {
            append_global_prefix_value_expr(&mut target.0, mangle_env);
            append_global_prefix_value_expr(&mut idx.0, mangle_env);
        }
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
            span: _,
        } => {
            append_global_prefix_value_expr(&mut value_expr.0, mangle_env);
            for arm in arms {
                append_global_prefix_type_expr(&mut arm.type_case.0, mangle_env);
                mangle_env.push_idents();
                if let Some(identifier) = &arm.identifier_binding {
                    mangle_env.insert_ident(identifier.clone());
                }
                append_global_prefix_value_expr(&mut arm.value_expr.0, mangle_env);
                mangle_env.pop_idents();
            }

            if let Some(arm) = else_arm {
                append_global_prefix_type_expr(&mut arm.type_case.0, mangle_env);
                mangle_env.push_idents();
                if let Some(identifier) = &arm.identifier_binding {
                    mangle_env.insert_ident(identifier.clone());
                }
                append_global_prefix_value_expr(&mut arm.value_expr.0, mangle_env);
                mangle_env.pop_idents();
            }
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                if let ValFmtStringContents::Expr(e) = c {
                    append_global_prefix_value_expr(&mut e.0, mangle_env);
                }
            }
        }
        ValueExpr::Array(exprs) => {
            for expr in exprs {
                append_global_prefix_value_expr(&mut expr.0, mangle_env);
            }
        }
        ValueExpr::InlineGo(t) => {
            let mut parser = TSParser::new();
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
                if mangle_env.is_top_level_ident(&ident) {
                    let mut v = Vec::new();
                    v.extend_from_slice(&mangle_env.global_prefix);
                    v.push(ident.clone());
                    let mangled = mangle(&v);
                    let size_diff = mangled.len() - ident.len();

                    t.drain((range.start_byte + translation)..(range.end_byte + translation));
                    t.insert_str(range.start_byte + translation, &mangled);

                    translation += size_diff;
                }
            }
        }
        ValueExpr::Lambda(lambda_expr) => {
            let LambdaFunctionExpr {
                is_mut: _,
                params,
                return_type,
                value_expr,
            } = &mut **lambda_expr;
            for (_, param_type) in params {
                if let Some(param_type) = param_type.as_mut() {
                    append_global_prefix_type_expr(&mut param_type.0, mangle_env);
                }
            }
            if let Some(return_type) = return_type {
                append_global_prefix_type_expr(&mut return_type.0, mangle_env);
            }
            mangle_env.push_idents();
            append_global_prefix_value_expr(&mut value_expr.0, mangle_env);
            mangle_env.pop_idents();
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
            ..
        } => {
            // TODO: type params
            append_global_prefix_value_expr(&mut target.0, mangle_env);
            params
                .iter_mut()
                .for_each(|param| append_global_prefix_value_expr(&mut param.0, mangle_env));
            for param in type_params {
                append_global_prefix_type_expr(&mut param.0, mangle_env);
            }
        }
        ValueExpr::RawVariable(..) => panic!("raw variable shouldn't be here"),
        ValueExpr::Variable(_, name, _, _) => {
            if mangle_env.is_top_level_ident(name) {
                let mut v = Vec::new();
                v.extend_from_slice(&mangle_env.global_prefix);
                v.push(name.clone());
                *name = mangle(&v);
            }
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            append_global_prefix_value_expr(&mut condition.0, mangle_env);

            mangle_env.push_idents();
            append_global_prefix_value_expr(&mut then.0, mangle_env);
            mangle_env.pop_idents();

            if let Some(r#else) = r#else {
                mangle_env.push_idents();
                append_global_prefix_value_expr(&mut r#else.0, mangle_env);
            }
        }
        ValueExpr::While { condition, body } => {
            append_global_prefix_value_expr(&mut condition.0, mangle_env);
            mangle_env.push_idents();
            append_global_prefix_value_expr(&mut body.0, mangle_env);
            mangle_env.pop_idents();
        }
        ValueExpr::Tuple(value_exprs) => value_exprs
            .iter_mut()
            .for_each(|value_expr| append_global_prefix_value_expr(&mut value_expr.0, mangle_env)),
        ValueExpr::Block(value_exprs) => {
            mangle_env.push_idents();
            value_exprs.iter_mut().for_each(|value_expr| {
                append_global_prefix_value_expr(&mut value_expr.0, mangle_env)
            });
            mangle_env.pop_idents();
        }
        ValueExpr::Duck(items) => items.iter_mut().for_each(|(_, value_expr)| {
            append_global_prefix_value_expr(&mut value_expr.0, mangle_env)
        }),
        ValueExpr::Struct {
            name,
            fields,
            type_params,
        } => {
            if mangle_env.is_top_level_type(name) {
                let mut v = Vec::new();
                v.extend_from_slice(&mangle_env.global_prefix);
                v.push(name.clone());
                *name = mangle(&v);
            }

            fields.iter_mut().for_each(|(_, value_expr)| {
                append_global_prefix_value_expr(&mut value_expr.0, mangle_env)
            });

            for (g, _) in type_params {
                append_global_prefix_type_expr(g, mangle_env);
            }
        }
        ValueExpr::FieldAccess { target_obj, .. } => {
            append_global_prefix_value_expr(&mut target_obj.0, mangle_env);
        }
        ValueExpr::ExtensionAccess { target_obj, .. } => {
            append_global_prefix_value_expr(&mut target_obj.0, mangle_env);
        }
        ValueExpr::Return(Some(value_expr)) => {
            append_global_prefix_value_expr(&mut value_expr.0, mangle_env)
        }
        ValueExpr::VarAssign(assignment) => {
            append_global_prefix_value_expr(&mut assignment.0.target.0, mangle_env);
            append_global_prefix_value_expr(&mut assignment.0.value_expr.0, mangle_env);
        }
        ValueExpr::VarDecl(declaration) => {
            let declaration = &mut declaration.0;
            if let Some(type_expr) = &mut declaration.type_expr {
                append_global_prefix_type_expr(&mut type_expr.0, mangle_env);
            }
            mangle_env.insert_ident(declaration.name.clone());

            append_global_prefix_value_expr(&mut declaration.initializer.0, mangle_env);
        }
        ValueExpr::Add(lhs, rhs) => {
            append_global_prefix_value_expr(&mut lhs.0, mangle_env);
            append_global_prefix_value_expr(&mut rhs.0, mangle_env);
        }
        ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Or(lhs, rhs)
        | ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs)
        | ValueExpr::Mul(lhs, rhs) => {
            append_global_prefix_value_expr(&mut lhs.0, mangle_env);
            append_global_prefix_value_expr(&mut rhs.0, mangle_env);
        }
        ValueExpr::BoolNegate(value_expr) => {
            append_global_prefix_value_expr(&mut value_expr.0, mangle_env);
        }
    }
}

fn module_descent(name: String, current_dir: PathBuf) -> SourceFile {
    let joined = current_dir.join(&name);
    let mod_dir = File::open(&joined);
    if let Ok(mod_dir) = mod_dir
        && mod_dir.metadata().unwrap().is_dir()
    {
        std::fs::read_dir(&joined)
            .unwrap()
            .filter_map(|dir_entry| match dir_entry {
                Ok(dir_entry)
                    if dir_entry.metadata().unwrap().is_dir()
                        || dir_entry.file_name().to_string_lossy().ends_with(".duck") =>
                {
                    Some(dir_entry)
                }
                _ => None,
            })
            .map(|dir_entry| {
                (
                    dir_entry.metadata().unwrap().is_file(),
                    module_descent(
                        dir_entry
                            .file_name()
                            .into_string()
                            .unwrap()
                            .split(".duck")
                            .next()
                            .unwrap()
                            .into(),
                        joined.clone(),
                    ),
                )
            })
            .fold(SourceFile::default(), |mut acc, (internal, parsed_src)| {
                if internal {
                    acc.function_definitions
                        .extend(parsed_src.function_definitions);
                    acc.type_definitions.extend(parsed_src.type_definitions);
                    acc.sub_modules.extend(parsed_src.sub_modules);
                    acc.struct_definitions.extend(parsed_src.struct_definitions);
                    acc.test_cases.extend(parsed_src.test_cases);
                    acc.extensions_defs.extend(parsed_src.extensions_defs);
                }
                acc.use_statements.extend(parsed_src.use_statements);
                acc
            })
    } else {
        let src_text = std::fs::read_to_string(format!("{}.duck", joined.to_str().unwrap()))
            .unwrap_or_else(|_| panic!("{}", joined.to_str().unwrap().to_string()))
            .leak() as &'static str;
        let target_path = joined.to_string_lossy();
        let target_path_leaked = target_path.to_string().leak() as &str;
        let (lex, lex_errors) = lex_parser(target_path_leaked, src_text)
            .parse(src_text)
            .into_output_errors();

        lex_errors.into_iter().for_each(|e| {
            parse_failure(
                &target_path,
                &Rich::<&str, SS>::custom(
                    SS {
                        start: e.span().start,
                        end: e.span().end,
                        context: Context {
                            file_name: target_path_leaked,
                            file_contents: src_text,
                        },
                    },
                    "Lex Error",
                ),
                src_text,
            );
        });

        let lex = lex.unwrap();
        let (parse, parse_errors) = source_file_parser(current_dir.clone(), make_input)
            .parse(make_input(
                SS {
                    start: 0,
                    end: src_text.len(),
                    context: Context {
                        file_name: target_path_leaked,
                        file_contents: src_text,
                    },
                },
                &lex,
            ))
            .into_output_errors();

        parse_errors.into_iter().for_each(|e| {
            parse_failure(&target_path, &e, src_text);
        });

        parse.unwrap()
    }
}

pub fn source_file_parser<'src, I, M>(
    p: PathBuf,
    make_input: M,
) -> impl Parser<'src, I, SourceFile, extra::Err<Rich<'src, Token, SS>>>
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    let p = Box::leak(Box::new(p));
    recursive(|e| {
        choice((
            use_statement_parser().map(SourceUnit::Use),
            type_definition_parser().map(SourceUnit::Type),
            extensions_def_parser(make_input.clone()).map(SourceUnit::Extensions),
            tsx_component_parser().map(SourceUnit::Component),
            duckx_component_parser(make_input.clone()).map(SourceUnit::Template),
            struct_definition_parser(make_input.clone()).map(SourceUnit::Struct),
            function_definition_parser(make_input.clone()).map(SourceUnit::Func),
            test_parser(make_input.clone()).map(SourceUnit::Test),
            schema_def_parser::schema_definition_parser(make_input).map(SourceUnit::Schema),
            just(Token::Module)
                .ignore_then(select_ref! { Token::Ident(i) => i.to_owned() })
                .then(choice((
                    just(Token::ControlChar(';')).to(None),
                    e.clone()
                        .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
                        .map(Some),
                )))
                .map(|(name, src)| {
                    if let Some(src) = src {
                        SourceUnit::Module(name, src)
                    } else {
                        SourceUnit::Module(name.clone(), module_descent(name.clone(), p.clone()))
                    }
                }),
        ))
        .repeated()
        .collect::<Vec<_>>()
        .map(|source_units| {
            let mut function_definitions = Vec::new();
            let mut type_definitions = Vec::new();
            let mut extensions_defs = Vec::new();
            let mut struct_definitions = Vec::new();
            let mut use_statements = Vec::new();
            let mut sub_modules = Vec::new();
            let mut tsx_components = Vec::new();
            let mut template_components = Vec::new();
            let mut test_cases = Vec::new();
            let mut schema_defs = Vec::new();

            for source_unit in source_units {
                use SourceUnit::*;
                match source_unit {
                    Func(def) => function_definitions.push(def),
                    Type(def) => type_definitions.push(def),
                    Extensions(def) => extensions_defs.push(def),
                    Struct(def) => struct_definitions.push(def),
                    Use(def) => use_statements.push(def),
                    Module(name, def) => sub_modules.push((name, def)),
                    Component(tsx_component) => tsx_components.push(tsx_component),
                    Template(duckx_component) => template_components.push(duckx_component),
                    Test(test_case) => test_cases.push(test_case.0),
                    Schema(schema_def) => schema_defs.push(schema_def),
                }
            }

            SourceFile {
                function_definitions,
                type_definitions,
                extensions_defs,
                struct_definitions,
                use_statements,
                sub_modules,
                tsx_components,
                duckx_components: template_components,
                test_cases,
                schema_defs,
            }
        })
    })
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, path::PathBuf};

    use chumsky::Parser;

    use crate::parse::{
        function_parser::FunctionDefintion, lexer::lex_parser, make_input, schema_def_parser::{IfBranch, SchemaDefinition, SchemaField}, source_file_parser::{source_file_parser, SourceFile}, struct_parser::StructDefinition, tsx_component_parser::TsxComponent, type_parser::{Duck, TypeDefinition, TypeExpr}, use_statement_parser::{Indicator, UseStatement}, value_parser::{
            empty_range, source_file_into_empty_range, type_expr_into_empty_range, value_expr_into_empty_range, IntoBlock, ValueExpr
        }, Field
    };

    #[test]
    fn do_test() {
        let test_cases = vec![
            (
                "fn abc(){}",
                SourceFile {
                    function_definitions: vec![FunctionDefintion {
                        name: "abc".into(),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
            ),
            (
                "schema Yoo = { name: String if true }",
                SourceFile {
                    schema_defs: vec![SchemaDefinition {
                        name: "Yoo".into(),
                        fields: vec![SchemaField {
                            name: "name".to_string(),
                            type_expr: (TypeExpr::String(None), empty_range()),
                            if_branch: Some((IfBranch {
                                condition: (ValueExpr::Bool(true), empty_range()),
                                value_expr: None,
                            }, empty_range())),
                            else_branch_value_expr: None,
                            span: empty_range()
                        }],
                        comments: vec![],
                        span: empty_range()
                    }],
                    ..Default::default()
                },
            ),
            (
                "component MyComp() tsx {console.log('hallo, welt')}",
                SourceFile {
                    tsx_components: vec![TsxComponent {
                        name: "MyComp".to_string(),
                        props_type: TypeExpr::Duck(Duck { fields: vec![] }).into_empty_span(),
                        typescript_source: (
                            "console.log('hallo, welt')".to_string(),
                            empty_range(),
                        ),
                    }],
                    ..Default::default()
                },
            ),
            (
                "component MyComp(props: {x: String, y: Int}) tsx {console.log('hallo, welt')}",
                SourceFile {
                    tsx_components: vec![TsxComponent {
                        name: "MyComp".to_string(),
                        props_type: TypeExpr::Duck(Duck {
                            fields: vec![
                                Field {
                                    name: "x".to_string(),
                                    type_expr: TypeExpr::String(None).into_empty_span(),
                                },
                                Field {
                                    name: "y".to_string(),
                                    type_expr: TypeExpr::Int(None).into_empty_span(),
                                },
                            ],
                        })
                        .into_empty_span(),
                        typescript_source: (
                            "console.log('hallo, welt')".to_string(),
                            empty_range(),
                        ),
                    }],
                    ..Default::default()
                },
            ),
            (
                "fn abc(){}fn xyz(){}",
                SourceFile {
                    function_definitions: vec![
                        FunctionDefintion {
                            name: "abc".into(),
                            ..Default::default()
                        },
                        FunctionDefintion {
                            name: "xyz".into(),
                            ..Default::default()
                        },
                    ],
                    ..Default::default()
                },
            ),
            (
                "use x;",
                SourceFile {
                    use_statements: vec![UseStatement::Regular(
                        false,
                        vec![Indicator::Module("x".into())],
                    )],
                    ..Default::default()
                },
            ),
            (
                "type X = {x: String};",
                SourceFile {
                    type_definitions: vec![TypeDefinition {
                        name: "X".into(),
                        type_expression: TypeExpr::Duck(Duck {
                            fields: vec![Field::new(
                                "x".into(),
                                TypeExpr::String(None).into_empty_span(),
                            )],
                        })
                        .into_empty_span(),
                        generics: vec![],
                    }],
                    ..Default::default()
                },
            ),
            (
                "struct X = {x: String};",
                SourceFile {
                    struct_definitions: vec![StructDefinition {
                        name: "X".into(),
                        fields: vec![Field::new(
                            "x".to_string(),
                            TypeExpr::String(None).into_empty_span(),
                        )],
                        methods: vec![],
                        mut_methods: HashSet::new(),
                        generics: vec![],
                        doc_comments: vec![],
                    }],
                    ..Default::default()
                },
            ),
            (
                "module abc {}",
                SourceFile {
                    sub_modules: vec![(
                        "abc".into(),
                        SourceFile {
                            ..Default::default()
                        },
                    )],
                    ..Default::default()
                },
            ),
            (
                "module abc {module xyz{}}",
                SourceFile {
                    sub_modules: vec![(
                        "abc".into(),
                        SourceFile {
                            sub_modules: vec![(
                                "xyz".into(),
                                SourceFile {
                                    ..Default::default()
                                },
                            )],
                            ..Default::default()
                        },
                    )],
                    ..Default::default()
                },
            ),
            (
                "module abc {use test_mod; module xyz { use lol; } fn abc() {} }",
                SourceFile {
                    sub_modules: vec![(
                        "abc".into(),
                        SourceFile {
                            sub_modules: vec![(
                                "xyz".into(),
                                SourceFile {
                                    use_statements: vec![UseStatement::Regular(
                                        false,
                                        vec![Indicator::Module("lol".into())],
                                    )],
                                    ..Default::default()
                                },
                            )],
                            use_statements: vec![UseStatement::Regular(
                                false,
                                vec![Indicator::Module("test_mod".into())],
                            )],
                            function_definitions: vec![FunctionDefintion {
                                name: "abc".into(),
                                ..Default::default()
                            }],
                            ..Default::default()
                        },
                    )],
                    ..Default::default()
                },
            ),
            (
                "use x;fn abc() -> String {}type X = {x: String};fn xyz(){}",
                SourceFile {
                    function_definitions: vec![
                        FunctionDefintion {
                            name: "abc".into(),
                            return_type: Some(TypeExpr::String(None).into_empty_span()),
                            ..Default::default()
                        },
                        FunctionDefintion {
                            name: "xyz".into(),
                            ..Default::default()
                        },
                    ],
                    use_statements: vec![UseStatement::Regular(
                        false,
                        vec![Indicator::Module("x".into())],
                    )],
                    type_definitions: vec![TypeDefinition {
                        name: "X".into(),
                        type_expression: TypeExpr::Duck(Duck {
                            fields: vec![Field::new(
                                "x".into(),
                                TypeExpr::String(None).into_empty_span(),
                            )],
                        })
                        .into_empty_span(),
                        generics: vec![],
                    }],
                    ..Default::default()
                },
            ),
        ];

        for (src, exp) in test_cases {
            let lex = lex_parser("test", "").parse(src).into_result().expect(src);
            let mut parse = source_file_parser(PathBuf::from("test_files"), make_input)
                .parse(make_input(empty_range(), &lex))
                .into_result()
                .expect(src);

            source_file_into_empty_range(&mut parse);

            for c in parse.tsx_components.iter_mut() {
                type_expr_into_empty_range(&mut c.props_type);
            }

            assert_eq!(parse, exp, "{src}");
        }
    }

    #[test]
    fn test_mod_structure() {
        let test_cases = vec![
            ("01.duck", SourceFile::default()),
            (
                "02.duck",
                SourceFile {
                    sub_modules: vec![
                        ("abc".into(), SourceFile::default()),
                        ("xyz".into(), SourceFile::default()),
                    ],
                    ..Default::default()
                },
            ),
            (
                "03.duck",
                SourceFile {
                    sub_modules: vec![
                        (
                            "abc".into(),
                            SourceFile {
                                sub_modules: vec![("lol".into(), SourceFile::default())],
                                ..SourceFile::default()
                            },
                        ),
                        (
                            "xyz".into(),
                            SourceFile {
                                sub_modules: vec![("foo".into(), SourceFile::default())],
                                ..SourceFile::default()
                            },
                        ),
                    ],
                    ..Default::default()
                },
            ),
            (
                "04.duck",
                SourceFile {
                    sub_modules: vec![("empty".into(), SourceFile::default())],
                    ..Default::default()
                },
            ),
            (
                "05.duck",
                SourceFile {
                    sub_modules: vec![
                        ("empty".into(), SourceFile::default()),
                        (
                            "single".into(),
                            SourceFile {
                                function_definitions: vec![FunctionDefintion {
                                    name: "my_single_fun".into(),
                                    ..Default::default()
                                }],
                                ..Default::default()
                            },
                        ),
                    ],
                    ..Default::default()
                },
            ),
            (
                "06.duck",
                SourceFile {
                    sub_modules: vec![(
                        "multiple".into(),
                        SourceFile {
                            function_definitions: vec![
                                FunctionDefintion {
                                    name: "some_abc_func".into(),
                                    value_expr: ValueExpr::String("Hello from module".into(), true)
                                        .into_empty_span()
                                        .into_block(),
                                    ..Default::default()
                                },
                                FunctionDefintion {
                                    name: "some_xyz_func".into(),
                                    value_expr: ValueExpr::Int(1).into_empty_span().into_block(),
                                    ..Default::default()
                                },
                            ],
                            ..Default::default()
                        },
                    )],
                    ..Default::default()
                },
            ),
            (
                "07.duck",
                SourceFile {
                    sub_modules: vec![
                        (
                            "multiple".into(),
                            SourceFile {
                                function_definitions: vec![
                                    FunctionDefintion {
                                        name: "some_abc_func".into(),
                                        value_expr: ValueExpr::String(
                                            "Hello from module".into(),
                                            true,
                                        )
                                        .into_empty_span_and_block(),
                                        ..Default::default()
                                    },
                                    FunctionDefintion {
                                        name: "some_xyz_func".into(),
                                        value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
                                        ..Default::default()
                                    },
                                ],
                                ..Default::default()
                            },
                        ),
                        (
                            "nested".into(),
                            SourceFile {
                                function_definitions: vec![FunctionDefintion {
                                    name: "hello_from_x".into(),
                                    ..Default::default()
                                }],
                                sub_modules: vec![(
                                    "level1".into(),
                                    SourceFile {
                                        function_definitions: vec![FunctionDefintion {
                                            name: "hello_from_y".into(),
                                            ..Default::default()
                                        }],
                                        sub_modules: vec![(
                                            "level2".into(),
                                            SourceFile {
                                                function_definitions: vec![FunctionDefintion {
                                                    name: "hello_from_z".into(),
                                                    ..Default::default()
                                                }],
                                                ..Default::default()
                                            },
                                        )],
                                        ..Default::default()
                                    },
                                )],
                                ..Default::default()
                            },
                        ),
                        ("empty".into(), SourceFile::default()),
                        ("another_mod".into(), SourceFile::default()),
                    ],
                    ..Default::default()
                },
            ),
            (
                "08.duck",
                SourceFile {
                    sub_modules: vec![
                        (
                            "multiple".into(),
                            SourceFile {
                                function_definitions: vec![
                                    FunctionDefintion {
                                        name: "some_abc_func".into(),
                                        value_expr: ValueExpr::String(
                                            "Hello from module".into(),
                                            true,
                                        )
                                        .into_empty_span_and_block(),
                                        ..Default::default()
                                    },
                                    FunctionDefintion {
                                        name: "some_xyz_func".into(),
                                        value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
                                        ..Default::default()
                                    },
                                ],
                                ..Default::default()
                            },
                        ),
                        (
                            "nested".into(),
                            SourceFile {
                                function_definitions: vec![FunctionDefintion {
                                    name: "hello_from_x".into(),
                                    ..Default::default()
                                }],
                                sub_modules: vec![(
                                    "level1".into(),
                                    SourceFile {
                                        function_definitions: vec![FunctionDefintion {
                                            name: "hello_from_y".into(),
                                            ..Default::default()
                                        }],
                                        sub_modules: vec![(
                                            "level2".into(),
                                            SourceFile {
                                                function_definitions: vec![FunctionDefintion {
                                                    name: "hello_from_z".into(),
                                                    ..Default::default()
                                                }],
                                                ..Default::default()
                                            },
                                        )],
                                        ..Default::default()
                                    },
                                )],
                                ..Default::default()
                            },
                        ),
                        (
                            "nested2".into(),
                            SourceFile {
                                function_definitions: vec![FunctionDefintion {
                                    name: "hello_from_x".into(),
                                    ..Default::default()
                                }],
                                sub_modules: vec![(
                                    "level1".into(),
                                    SourceFile {
                                        function_definitions: vec![FunctionDefintion {
                                            name: "hello_from_y".into(),
                                            ..Default::default()
                                        }],
                                        sub_modules: vec![(
                                            "level2".into(),
                                            SourceFile {
                                                function_definitions: vec![
                                                    FunctionDefintion {
                                                        name: "this_is_a_func".into(),
                                                        ..Default::default()
                                                    },
                                                    FunctionDefintion {
                                                        name: "this_is_another_func".into(),
                                                        ..Default::default()
                                                    },
                                                    FunctionDefintion {
                                                        name: "yet_another".into(),
                                                        ..Default::default()
                                                    },
                                                ],
                                                ..Default::default()
                                            },
                                        )],
                                        ..Default::default()
                                    },
                                )],
                                ..Default::default()
                            },
                        ),
                    ],
                    ..Default::default()
                },
            ),
        ];

        let dir = PathBuf::from("test_files").join("modules");

        for (main_file, mut expected) in test_cases {
            let src = std::fs::read_to_string(dir.join(main_file)).unwrap();
            let lex = lex_parser("test", "").parse(&src).unwrap();
            let mut got = source_file_parser(dir.clone(), make_input)
                .parse(make_input(empty_range(), &lex))
                .unwrap();
            source_file_into_empty_range(&mut got);
            fn sort_all(x: &mut SourceFile) {
                x.function_definitions.sort_by_key(|x| x.name.clone());
                for (_, s) in x.sub_modules.iter_mut() {
                    sort_all(s);
                }
            }

            sort_all(&mut expected);
            sort_all(&mut got);

            assert_eq!(expected, got, "{main_file}");
        }
    }

    #[test]
    fn test_flatten() {
        let test_cases = vec![
            (
                SourceFile::default().flatten(&vec![], false),
                SourceFile::default(),
            ),
            (
                SourceFile::default(),
                SourceFile {
                    sub_modules: vec![
                        ("abc".into(), SourceFile::default()),
                        ("xyz".into(), SourceFile::default()),
                    ],
                    ..Default::default()
                },
            ),
            // todo(@Apfelfrosch): respect the new way to create structs
            // (
            //     SourceFile {
            //         type_definitions: vec![TypeDefinition {
            //             name: mangle(&["abc", "TestStruct"]),
            //             type_expression: TypeExpr::Struct(StructDefinition {
            //                 name: "TestStruct",
            //                 fields: vec![Field {
            //                     name: "recv".into(),
            //                     type_expr: TypeExpr::TypeName(
            //                         false,
            //                         mangle(&["abc", "TestStruct"]),
            //                         None,
            //                     )
            //                     .into_empty_span(),
            //                 }],
            //                 methods: vec![],
            //                 generics: None,
            //             })
            //             .into_empty_span(),
            //             generics: None,
            //         }],
            //         use_statements: vec![UseStatement::Go("fmt".into(), None)],
            //         function_definitions: vec![
            //             FunctionDefintion {
            //                 name: mangle(&["abc", "lol", "im_a_func"]),
            //                 value_expr: ValueExpr::Block(vec![
            //                     ValueExpr::FunctionCall {
            //                         target: ValueExpr::Variable(
            //                             true,
            //                             mangle(&["abc", "lol", "called"]),
            //                             None,
            //                         )
            //                         .into_empty_span()
            //                         .into(),
            //                         params: vec![],
            //                         type_params: None,
            //                     }
            //                     .into_empty_span(),
            //                 ])
            //                 .into_empty_span(),
            //                 ..Default::default()
            //             },
            //             FunctionDefintion {
            //                 name: mangle(&["abc", "im_calling_a_sub_module"]),
            //                 value_expr: ValueExpr::Block(vec![
            //                     ValueExpr::Variable(true, mangle(&["abc", "lol", "called"]), None)
            //                         .into_empty_span(),
            //                 ])
            //                 .into_empty_span(),
            //                 ..Default::default()
            //             },
            //             FunctionDefintion {
            //                 name: mangle(&["abc", "lol", "called"]),
            //                 value_expr: ValueExpr::Block(vec![
            //                     ValueExpr::FunctionCall {
            //                         target: ValueExpr::Variable(
            //                             true,
            //                             mangle(&["abc", "lol", "called"]),
            //                             None,
            //                         )
            //                         .into_empty_span()
            //                         .into(),
            //                         params: vec![],
            //                         type_params: None,
            //                     }
            //                     .into_empty_span(),
            //                 ])
            //                 .into_empty_span(),
            //                 ..Default::default()
            //             },
            //         ],
            //         ..Default::default()
            //     },
            //     SourceFile {
            //         sub_modules: vec![
            //             (
            //                 "abc".into(),
            //                 SourceFile {
            //                     use_statements: vec![UseStatement::Regular(
            //                         false,
            //                         vec![
            //                             Indicator::Module("lol".into()),
            //                             Indicator::Symbols(vec!["called".into()]),
            //                         ],
            //                     )],
            //                     type_definitions: vec![TypeDefinition {
            //                         name: "TestStruct".into(),
            //                         type_expression: TypeExpr::Struct(StructDefinition {
            //                             fields: vec![Field {
            //
            //                                 name: "recv".into(),
            //                                 type_expr: TypeExpr::RawTypeName(
            //                                     false,
            //                                     vec!["TestStruct".into()],
            //                                     None,
            //                                 )
            //                                 .into_empty_span(),
            //                             }],
            //                         })
            //                         .into_empty_span(),
            //                         generics: None,
            //                     }],
            //                     function_definitions: vec![FunctionDefintion {
            //                         name: "im_calling_a_sub_module".into(),
            //                         value_expr: ValueExpr::Block(vec![
            //                             ValueExpr::RawVariable(false, vec!["called".into()])
            //                                 .into_empty_span(),
            //                         ])
            //                         .into_empty_span(),
            //                         ..Default::default()
            //                     }],
            //                     sub_modules: vec![(
            //                         "lol".into(),
            //                         SourceFile {
            //                             use_statements: vec![UseStatement::Go("fmt".into(), None)],
            //                             function_definitions: vec![
            //                                 FunctionDefintion {
            //                                     name: "im_a_func".into(),
            //                                     value_expr: ValueExpr::Block(vec![
            //                                         ValueExpr::FunctionCall {
            //                                             target: ValueExpr::RawVariable(
            //                                                 false,
            //                                                 vec!["called".into()],
            //                                             )
            //                                             .into_empty_span()
            //                                             .into(),
            //                                             params: vec![],
            //                                             type_params: None,
            //                                         }
            //                                         .into_empty_span(),
            //                                     ])
            //                                     .into_empty_span(),
            //                                     ..Default::default()
            //                                 },
            //                                 FunctionDefintion {
            //                                     name: "called".into(),
            //                                     value_expr: ValueExpr::Block(vec![
            //                                         ValueExpr::FunctionCall {
            //                                             target: ValueExpr::RawVariable(
            //                                                 false,
            //                                                 vec!["called".into()],
            //                                             )
            //                                             .into_empty_span()
            //                                             .into(),
            //                                             params: vec![],
            //                                             type_params: None,
            //                                         }
            //                                         .into_empty_span(),
            //                                     ])
            //                                     .into_empty_span(),
            //                                     ..Default::default()
            //                                 },
            //                             ],
            //                             ..SourceFile::default()
            //                         },
            //                     )],
            //                     ..SourceFile::default()
            //                 },
            //             ),
            //             (
            //                 "xyz".into(),
            //                 SourceFile {
            //                     sub_modules: vec![("foo".into(), SourceFile::default())],
            //                     ..SourceFile::default()
            //                 },
            //             ),
            //         ],
            //         ..Default::default()
            //     },
            // ),
            // (
            //     SourceFile::default(),
            //     SourceFile {
            //         sub_modules: vec![("empty".into(), SourceFile::default())],
            //         ..Default::default()
            //     },
            // ),
            // (
            //     SourceFile {
            //         function_definitions: vec![FunctionDefintion {
            //             name: "single_my_single_fun".into(),
            //             ..Default::default()
            //         }],
            //         ..Default::default()
            //     },
            //     SourceFile {
            //         sub_modules: vec![
            //             ("empty".into(), SourceFile::default()),
            //             (
            //                 "single".into(),
            //                 SourceFile {
            //                     function_definitions: vec![FunctionDefintion {
            //                         name: "my_single_fun".into(),
            //                         ..Default::default()
            //                     }],
            //                     ..Default::default()
            //                 },
            //             ),
            //         ],
            //         ..Default::default()
            //     },
            // ),
            // (
            //     SourceFile {
            //         function_definitions: vec![
            //             FunctionDefintion {
            //                 name: "multiple_some_abc_func".into(),
            //                 value_expr: ValueExpr::String("Hello from module".into())
            //                     .into_empty_span_and_block(),
            //                 ..Default::default()
            //             },
            //             FunctionDefintion {
            //                 name: "multiple_some_xyz_func".into(),
            //                 value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
            //                 ..Default::default()
            //             },
            //         ],
            //         ..Default::default()
            //     },
            //     SourceFile {
            //         sub_modules: vec![(
            //             "multiple".into(),
            //             SourceFile {
            //                 function_definitions: vec![
            //                     FunctionDefintion {
            //                         name: "some_abc_func".into(),
            //                         value_expr: ValueExpr::String("Hello from module".into())
            //                             .into_empty_span_and_block(),
            //                         ..Default::default()
            //                     },
            //                     FunctionDefintion {
            //                         name: "some_xyz_func".into(),
            //                         value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
            //                         ..Default::default()
            //                     },
            //                 ],
            //                 ..Default::default()
            //             },
            //         )],
            //         ..Default::default()
            //     },
            // ),
            // (
            //     SourceFile {
            //         function_definitions: vec![
            //             FunctionDefintion {
            //                 name: "multiple_some_abc_func".into(),
            //                 value_expr: ValueExpr::String("Hello from module".into())
            //                     .into_empty_span_and_block(),
            //                 ..Default::default()
            //             },
            //             FunctionDefintion {
            //                 name: "multiple_some_xyz_func".into(),
            //                 value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
            //                 ..Default::default()
            //             },
            //             FunctionDefintion {
            //                 name: "nested_hello_from_x".into(),
            //                 ..Default::default()
            //             },
            //             FunctionDefintion {
            //                 name: "nested_level1_hello_from_y".into(),
            //                 ..Default::default()
            //             },
            //             FunctionDefintion {
            //                 name: "nested_level1_level2_hello_from_z".into(),
            //                 ..Default::default()
            //             },
            //         ],
            //         ..Default::default()
            //     },
            //     SourceFile {
            //         sub_modules: vec![
            //             (
            //                 "multiple".into(),
            //                 SourceFile {
            //                     function_definitions: vec![
            //                         FunctionDefintion {
            //                             name: "some_abc_func".into(),
            //                             value_expr: ValueExpr::String("Hello from module".into())
            //                                 .into_empty_span_and_block(),
            //                             ..Default::default()
            //                         },
            //                         FunctionDefintion {
            //                             name: "some_xyz_func".into(),
            //                             value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
            //                             ..Default::default()
            //                         },
            //                     ],
            //                     ..Default::default()
            //                 },
            //             ),
            //             (
            //                 "nested".into(),
            //                 SourceFile {
            //                     function_definitions: vec![FunctionDefintion {
            //                         name: "hello_from_x".into(),
            //                         ..Default::default()
            //                     }],
            //                     sub_modules: vec![(
            //                         "level1".into(),
            //                         SourceFile {
            //                             function_definitions: vec![FunctionDefintion {
            //                                 name: "hello_from_y".into(),
            //                                 ..Default::default()
            //                             }],
            //                             sub_modules: vec![(
            //                                 "level2".into(),
            //                                 SourceFile {
            //                                     function_definitions: vec![FunctionDefintion {
            //                                         name: "hello_from_z".into(),
            //                                         ..Default::default()
            //                                     }],
            //                                     ..Default::default()
            //                                 },
            //                             )],
            //                             ..Default::default()
            //                         },
            //                     )],
            //                     ..Default::default()
            //                 },
            //             ),
            //             ("empty".into(), SourceFile::default()),
            //             ("another_mod".into(), SourceFile::default()),
            //         ],
            //         ..Default::default()
            //     },
            // ),
        ];

        for (i, (mut expected, mut original)) in test_cases.into_iter().enumerate() {
            fn sort_all(x: &mut SourceFile) {
                x.function_definitions.sort_by_key(|x| x.name.clone());
                x.type_definitions.sort_by_key(|x| x.name.clone());
                for (_, s) in x.sub_modules.iter_mut() {
                    sort_all(s);
                }
            }
            source_file_into_empty_range(&mut original);

            let mut original = original.flatten(&vec![], false);

            sort_all(&mut expected);
            sort_all(&mut original);

            for func in original.function_definitions.iter_mut() {
                func.span = empty_range();
                value_expr_into_empty_range(&mut func.value_expr);
            }

            assert_eq!(expected, original, "{i}");
        }
    }
}
