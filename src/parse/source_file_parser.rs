use std::{collections::HashMap, fs::File, path::PathBuf};

use chumsky::{input::BorrowInput, prelude::*};
use tree_sitter::{Node, Parser as TSParser};

use crate::{
    parse::{
        Context, SS, Spanned,
        component_parser::{TsxComponent, tsx_component_parser},
        function_parser::{FunctionDefintion, LambdaFunctionExpr, function_definition_parser},
        lexer::{Token, lex_parser},
        make_input, parse_failure,
        struct_parser::{StructDefinition, struct_definition_parser},
        type_parser::{Duck, TypeDefinition, TypeExpr, type_definition_parser},
        use_statement_parser::{Indicator, UseStatement, use_statement_parser},
        value_parser::{ValFmtStringContents, ValueExpr},
    },
    semantics::ident_mangler::{
        MangleEnv, mangle, mangle_tsx_component, mangle_type_expression, mangle_value_expr,
        unmangle,
    },
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceFile {
    pub function_definitions: Vec<FunctionDefintion>,
    pub type_definitions: Vec<TypeDefinition>,
    pub struct_definitions: Vec<StructDefinition>,
    pub use_statements: Vec<UseStatement>,
    pub sub_modules: Vec<(String, SourceFile)>,
    pub tsx_components: Vec<TsxComponent>,
}

#[derive(Debug, Clone)]
pub enum SourceUnit {
    Func(FunctionDefintion),
    Type(TypeDefinition),
    Component(TsxComponent),
    Struct(StructDefinition),
    Use(UseStatement),
    Module(String, SourceFile),
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
                components: s.tsx_components.iter().map(|x| x.name.clone()).collect(),
                imports: {
                    let mut imports = HashMap::new();
                    if with_std {
                        imports.insert("std".into(), (true, vec![]));
                    }
                    for u in &s.use_statements {
                        if let UseStatement::Regular(glob, v) = u {
                            let pre = v
                                .iter()
                                .take_while(|x| matches!(x, Indicator::Module(_)))
                                .map(|x| {
                                    let Indicator::Module(x) = x else { panic!() };
                                    x.to_string()
                                })
                                .collect::<Vec<_>>();
                            let last = v.last();
                            if let Some(Indicator::Symbols(sym)) = last {
                                for s in sym {
                                    imports.insert(s.clone(), (*glob, pre.clone()));
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

                for f in src.function_definitions {
                    mangle_env.insert_ident(f.name[prefix.len()..].to_string());
                    result.function_definitions.push(f);
                }

                for t in src.type_definitions {
                    mangle_env.insert_type(t.name[prefix.len()..].to_string());
                    result.type_definitions.push(t);
                }

                for struct_definition in src.struct_definitions {
                    mangle_env.insert_type(struct_definition.name[prefix.len()..].to_string());
                    result.struct_definitions.push(struct_definition);
                }

                for u in &src.use_statements {
                    if matches!(u, UseStatement::Go(..)) {
                        result.push_use(u);
                    }
                }
            }

            for u in &s.use_statements {
                if matches!(u, UseStatement::Go(..)) {
                    result.push_use(u);
                }
            }

            for func in &s.function_definitions {
                let mut f = func.clone();

                let mut p = Vec::new();
                p.extend_from_slice(prefix);
                p.push(f.name.clone());
                f.name = mangle(&p);

                if let Some(return_type) = &mut f.return_type {
                    mangle_type_expression(&mut return_type.0, prefix, &mut mangle_env);
                }
                mangle_env.push_idents();
                if let Some(params) = &mut f.params {
                    for (name, type_expr) in params {
                        mangle_type_expression(&mut type_expr.0, prefix, &mut mangle_env);
                        mangle_env.insert_ident(name.clone());
                    }
                }
                mangle_value_expr(&mut f.value_expr.0, global_prefix, prefix, &mut mangle_env);
                mangle_env.pop_idents();
                result.function_definitions.push(f);
            }

            for t in &s.type_definitions {
                let mut ty = t.clone();

                let mut p = Vec::new();
                p.extend_from_slice(prefix);
                p.push(ty.name.clone());
                ty.name = mangle(&p);
                mangle_type_expression(&mut ty.type_expression.0, prefix, &mut mangle_env);
                result.type_definitions.push(ty);
            }

            // todo(@Apfelfrosch): implement flatten for struct definitions
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

            for c in &s.tsx_components {
                // todo: mangle components in tsx
                let mut c = c.clone();
                mangle_tsx_component(&mut c, global_prefix, prefix, &mut mangle_env);
                result.tsx_components.push(c.clone());
            }

            result
        }

        let mut r = flatten0(self, global_prefix, &vec![], with_std);

        let mut mangle_env = MangleEnv {
            sub_mods: Vec::new(),
            global_prefix: global_prefix.clone(),
            components: r.tsx_components.iter().map(|x| x.name.clone()).collect(),
            imports: HashMap::new(),
            names: vec![
                r.function_definitions
                    .iter()
                    .map(|x| x.name.clone())
                    .collect::<Vec<_>>(),
            ],
            types: vec![
                r.type_definitions
                    .iter()
                    .map(|x| x.name.clone())
                    .chain(r.struct_definitions.iter().map(|x| x.name.clone()))
                    .collect::<Vec<_>>(),
            ],
        };

        for f in &mut r.function_definitions {
            let mut c = global_prefix.clone();
            c.extend(unmangle(&f.name));
            f.name = mangle(&c);

            for t in f.return_type.iter_mut().map(|x| &mut x.0).chain(
                f.params
                    .iter_mut()
                    .flat_map(|x| x.iter_mut().map(|x| &mut x.1.0)),
            ) {
                append_global_prefix_type_expr(t, &mut mangle_env);
            }

            append_global_prefix_value_expr(&mut f.value_expr.0, &mut mangle_env);
        }

        for t in &mut r.type_definitions {
            let mut c = global_prefix.clone();
            c.extend(unmangle(&t.name));
            t.name = mangle(&c);

            append_global_prefix_type_expr(&mut t.type_expression.0, &mut mangle_env);
        }

        for s in &mut r.struct_definitions {
            let mut c = global_prefix.clone();
            c.extend(unmangle(&s.name));
            s.name = mangle(&c);

            for m in &mut s.methods {
                for t in m.return_type.iter_mut().map(|x| &mut x.0).chain(
                    m.params
                        .iter_mut()
                        .flat_map(|x| x.iter_mut().map(|x| &mut x.1.0)),
                ) {
                    append_global_prefix_type_expr(t, &mut mangle_env);
                }

                append_global_prefix_value_expr(&mut m.value_expr.0, &mut mangle_env);
            }
        }

        for s in &mut r.tsx_components {
            let mut c = global_prefix.clone();
            c.extend(unmangle(&s.name));
            s.name = mangle(&c);
        }

        r
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
        TypeExpr::Fun(params, return_type) => {
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
        TypeExpr::Array(t) => {
            append_global_prefix_type_expr(&mut t.0, mangle_env);
        }
        _ => {}
    }
}

fn append_global_prefix_value_expr(value_expr: &mut ValueExpr, mangle_env: &mut MangleEnv) {
    match value_expr {
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
        ValueExpr::Array(ty, exprs) => {
            if let Some(ty) = ty {
                append_global_prefix_type_expr(&mut ty.0, mangle_env);
            }

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
                params,
                return_type,
                value_expr,
            } = &mut **lambda_expr;
            for (_, param_type) in params {
                append_global_prefix_type_expr(&mut param_type.0, mangle_env);
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
        } => {
            // TODO: type params
            append_global_prefix_value_expr(&mut target.0, mangle_env);
            params
                .iter_mut()
                .for_each(|param| append_global_prefix_value_expr(&mut param.0, mangle_env));
            if let Some(type_params) = type_params {
                for param in type_params {
                    append_global_prefix_type_expr(&mut param.0, mangle_env);
                }
            }
        }
        ValueExpr::RawVariable(..) => panic!("raw variable shouldn't be here"),
        ValueExpr::Variable(_, name, _) => {
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

            if let Some(type_params) = type_params {
                for (g, _) in type_params {
                    append_global_prefix_type_expr(g, mangle_env);
                }
            }
        }
        ValueExpr::FieldAccess { target_obj, .. } => {
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
            .fold(SourceFile::default(), |mut acc, (internal, x)| {
                if internal {
                    acc.function_definitions.extend(x.function_definitions);
                    acc.type_definitions.extend(x.type_definitions);
                    acc.sub_modules.extend(x.sub_modules);
                    acc.struct_definitions.extend(x.struct_definitions);
                }
                acc.use_statements.extend(x.use_statements);
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
            tsx_component_parser().map(SourceUnit::Component),
            struct_definition_parser(make_input.clone()).map(SourceUnit::Struct),
            function_definition_parser(make_input).map(SourceUnit::Func),
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
            let mut struct_definitions = Vec::new();
            let mut use_statements = Vec::new();
            let mut sub_modules = Vec::new();
            let mut tsx_components = Vec::new();

            for source_unit in source_units {
                use SourceUnit::*;
                match source_unit {
                    Func(def) => function_definitions.push(def),
                    Type(def) => type_definitions.push(def),
                    Struct(def) => struct_definitions.push(def),
                    Use(def) => use_statements.push(def),
                    Module(name, def) => sub_modules.push((name, def)),
                    Component(tsx_component) => tsx_components.push(tsx_component),
                }
            }

            SourceFile {
                function_definitions,
                type_definitions,
                struct_definitions,
                use_statements,
                sub_modules,
                tsx_components,
            }
        })
    })
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use chumsky::Parser;

    use crate::parse::{
        Field,
        component_parser::TsxComponent,
        function_parser::FunctionDefintion,
        lexer::lex_parser,
        make_input,
        source_file_parser::{SourceFile, source_file_parser},
        struct_parser::StructDefinition,
        type_parser::{Duck, TypeDefinition, TypeExpr},
        use_statement_parser::{Indicator, UseStatement},
        value_parser::{
            IntoBlock, ValueExpr, empty_range, source_file_into_empty_range,
            type_expr_into_empty_range, value_expr_into_empty_range,
        },
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
                "component MyComp() tsx {console.log('hallo, welt')}",
                SourceFile {
                    tsx_components: vec![TsxComponent {
                        name: "MyComp".to_string(),
                        props_type: TypeExpr::Duck(Duck { fields: vec![] }).into_empty_span(),
                        typescript_source: (
                            "console.log('hallo, welt')".to_string(),
                            empty_range(),
                        ),
                        is_server_component: false,
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
                                    type_expr: TypeExpr::String.into_empty_span(),
                                },
                                Field {
                                    name: "y".to_string(),
                                    type_expr: TypeExpr::Int.into_empty_span(),
                                },
                            ],
                        })
                        .into_empty_span(),
                        typescript_source: (
                            "console.log('hallo, welt')".to_string(),
                            empty_range(),
                        ),
                        is_server_component: false,
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
                                TypeExpr::String.into_empty_span(),
                            )],
                        })
                        .into_empty_span(),
                        generics: None,
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
                            TypeExpr::String.into_empty_span(),
                        )],
                        methods: vec![],
                        generics: None,
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
                "module abc {use test; module xyz { use lol; } fn abc() {} }",
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
                                vec![Indicator::Module("test".into())],
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
                            return_type: Some(TypeExpr::String.into_empty_span()),
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
                                TypeExpr::String.into_empty_span(),
                            )],
                        })
                        .into_empty_span(),
                        generics: None,
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
                value_expr_into_empty_range(&mut func.value_expr);
            }

            assert_eq!(expected, original, "{i}");
        }
    }
}
