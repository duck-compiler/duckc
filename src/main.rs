#![feature(impl_trait_in_bindings)]
#![allow(
    clippy::needless_return,
    clippy::match_like_matches_macro,
    clippy::only_used_in_recursion,
    clippy::large_enum_variant
)]

use std::{
    env,
    error::Error,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    process,
    sync::mpsc::Sender,
};

use chumsky::{Parser, error::Rich};
use colored::Colorize;
use parse::{Spanned, lexer::Token, source_file_parser::SourceFile};
use tags::Tag;

use crate::{
    parse::{
        Context, SS,
        function_parser::LambdaFunctionExpr,
        lexer::lex_parser,
        make_input, parse_failure,
        source_file_parser::source_file_parser,
        type_parser::{Duck, TypeExpr},
        use_statement_parser::UseStatement,
        value_parser::{
            Assignment, Declaration, ValFmtStringContents, ValHtmlStringContents, ValueExpr,
        },
    },
    semantics::type_resolve::{self, TypeEnv},
};

use lazy_static::lazy_static;

pub mod cli;
pub mod dargo;
pub mod emit;
pub mod go_fixup;
pub mod parse;
pub mod reports;
pub mod semantics;
pub mod tags;

lazy_static! {
    static ref DUCK_STD_PATH: PathBuf = {
        env::home_dir()
            .map(|mut path| {
                path.push(".duck");
                path.push("std");
                path.push("std.duck");
                path
            })
            .expect("couldn't get pathbuf for std lib")
    };
    static ref DARGO_DOT_DIR: PathBuf = {
        fn require_sub_dir(str: &str) {
            let Ok(current_dir) = env::current_dir() else {
                println!("{}{} coulnd't read current dir", Tag::Dargo, Tag::Err,);
                panic!()
            };

            let required_dir = {
                let mut current_dir_clone = current_dir.clone();
                current_dir_clone.push(str);
                current_dir_clone
            };

            if required_dir.exists() {
                return;
            }

            if let Err(err) = fs::create_dir(required_dir.clone()) {
                println!(
                    "{}{} Couldn't create {} dot dir in current directory. - {err}",
                    Tag::Dargo,
                    Tag::Err,
                    required_dir.to_string_lossy()
                );
            }
        }

        let duck_dir = Path::new(".dargo");

        require_sub_dir(".dargo");
        require_sub_dir(".dargo/git");
        require_sub_dir(".dargo/project");

        return duck_dir.to_path_buf();
    };
}

// todo(@Mvmo): code doc generation, using doc comments
//      we already have the function lex_with_comments, right below this issue.
//      now the only thing we have to do is lex the code with comments and then generate some kind of report, maybe in json, which can be interpreted by some docs generator.
//      or maybe just generate the html docs directly
#[allow(dead_code)]
fn lex_with_comments(file_name: &'static str, file_contents: &'static str) -> Vec<Spanned<Token>> {
    let (lex, lex_errors) = lex_parser(file_name, file_contents)
        .parse(file_contents)
        .into_output_errors();

    lex_errors.into_iter().for_each(|e| {
        parse_failure(
            file_name,
            &Rich::<&str, SS>::custom(
                SS {
                    start: e.span().start,
                    end: e.span().end,
                    context: crate::parse::Context {
                        file_name,
                        file_contents,
                    },
                },
                format!("{}{} {}", Tag::Lexer, Tag::Err, e.reason()),
            ),
            file_contents,
        );
    });

    lex.unwrap()
}

fn lex(file_name: &'static str, file_contents: &'static str) -> Vec<Spanned<Token>> {
    let (lex, lex_errors) = lex_parser(file_name, file_contents)
        .parse(file_contents)
        .into_output_errors();

    lex_errors.into_iter().for_each(|e| {
        parse_failure(
            file_name,
            &Rich::<&str, SS>::custom(
                SS {
                    start: e.span().start,
                    end: e.span().end,
                    context: crate::parse::Context {
                        file_name,
                        file_contents,
                    },
                },
                format!("{}{} {}", Tag::Lexer, Tag::Err, e.reason()),
            ),
            file_contents,
        );
    });

    lex.unwrap()
        .iter()
        .filter(|(token, _)| !matches!(token, Token::Comment(..) | Token::DocComment(..)))
        .cloned()
        .collect::<Vec<_>>()
}

fn parse_src_file(
    src_file: &Path,
    src_file_name: &'static str,
    src_file_file_contents: &'static str,
    tokens: Vec<Spanned<Token>>,
) -> SourceFile {
    if !DUCK_STD_PATH.exists() {
        println!(
            "{}{}{} Standard library not found",
            Tag::Dargo,
            Tag::Err,
            Tag::Build,
        );
        std::process::exit(0);
    }

    let file_text = std::fs::read_to_string(DUCK_STD_PATH.to_path_buf())
        .unwrap()
        .leak();
    let lex = lex("std.duck", file_text);
    let mut std_src_file = source_file_parser(
        {
            let mut buf = DUCK_STD_PATH.to_path_buf();
            buf.pop();
            buf
        },
        make_input,
    )
    .parse(make_input(
        SS {
            start: 0,
            end: file_text.len(),
            context: Context {
                file_name: "std.duck",
                file_contents: file_text,
            },
        },
        lex.as_slice(),
    ))
    .unwrap()
    .flatten(&vec!["std".to_string()], false);

    for func in std_src_file.function_definitions.iter_mut() {
        if let Some(params) = &mut func.params {
            for (_, p) in params {
                typename_reset_global(&mut p.0);
            }
        }

        if let Some(ret) = &mut func.return_type {
            typename_reset_global(&mut ret.0);
        }

        typename_reset_global_value_expr(&mut func.value_expr.0);
    }

    fn typename_reset_global(t: &mut TypeExpr) {
        match t {
            TypeExpr::TypeName(global, _, type_params) => {
                type_params
                    .iter_mut()
                    .for_each(|(t, _)| typename_reset_global(t));
                *global = false;
            }
            TypeExpr::Array(t) => typename_reset_global(&mut t.0),
            TypeExpr::Duck(Duck { fields }) => {
                for field in fields {
                    typename_reset_global(&mut field.type_expr.0);
                }
            }
            TypeExpr::Tuple(fields) => {
                for field in fields {
                    typename_reset_global(&mut field.0);
                }
            }
            TypeExpr::Fun(params, ret, _) => {
                for (_, p) in params {
                    typename_reset_global(&mut p.0);
                }

                if let Some(ret) = ret {
                    typename_reset_global(&mut ret.0);
                }
            }
            _ => {}
        }
    }

    fn typename_reset_global_value_expr(type_expr: &mut ValueExpr) {
        match type_expr {
            ValueExpr::Defer(d) => typename_reset_global_value_expr(&mut d.0),
            ValueExpr::As(v, t) => {
                typename_reset_global(&mut t.0);
                typename_reset_global_value_expr(&mut v.0);
            }
            ValueExpr::Deref(v) | ValueExpr::Ref(v) | ValueExpr::RefMut(v) => {
                typename_reset_global_value_expr(&mut v.0)
            }
            ValueExpr::For {
                ident: _,
                target,
                block,
            } => {
                typename_reset_global_value_expr(&mut target.0);
                typename_reset_global_value_expr(&mut block.0);
            }
            ValueExpr::HtmlString(contents) => {
                for c in contents {
                    if let ValHtmlStringContents::Expr(e) = c {
                        typename_reset_global_value_expr(&mut e.0);
                    }
                }
            }
            ValueExpr::Match {
                value_expr,
                arms,
                else_arm,
                span: _,
            } => {
                typename_reset_global_value_expr(&mut value_expr.0);
                for arm in arms {
                    typename_reset_global_value_expr(&mut arm.value_expr.0);
                    typename_reset_global(&mut arm.type_case.0);
                }
                if let Some(else_arm) = else_arm {
                    typename_reset_global_value_expr(&mut else_arm.value_expr.0);
                    typename_reset_global(&mut else_arm.type_case.0);
                }
            }
            ValueExpr::Block(exprs) => {
                for expr in exprs {
                    typename_reset_global_value_expr(&mut expr.0);
                }
            }
            ValueExpr::Add(l, r)
            | ValueExpr::Mul(l, r)
            | ValueExpr::Sub(l, r)
            | ValueExpr::Div(l, r)
            | ValueExpr::Mod(l, r)
            | ValueExpr::Equals(l, r)
            | ValueExpr::NotEquals(l, r)
            | ValueExpr::LessThan(l, r)
            | ValueExpr::LessThanOrEquals(l, r)
            | ValueExpr::GreaterThan(l, r)
            | ValueExpr::GreaterThanOrEquals(l, r)
            | ValueExpr::And(l, r)
            | ValueExpr::Or(l, r) => {
                typename_reset_global_value_expr(&mut l.0);
                typename_reset_global_value_expr(&mut r.0);
            }
            ValueExpr::Lambda(l) => {
                let LambdaFunctionExpr {
                    is_mut: _,
                    params,
                    return_type,
                    value_expr,
                } = &mut **l;
                for (_, p) in params {
                    if let Some(p) = p.as_mut() {
                        typename_reset_global(&mut p.0);
                    }
                }

                if let Some(return_type) = return_type {
                    typename_reset_global(&mut return_type.0);
                }

                typename_reset_global_value_expr(&mut value_expr.0);
            }
            ValueExpr::ArrayAccess(target, idx) => {
                typename_reset_global_value_expr(&mut target.0);
                typename_reset_global_value_expr(&mut idx.0);
            }
            ValueExpr::FunctionCall {
                target,
                params,
                type_params: _,
                ..
            } => {
                // todo: type_params
                for p in params {
                    typename_reset_global_value_expr(&mut p.0);
                }
                typename_reset_global_value_expr(&mut target.0);
            }
            ValueExpr::FieldAccess { target_obj, .. } => {
                typename_reset_global_value_expr(&mut target_obj.0);
            }
            ValueExpr::ExtensionAccess { target_obj, .. } => {
                typename_reset_global_value_expr(&mut target_obj.0);
            }
            ValueExpr::Array(exprs) => {
                for expr in exprs {
                    typename_reset_global_value_expr(&mut expr.0);
                }
            }
            ValueExpr::BoolNegate(expr) | ValueExpr::Return(Some(expr)) => {
                typename_reset_global_value_expr(&mut expr.0);
            }
            ValueExpr::FormattedString(content) => {
                for c in content {
                    if let ValFmtStringContents::Expr(e) = c {
                        typename_reset_global_value_expr(&mut e.0);
                    }
                }
            }
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                typename_reset_global_value_expr(&mut condition.0);
                typename_reset_global_value_expr(&mut then.0);
                if let Some(r#else) = r#else {
                    typename_reset_global_value_expr(&mut r#else.0);
                }
            }
            ValueExpr::While { condition, body } => {
                typename_reset_global_value_expr(&mut condition.0);
                typename_reset_global_value_expr(&mut body.0);
            }
            ValueExpr::VarDecl(b) => {
                let Declaration {
                    name: _,
                    type_expr,
                    initializer,
                    is_const: _,
                } = &mut b.0;

                if let Some(type_expr) = type_expr.as_mut() {
                    typename_reset_global(&mut type_expr.0);
                }

                typename_reset_global_value_expr(&mut initializer.0);
            }
            ValueExpr::VarAssign(b) => {
                let Assignment { target, value_expr } = &mut b.0;
                typename_reset_global_value_expr(&mut target.0);
                typename_reset_global_value_expr(&mut value_expr.0);
            }
            ValueExpr::Tuple(fields) => {
                for field in fields {
                    typename_reset_global_value_expr(&mut field.0);
                }
            }
            ValueExpr::Duck(fields) => {
                for field in fields {
                    typename_reset_global_value_expr(&mut field.1.0);
                }
            }
            ValueExpr::Struct { fields, .. } => {
                for field in fields {
                    typename_reset_global_value_expr(&mut field.1.0);
                }
            }
            ValueExpr::Break
            | ValueExpr::Char(..)
            | ValueExpr::Continue
            | ValueExpr::Float(..)
            | ValueExpr::String(..)
            | ValueExpr::Int(..)
            | ValueExpr::Bool(..)
            | ValueExpr::Variable(..)
            | ValueExpr::RawVariable(..)
            | ValueExpr::Tag(..)
            | ValueExpr::Return(..)
            | ValueExpr::InlineGo(..) => {}
        }
    }

    let (src_file, parse_errors) = source_file_parser(
        {
            let mut src_file_clone = src_file.to_path_buf();
            src_file_clone.pop();
            src_file_clone
        },
        make_input,
    )
    .parse(make_input(
        SS {
            start: 0,
            end: src_file_file_contents.len(),
            context: Context {
                file_name: src_file_name,
                file_contents: src_file_file_contents,
            },
        },
        &tokens,
    ))
    .into_output_errors();

    parse_errors.into_iter().for_each(|e| {
        parse_failure(src_file_name, &e, src_file_file_contents);
    });

    // TODO: do this for all dependencies
    let mut result = src_file.unwrap().flatten(&vec![], true);
    if !false {
        for s in &std_src_file.function_definitions {
            result.function_definitions.push(s.clone());
        }
        for s in &std_src_file.type_definitions {
            result.type_definitions.push(s.clone());
        }
        for s in &std_src_file.struct_definitions {
            result.struct_definitions.push(s.clone());
        }
        for s in &std_src_file.use_statements {
            if let UseStatement::Go(..) = s {
                result.push_use(s);
            }
        }
        for s in &std_src_file.tsx_components {
            result.tsx_components.push(s.clone());
        }
        for s in &std_src_file.duckx_components {
            result.duckx_components.push(s.clone());
        }

        for test_case in &std_src_file.test_cases {
            result.test_cases.push(test_case.clone());
        }
        for extension_def in std_src_file.extensions_defs {
            result.extensions_defs.push(extension_def.clone());
        }
    }

    result
}

fn typecheck<'a>(src_file_ast: &mut SourceFile, tailwind_tx: &'a Sender<String>) -> TypeEnv<'a> {
    let mut type_env = TypeEnv {
        tailwind_sender: Some(tailwind_tx),
        ..TypeEnv::default()
    };
    type_resolve::typeresolve_source_file(src_file_ast, &mut type_env);

    type_env
}

fn write_in_duck_dotdir(file_name: &str, content: &str) -> PathBuf {
    let target_file = {
        let mut target_file_path = DARGO_DOT_DIR.clone();
        target_file_path.push(file_name);
        target_file_path
    };

    let mut file = File::create(target_file.clone()).expect("couldn't create file in duck dot dir"); // TODO error handling
    file.write_all(content.as_bytes())
        .expect("couldn't write file in duck dot dir"); // TODO error handling

    target_file
}

fn duck_with_message(msg: &str) {
    println!(
        "{}\n{}{}{}  {msg}\n{}",
        " _,".bright_yellow().bold(),
        "(".bright_yellow().bold(),
        "~".blue().bold(),
        "<".yellow().bold(),
        "<_)".bright_yellow().bold(),
    );
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli_result = dargo::cli::run_cli();
    if let Err(err) = cli_result {
        duck_with_message("Ooops... something went wrong!!");
        println!("{}", err.0);
        process::exit(1);
    }

    Ok(())
}
