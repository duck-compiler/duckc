use colored::Colorize;
use lazy_static::lazy_static;
use std::{
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
};

use crate::{
    bundle,
    cli::go_cli::{self, GoCliErrKind},
    dargo::cli::CompileArgs,
    tags::Tag,
    write_in_duck_dotdir, DARGO_DOT_DIR,
};

#[derive(Debug)]
pub enum CompileErrKind {
    CorruptedFileName,
    TargetPathIsDirectory,
    FileNotFound,
    CannotReadFile,
    GoCli(GoCliErrKind),
}

lazy_static! {
    static ref COMPILE_TAG: String = " compile ".on_bright_black().bright_white().to_string();
}

pub struct CompileOutput {
    pub binary_path: PathBuf,
}

pub fn compile(compile_args: CompileArgs) -> Result<CompileOutput, (String, CompileErrKind)> {
    let src_file: PathBuf = compile_args.file;
    let binary_output_name: Option<String> = compile_args.output_name;

    if src_file.is_dir() {
        let message = format!(
            "{}{} the path you provided is a directory. You need to provide a .duck file",
            *COMPILE_TAG,
            Tag::Err,
        );

        return Err((message, CompileErrKind::TargetPathIsDirectory));
    }

    if src_file
        .extension()
        .ok_or_else(|| {
            format!(
                "{}{} couldn't extract file extension from provided source file",
                *COMPILE_TAG,
                Tag::Err,
            )
        })
        .unwrap()
        != "duck"
    {
        let message = format!(
            "{}{} the path you provided is not a valid duck source file. You need to provide a .duck file",
            *COMPILE_TAG,
            Tag::Err,
        );
        return Err((message, CompileErrKind::TargetPathIsDirectory));
    }

    let src_file_name: &'static str = src_file
        .file_name()
        .ok_or_else(|| {
            (
                format!(
                    "{}{} couldn't get the filename from given ",
                    *COMPILE_TAG,
                    Tag::Err
                ),
                CompileErrKind::CorruptedFileName,
            )
        })?
        .to_str()
        .ok_or_else(|| {
            (
                format!(
                    "{}{} the filename is an invalid utf-8 string",
                    *COMPILE_TAG,
                    Tag::Err
                ),
                CompileErrKind::CorruptedFileName,
            )
        })?
        .to_string()
        .leak();

    let src_file_file_contents: &'static str = fs::read_to_string(&src_file)
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't read file '{}'. msg='{}'",
                    *COMPILE_TAG,
                    Tag::Err,
                    src_file.to_string_lossy().bright_blue(),
                    err.to_string().bright_red()
                ),
                CompileErrKind::CannotReadFile,
            )
        })?
        .to_string()
        .leak();

    compile_new_pipeline(
        src_file_file_contents,
        src_file_name,
        binary_output_name,
        compile_args.optimize_go,
    )
}

fn compile_new_pipeline(
    src: &'static str,
    src_file_name: &'static str,
    binary_output_name: Option<String>,
    optimize_go: bool,
) -> Result<CompileOutput, (String, CompileErrKind)> {
    use crate::dargo::module_loader::ModuleStore;
    use crate::emit2::{lower, lower_js, render, render_js};
    use crate::parser2::parser::{parse, Item, SourceFile, UseDecl};
    use crate::parser2::tokenizer::tokenize_no_comments;
    use crate::semantics2::mono::monomorphize;
    use crate::semantics2::resolver::resolve_with_modules;
    use crate::semantics2::type_infer::infer;

    use crate::parser2::errors::render_errors;

    // file_id=1 for user source; std module files get unique ids >= 2
    let (tokens, lex_errors) = tokenize_no_comments(src, 1);
    if !lex_errors.is_empty() {
        let msg = format!(
            "{}{} Lex errors:\n{}",
            *COMPILE_TAG,
            Tag::Err,
            render_errors(
                src,
                src_file_name,
                None,
                lex_errors.iter().map(|e| ("lex", e.msg.as_str(), e.span)),
                true
            ),
        );
        return Err((msg, CompileErrKind::CannotReadFile));
    }

    let (user_ast, parse_errors) = parse(tokens, 1);
    if !parse_errors.is_empty() {
        let msg = format!(
            "{}{} Parse errors:\n{}",
            *COMPILE_TAG,
            Tag::Err,
            render_errors(
                src,
                src_file_name,
                None,
                parse_errors
                    .iter()
                    .map(|e| ("parse", e.msg.as_str(), e.span)),
                true
            ),
        );
        return Err((msg, CompileErrKind::CannotReadFile));
    }

    // Load Duck modules referenced by `use std::*` imports in the user source.
    let std_root = crate::DUCK_STD_PATH
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_default();
    let mut module_store = ModuleStore::new(std_root);
    // non-fatal: module load errors are surfaced as resolve errors later
    let _load_errors = module_store.load_needed(&user_ast.items);

    // module items first so UseDecl::Duck bindings can resolve against them
    let module_item_names = module_store.all_mangled_names();
    let merged_ast = {
        let mut items = Vec::new();
        // inject go imports and items from every loaded module
        for loaded in module_store.modules.values() {
            for imp in &loaded.go_imports {
                use crate::parser2::parser::{Span, WithSpan};
                let ws = WithSpan::new(imp.clone(), Span::dummy());
                items.push(Item::Use(UseDecl::Go(vec![ws], Span::dummy())));
            }
            items.extend(loaded.items.clone());
        }
        items.extend(user_ast.items);
        SourceFile { items }
    };

    let resolve_out = resolve_with_modules(merged_ast, &module_item_names, &module_store);
    if !resolve_out.errors.is_empty() {
        let msg = format!(
            "{}{} Resolve errors:\n{}",
            *COMPILE_TAG,
            Tag::Err,
            render_errors(
                src,
                src_file_name,
                Some(&module_store.sources),
                resolve_out
                    .errors
                    .iter()
                    .map(|e| ("resolve", e.msg.as_str(), e.span)),
                true
            ),
        );
        return Err((msg, CompileErrKind::CannotReadFile));
    }

    // Load TS type info for type inference.
    {
        let node_modules_candidates = [std::env::current_dir()
            .unwrap_or_default()
            .join("node_modules")];
        for item in &resolve_out.source_file.items {
            if let Item::Use(UseDecl::Ts(pkg_name, _)) = item {
                for nm in &node_modules_candidates {
                    if crate::ts_interop::scan_package(pkg_name, nm) {
                        break;
                    }
                }
            }
        }
    }

    let infer_out = monomorphize(infer(resolve_out));

    if !infer_out.errors.is_empty() {
        let errors_text = render_errors(
            src,
            src_file_name,
            Some(&module_store.sources),
            infer_out
                .errors
                .iter()
                .map(|e| ("type", e.msg.as_str(), e.span)),
            true,
        );
        let msg = format!("{}{} Type errors:\n{}", *COMPILE_TAG, Tag::Err, errors_text);
        return Err((msg, CompileErrKind::CannotReadFile));
    }

    let go_file = lower(infer_out.clone(), "main");
    let go_src =
        crate::go_fixup::remove_unused_imports::cleanup_go_source(&render(&go_file), false);

    // Compile JS output and bundle it.
    let js_file = lower_js(infer_out);
    let client_js = render_js(&js_file);

    if !js_file.client_fn_names.is_empty() || !js_file.ts_packages.is_empty() {
        let bundled = bundle::bundle(bundle::BundleInput {
            ts_packages: js_file.ts_packages.clone(),
            client_js,
            client_fn_names: js_file.client_fn_names.clone(),
            node_modules: None,
        });

        let bundle_path = {
            let mut p = DARGO_DOT_DIR.clone();
            p.push("client.js");
            p
        };
        fs::write(&bundle_path, &bundled).map_err(|e| {
            (
                format!("{}{} couldn't write client.js: {e}", *COMPILE_TAG, Tag::Err),
                CompileErrKind::CannotReadFile,
            )
        })?;
    }

    let go_file_name = format!("{src_file_name}.gen.go");
    let go_output_file = write_in_duck_dotdir(&go_file_name, &go_src);

    if optimize_go {
        let _ = go_cli::format(go_output_file.as_path());
    }

    let external_go_imports: Vec<String> = {
        go_file
            .imports
            .iter()
            .filter(|p| p.split('/').next().map_or(false, |seg| seg.contains('.')))
            .cloned()
            .collect()
    };

    let executable_path = go_cli::build(
        &DARGO_DOT_DIR,
        binary_output_name
            .map(OsString::from)
            .unwrap_or(OsString::from("duck_out"))
            .as_os_str(),
        Path::new(&go_file_name),
        &external_go_imports,
    )
    .map_err(|err| {
        (
            format!("{}{}", *COMPILE_TAG, err.0),
            CompileErrKind::GoCli(err.1),
        )
    })?;

    println!(
        "{}{}{} Successfully compiled binary",
        Tag::Dargo,
        *COMPILE_TAG,
        Tag::Check,
    );

    Ok(CompileOutput {
        binary_path: executable_path,
    })
}
