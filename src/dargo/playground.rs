use std::collections::HashMap;
use std::env;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::process::Command;

use crate::emit2::go_ir::GoFile;
use crate::emit2::{lower, lower_js, render, render_js};
use crate::parser2::errors::render_errors;
use crate::parser2::parser::{
    DefId, DefKind, SymbolTable, TypeDescription, TypeExpr, Typed, WithSpan, parse,
};
use crate::parser2::tokenizer::{Token, tokenize_no_comments};
use crate::semantics2::mono::monomorphize;
use crate::semantics2::resolver::{ResolveOutput, resolve};
use crate::semantics2::type_infer::{InferOutput, infer};

static HTML: &str = include_str!("../../playground/index.html");

/// Stable directory for playground-installed packages: `~/.duck/playground/`
fn playground_pkg_dir() -> PathBuf {
    env::home_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".duck")
        .join("playground")
}

pub fn serve() {
    let addr = "127.0.0.1:7878";
    let listener = TcpListener::bind(addr).expect("could not bind 127.0.0.1:7878");
    println!("playground → http://{addr}");
    for stream in listener.incoming().flatten() {
        handle(stream);
    }
}

fn handle(stream: TcpStream) {
    let mut writer = match stream.try_clone() {
        Ok(s) => s,
        Err(_) => return,
    };
    let mut reader = BufReader::new(stream);

    let mut request_line = String::new();
    if reader.read_line(&mut request_line).is_err() {
        return;
    }

    let mut content_length: usize = 0;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).is_err() {
            break;
        }
        if line.trim().is_empty() {
            break;
        }
        if line.to_ascii_lowercase().starts_with("content-length:") {
            content_length = line[15..].trim().parse().unwrap_or(0);
        }
    }

    let body = if content_length > 0 {
        let mut buf = vec![0u8; content_length];
        reader.read_exact(&mut buf).ok();
        String::from_utf8_lossy(&buf).into_owned()
    } else {
        String::new()
    };

    let parts: Vec<&str> = request_line.splitn(3, ' ').collect();
    let path = parts.get(1).copied().unwrap_or("/");
    let is_post = parts.first().copied() == Some("POST");

    let (content_type, payload) = match (is_post, path) {
        (false, _) => ("text/html; charset=utf-8", HTML.to_owned()),
        (true, "/run") => ("application/json", run_stages(&body)),
        (true, "/exec") => ("application/json", run_and_exec(&body)),
        (true, "/install") => ("application/json", install_packages(&body)),
        _ => ("text/plain", "not found".to_owned()),
    };

    let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\n\r\n{}",
        payload.len(),
        payload,
    );
    writer.write_all(response.as_bytes()).ok();
}

fn pipeline(src: &str) -> (serde_json::Value, String) {
    let t0 = std::time::Instant::now();
    let (tokens, lex_errors) = tokenize_no_comments(src, 0);
    let t_lex = t0.elapsed().as_micros();
    let token_count = tokens.len();
    let tokens_text = fmt_tokens(&tokens);
    let token_errors_text = render_errors(
        src,
        "<source>",
        None,
        lex_errors.iter().map(|e| ("lex", e.msg.as_str(), e.span)),
        false,
    );

    let t1 = std::time::Instant::now();
    let (ast, parse_errors) = parse(tokens, 0);
    let t_parse = t1.elapsed().as_micros();
    let ast_text = strip_noise(&format!("{ast:#?}"));
    let parse_errors_text = render_errors(
        src,
        "<source>",
        None,
        parse_errors
            .iter()
            .map(|e| ("parse", e.msg.as_str(), e.span)),
        false,
    );

    let t2 = std::time::Instant::now();
    let resolve_out = resolve(ast);
    let t_resolve = t2.elapsed().as_micros();
    let resolved_text = strip_noise(&format!("{:#?}", resolve_out.source_file));
    let resolve_errors_text = render_errors(
        src,
        "<source>",
        None,
        resolve_out
            .errors
            .iter()
            .map(|e| ("resolve", e.msg.as_str(), e.span)),
        false,
    );
    let symbol_table_text = fmt_symbol_table(&resolve_out);
    let scopes_text = fmt_scopes(&resolve_out);

    // Pre-scan external packages so type inference can check their method/field types.
    {
        use crate::parser2::parser::{Item, UseDecl};
        let mut external_go: Vec<String> = Vec::new();
        let mut external_ts: Vec<String> = Vec::new();
        for item in &resolve_out.source_file.items {
            match item {
                Item::Use(UseDecl::Go(path, _)) => {
                    let import_path = path[0].value.clone();
                    let first_seg = import_path.split('/').next().unwrap_or("");
                    if first_seg.contains('.') {
                        external_go.push(import_path);
                    }
                }
                Item::Use(UseDecl::Ts(pkg_name, _)) => {
                    external_ts.push(pkg_name.clone());
                }
                _ => {}
            }
        }
        if !external_go.is_empty() {
            let tmp = std::env::temp_dir().join("duck_playground");
            std::fs::create_dir_all(&tmp).ok();
            crate::go_interop::prepare_external_packages(&external_go, &tmp);
        }
        if !external_ts.is_empty() {
            // Try to find node_modules in a few standard locations.
            let node_modules_candidates = [
                std::env::current_dir()
                    .unwrap_or_default()
                    .join("node_modules"),
                playground_pkg_dir().join("node_modules"),
                std::env::temp_dir().join("duck_playground/node_modules"),
            ];
            for pkg in &external_ts {
                for nm in &node_modules_candidates {
                    if crate::ts_interop::scan_package(pkg, nm) {
                        break;
                    }
                }
            }
        }
    }

    let t3 = std::time::Instant::now();
    let infer_out = infer(resolve_out);
    let t_infer = t3.elapsed().as_micros();
    let typed_text = strip_noise(&format!("{:#?}", infer_out.source_file));
    let infer_errors_text = render_errors(
        src,
        "<source>",
        None,
        infer_out
            .errors
            .iter()
            .map(|e| ("type", e.msg.as_str(), e.span)),
        false,
    );
    let typed_symbols_text = fmt_typed_symbols(&infer_out);

    let t4 = std::time::Instant::now();
    let infer_out = monomorphize(infer_out);
    let t_mono = t4.elapsed().as_micros();

    let t5 = std::time::Instant::now();
    let js_file = lower_js(infer_out.clone());
    let js_src = {
        let rendered = render_js(&js_file);
        if rendered.trim().is_empty() {
            "(no client code - use `client fn` to mark functions for the browser)".to_string()
        } else {
            rendered
        }
    };
    let go_file = lower(infer_out, "main");
    let go_ir_text = fmt_go_ir(&go_file);
    let go_src = render(&go_file);
    let t_lower = t5.elapsed().as_micros();

    let data = serde_json::json!({
        "tokens":          tokens_text,
        "token_count":     format!("{token_count} tokens"),
        "token_errors":    token_errors_text,
        "ast":             ast_text,
        "parse_errors":    parse_errors_text,
        "resolved":        resolved_text,
        "resolve_errors":  resolve_errors_text,
        "symbol_table":    symbol_table_text,
        "scopes":          scopes_text,
        "typed":           typed_text,
        "infer_errors":    infer_errors_text,
        "typed_symbols":   typed_symbols_text,
        "go_ir":           go_ir_text,
        "go_src":          go_src,
        "js_src":          js_src,
        "t_lex":           t_lex,
        "t_parse":         t_parse,
        "t_resolve":       t_resolve,
        "t_infer":         t_infer,
        "t_mono":          t_mono,
        "t_lower":         t_lower,
    });

    (data, go_src)
}

fn run_stages(src: &str) -> String {
    pipeline(src).0.to_string()
}

fn run_and_exec(src: &str) -> String {
    let (mut data, go_src) = pipeline(src);
    let (stdout, stderr) = execute_go(&go_src);
    data["exec_stdout"] = serde_json::json!(stdout);
    data["exec_stderr"] = serde_json::json!(stderr);
    data.to_string()
}

fn install_packages(body: &str) -> String {
    // Body is a space/newline separated list of package names.
    let packages: Vec<&str> = body.split_whitespace().filter(|s| !s.is_empty()).collect();
    if packages.is_empty() {
        return serde_json::json!({ "ok": false, "error": "no package names provided" })
            .to_string();
    }

    let dir = playground_pkg_dir();
    if let Err(e) = std::fs::create_dir_all(&dir) {
        return serde_json::json!({ "ok": false, "error": format!("could not create playground dir: {e}") }).to_string();
    }

    // Ensure a minimal package.json exists so bun has a project root to work with.
    let pkg_json = dir.join("package.json");
    if !pkg_json.exists() {
        std::fs::write(
            &pkg_json,
            "{\"name\":\"duck-playground\",\"private\":true}\n",
        )
        .ok();
    }

    let mut args = vec!["add"];
    args.extend(packages.iter().copied());

    match Command::new("bun").args(&args).current_dir(&dir).output() {
        Ok(out) => {
            let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
            let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
            let combined = format!("{stdout}{stderr}");
            if out.status.success() {
                serde_json::json!({ "ok": true, "output": combined }).to_string()
            } else {
                serde_json::json!({ "ok": false, "error": combined }).to_string()
            }
        }
        Err(e) => serde_json::json!({ "ok": false, "error": format!("could not run bun: {e}") })
            .to_string(),
    }
}

fn external_imports_in_go_src(go_src: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut in_block = false;
    for line in go_src.lines() {
        let t = line.trim();
        if t.starts_with("import (") || t == "import(" {
            in_block = true;
            continue;
        }
        if in_block && t == ")" {
            in_block = false;
            continue;
        }
        let candidate = if in_block {
            Some(t)
        } else if t.starts_with("import \"") {
            Some(&t["import ".len()..])
        } else {
            None
        };
        if let Some(s) = candidate {
            if let Some(inner) = s.strip_prefix('"').and_then(|s| s.split('"').next()) {
                let first = inner.split('/').next().unwrap_or("");
                if first.contains('.') {
                    result.push(inner.to_string());
                }
            }
        }
    }
    result
}

fn execute_go(go_src: &str) -> (String, String) {
    let tmp = std::env::temp_dir().join("duck_playground");
    std::fs::create_dir_all(&tmp).ok();
    std::fs::write(tmp.join("main.go"), go_src).ok();
    std::fs::write(tmp.join("go.mod"), "module playground\n\ngo 1.21\n").ok();

    let go_bin = env::home_dir()
        .map(|h| h.join(".duck").join("go-compiler").join("bin").join("go"))
        .filter(|p| p.exists())
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|| "go".to_string());

    for pkg in external_imports_in_go_src(go_src) {
        Command::new(&go_bin)
            .args(["get", &pkg])
            .current_dir(&tmp)
            .output()
            .ok();
    }

    match Command::new(&go_bin)
        .arg("run")
        .arg("main.go")
        .current_dir(&tmp)
        .output()
    {
        Ok(o) => (
            String::from_utf8_lossy(&o.stdout).into_owned(),
            String::from_utf8_lossy(&o.stderr).into_owned(),
        ),
        Err(e) => (String::new(), format!("could not run go: {e}")),
    }
}

fn fmt_tokens(tokens: &[WithSpan<Token>]) -> String {
    tokens
        .iter()
        .map(|t| format!("{:>5}:{:<5}  {}", t.span.start, t.span.end, t.value))
        .collect::<Vec<_>>()
        .join("\n")
}

fn fmt_symbol_table(out: &ResolveOutput) -> String {
    let mut rows: Vec<String> = vec![
        format!(
            "{:<4}  {:<20}  {:<22}  {:<5}  {}",
            "ID", "NAME", "KIND", "DEPTH", "SPAN"
        ),
        format!("{}", "-".repeat(70)),
    ];
    for (id, def) in out.symbols.iter() {
        let kind = fmt_kind(&def.kind);
        let span = if def.span.is_dummy() {
            "-".into()
        } else {
            format!("{}..{}", def.span.start, def.span.end)
        };
        let depth = if matches!(def.kind, DefKind::Poison) {
            "-".into()
        } else {
            def.scope_depth.to_string()
        };
        rows.push(format!(
            "{:<4}  {:<20}  {:<22}  {:<5}  {}",
            id.0, def.name, kind, depth, span
        ));
    }
    rows.join("\n")
}

fn fmt_scopes(out: &ResolveOutput) -> String {
    let mut by_depth: std::collections::BTreeMap<u32, Vec<(DefId, &str, &DefKind)>> =
        std::collections::BTreeMap::new();

    for (id, def) in out.symbols.iter() {
        if matches!(def.kind, DefKind::Poison) {
            continue;
        }
        by_depth
            .entry(def.scope_depth)
            .or_default()
            .push((id, &def.name, &def.kind));
    }

    let global_reverse: HashMap<DefId, &str> = out
        .global_scope
        .iter()
        .map(|(name, &id)| (id, name.as_str()))
        .collect();

    let mut lines: Vec<String> = Vec::new();

    for (depth, mut entries) in by_depth {
        let label = match depth {
            0 => "global".into(),
            1 => "function / generics".into(),
            d => format!("block depth {d}"),
        };
        lines.push(format!("depth {depth}  [{label}]"));
        lines.push(format!("{}", "-".repeat(50)));

        entries.sort_by_key(|(id, _, _)| id.0);
        for (id, name, kind) in entries {
            let alias = global_reverse.get(&id).copied();
            let in_global = if alias.is_some() { "  ← global" } else { "" };
            lines.push(format!(
                "  #{:<4}  {:<18}  {}{}",
                id.0,
                name,
                fmt_kind(kind),
                in_global
            ));
        }
        lines.push(String::new());
    }

    if lines.is_empty() {
        "(empty)".into()
    } else {
        lines.join("\n")
    }
}

fn fmt_kind(kind: &DefKind) -> String {
    match kind {
        DefKind::Local { is_mut } => format!("Local(mut={})", if *is_mut { "T" } else { "F" }),
        DefKind::Param { is_mut } => format!("Param(mut={})", if *is_mut { "T" } else { "F" }),
        DefKind::Function {
            is_static,
            is_client,
        } => {
            format!(
                "Function(static={},client={})",
                if *is_static { "T" } else { "F" },
                if *is_client { "T" } else { "F" }
            )
        }
        DefKind::Struct => "Struct".into(),
        DefKind::TypeAlias => "TypeAlias".into(),
        DefKind::Const => "Const".into(),
        DefKind::GlobalVar => "GlobalVar".into(),
        DefKind::GenericParam => "GenericParam".into(),
        DefKind::GoPackage { import_path } => format!("GoPackage({import_path})"),
        DefKind::Module { members } => format!("Module({} members)", members.len()),
        DefKind::Poison => "Poison".into(),
    }
}

fn fmt_go_ir(file: &GoFile) -> String {
    use crate::emit2::go_ir::{
        GoBinOp, GoDecl, GoExpr, GoField, GoParam, GoStmt, GoType, GoUnaryOp,
    };

    fn type_str(ty: &GoType) -> String {
        match ty {
            GoType::Int64 => "int64".into(),
            GoType::Uint64 => "uint64".into(),
            GoType::Float64 => "float64".into(),
            GoType::Bool => "bool".into(),
            GoType::String => "string".into(),
            GoType::Rune => "rune".into(),
            GoType::Byte => "byte".into(),
            GoType::Slice(e) => format!("[]{}", type_str(e)),
            GoType::Ptr(e) => format!("*{}", type_str(e)),
            GoType::Named(n) => n.clone(),
            GoType::Any => "any".into(),
            GoType::DuckInterface(cs) => {
                let inner = cs
                    .iter()
                    .map(|(name, ty)| format!("{name}[{}]", type_str(ty)))
                    .collect::<Vec<_>>()
                    .join("; ");
                format!("interface{{ {inner} }}")
            }
            GoType::Func { params, ret } => {
                let ps = params.iter().map(type_str).collect::<Vec<_>>().join(", ");
                match ret {
                    Some(r) => format!("func({ps}) {}", type_str(r)),
                    None => format!("func({ps})"),
                }
            }
        }
    }

    fn param_str(p: &GoParam) -> String {
        format!("{} {}", p.name, type_str(&p.ty))
    }

    fn bin_op(op: &GoBinOp) -> &'static str {
        match op {
            GoBinOp::Add => "+",
            GoBinOp::Sub => "-",
            GoBinOp::Mul => "*",
            GoBinOp::Div => "/",
            GoBinOp::Mod => "%",
            GoBinOp::BitAnd => "&",
            GoBinOp::BitOr => "|",
            GoBinOp::BitXor => "^",
            GoBinOp::Shl => "<<",
            GoBinOp::Shr => ">>",
            GoBinOp::Eq => "==",
            GoBinOp::Neq => "!=",
            GoBinOp::Lt => "<",
            GoBinOp::Lte => "<=",
            GoBinOp::Gt => ">",
            GoBinOp::Gte => ">=",
            GoBinOp::And => "&&",
            GoBinOp::Or => "||",
        }
    }

    fn expr_str(e: &GoExpr) -> String {
        match e {
            GoExpr::Ident(s) => s.clone(),
            GoExpr::Int(v) => v.to_string(),
            GoExpr::UInt(v) => format!("{v}u"),
            GoExpr::Float(v) => format!("{v}"),
            GoExpr::Bool(v) => v.to_string(),
            GoExpr::Char(c) => format!("'{c}'"),
            GoExpr::Str(s) => format!("{s:?}"),
            GoExpr::Nil => "nil".into(),
            GoExpr::Raw(s) => format!("raw({s})"),
            GoExpr::BinOp { op, lhs, rhs } => {
                format!("({} {} {})", expr_str(lhs), bin_op(op), expr_str(rhs))
            }
            GoExpr::UnaryOp { op, operand } => {
                let sym = match op {
                    GoUnaryOp::Neg => "-",
                    GoUnaryOp::Not => "!",
                    GoUnaryOp::BitNot => "^",
                };
                format!("{sym}{}", expr_str(operand))
            }
            GoExpr::Call { callee, args } => {
                let as_ = args.iter().map(expr_str).collect::<Vec<_>>().join(", ");
                format!("{}({})", expr_str(callee), as_)
            }
            GoExpr::Field { base, field } => format!("{}.{field}", expr_str(base)),
            GoExpr::Index { base, index } => format!("{}[{}]", expr_str(base), expr_str(index)),
            GoExpr::StructLit { ty, fields } => {
                let fs = fields
                    .iter()
                    .map(|(k, v)| format!("{k}: {}", expr_str(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{ty}{{{fs}}}")
            }
            GoExpr::SliceLit { ty, elems } => {
                let es = elems.iter().map(expr_str).collect::<Vec<_>>().join(", ");
                format!("[]{}{{{es}}}", type_str(ty))
            }
            GoExpr::Closure { params, ret, .. } => {
                let ps = params.iter().map(param_str).collect::<Vec<_>>().join(", ");
                let r = ret
                    .as_ref()
                    .map(|t| format!(" {}", type_str(t)))
                    .unwrap_or_default();
                format!("func({ps}){r}{{...}}")
            }
            GoExpr::Cast { ty, value } => format!("{}({})", type_str(ty), expr_str(value)),
            GoExpr::Ref(v) => format!("&{}", expr_str(v)),
            GoExpr::Deref(v) => format!("*{}", expr_str(v)),
            GoExpr::TypeAssert { value, ty } => {
                format!("{}.({ty})", expr_str(value), ty = type_str(ty))
            }
        }
    }

    fn push_stmts(out: &mut String, stmts: &[GoStmt], depth: usize) {
        let pad = "  ".repeat(depth);
        for stmt in stmts {
            match stmt {
                GoStmt::Declare { name, ty } => {
                    out.push_str(&format!("{pad}Declare  {name} {}\n", type_str(ty)))
                }
                GoStmt::DeclareAssign { name, value } => out.push_str(&format!(
                    "{pad}DeclareAssign  {name} := {}\n",
                    expr_str(value)
                )),
                GoStmt::MultiDeclareAssign { names, value } => out.push_str(&format!(
                    "{pad}MultiDeclareAssign  {} := {}\n",
                    names.join(", "),
                    expr_str(value)
                )),
                GoStmt::Assign { target, value } => out.push_str(&format!(
                    "{pad}Assign  {} = {}\n",
                    expr_str(target),
                    expr_str(value)
                )),
                GoStmt::Return(None) => out.push_str(&format!("{pad}Return\n")),
                GoStmt::Return(Some(e)) => out.push_str(&format!("{pad}Return  {}\n", expr_str(e))),
                GoStmt::Break => out.push_str(&format!("{pad}Break\n")),
                GoStmt::Continue => out.push_str(&format!("{pad}Continue\n")),
                GoStmt::Defer(e) => out.push_str(&format!("{pad}Defer  {}\n", expr_str(e))),
                GoStmt::Expr(e) => out.push_str(&format!("{pad}Expr  {}\n", expr_str(e))),
                GoStmt::If { cond, then, else_ } => {
                    out.push_str(&format!("{pad}If  {}\n", expr_str(cond)));
                    push_stmts(out, then, depth + 1);
                    if let Some(el) = else_ {
                        out.push_str(&format!("{pad}Else\n"));
                        push_stmts(out, el, depth + 1);
                    }
                }
                GoStmt::ForCond { cond, body } => {
                    out.push_str(&format!("{pad}For  {}\n", expr_str(cond)));
                    push_stmts(out, body, depth + 1);
                }
                GoStmt::ForRange { val, iter, body } => {
                    out.push_str(&format!(
                        "{pad}ForRange  {val} := range {}\n",
                        expr_str(iter)
                    ));
                    push_stmts(out, body, depth + 1);
                }
                GoStmt::Loop(body) => {
                    out.push_str(&format!("{pad}Loop\n"));
                    push_stmts(out, body, depth + 1);
                }
                GoStmt::Block(body) => {
                    out.push_str(&format!("{pad}Block\n"));
                    push_stmts(out, body, depth + 1);
                }
                GoStmt::TypeSwitch {
                    value,
                    arms,
                    default,
                } => {
                    out.push_str(&format!("{pad}TypeSwitch  {}\n", expr_str(value)));
                    for arm in arms {
                        out.push_str(&format!(
                            "{}  case {} {}:\n",
                            pad,
                            arm.binding,
                            type_str(&arm.ty)
                        ));
                        push_stmts(out, &arm.body, depth + 2);
                    }
                    if let Some(def) = default {
                        out.push_str(&format!("{pad}  default:\n"));
                        push_stmts(out, def, depth + 2);
                    }
                }
            }
        }
    }

    let mut out = String::new();
    out.push_str(&format!("package {}\n", file.package));
    if !file.imports.is_empty() {
        out.push_str(&format!("\nimports: {}\n", file.imports.join(", ")));
    }
    for decl in &file.decls {
        out.push('\n');
        match decl {
            GoDecl::Func {
                name,
                receiver,
                params,
                ret,
                body,
            } => {
                let recv = receiver
                    .as_ref()
                    .map(|r| format!("({}) ", param_str(r)))
                    .unwrap_or_default();
                let ps = params.iter().map(param_str).collect::<Vec<_>>().join(", ");
                let ret_s = ret
                    .as_ref()
                    .map(|r| format!(" -> {}", type_str(r)))
                    .unwrap_or_default();
                out.push_str(&format!("func {recv}{name}({ps}){ret_s}\n"));
                push_stmts(&mut out, body, 1);
            }
            GoDecl::Struct { name, fields } => {
                out.push_str(&format!("struct {name}\n"));
                for GoField { name: fname, ty } in fields {
                    out.push_str(&format!("  {fname}  {}\n", type_str(ty)));
                }
            }
            GoDecl::TypeAlias { name, ty } => {
                out.push_str(&format!("type {name} = {}\n", type_str(ty)));
            }
            GoDecl::Var { name, ty, value } => {
                out.push_str(&format!(
                    "var {name} {}  = {}\n",
                    type_str(ty),
                    expr_str(value)
                ));
            }
            GoDecl::Interface { name, methods } => {
                out.push_str(&format!("interface {name}[T any]\n"));
                for m in methods {
                    let ret = m
                        .ret
                        .as_ref()
                        .map(|r| format!(" {}", type_str(r)))
                        .unwrap_or_default();
                    out.push_str(&format!("  {}(){ret}\n", m.name));
                }
            }
            GoDecl::Raw(s) => {
                out.push_str(s);
                out.push('\n');
            }
        }
    }
    out
}

fn fmt_typed_symbols(out: &InferOutput) -> String {
    let mut rows: Vec<String> = vec![
        format!(
            "{:<4}  {:<20}  {:<22}  {:<5}  {:<35}  {}",
            "ID", "NAME", "KIND", "DEPTH", "TYPE", "SPAN"
        ),
        "-".repeat(100),
    ];
    for (id, def) in out.symbols.iter() {
        let kind = fmt_kind(&def.kind);
        let span = if def.span.is_dummy() {
            "-".into()
        } else {
            format!("{}..{}", def.span.start, def.span.end)
        };
        let depth = if matches!(def.kind, DefKind::Poison) {
            "-".into()
        } else {
            def.scope_depth.to_string()
        };
        let ty = def
            .ty
            .as_ref()
            .map(|te| fmt_type_expr(te, &out.symbols))
            .unwrap_or_else(|| "?".into());
        rows.push(format!(
            "{:<4}  {:<20}  {:<22}  {:<5}  {:<35}  {}",
            id.0, def.name, kind, depth, ty, span
        ));
    }
    rows.join("\n")
}

fn fmt_type_expr(te: &TypeExpr<Typed>, symbols: &SymbolTable) -> String {
    fmt_type_desc(&te.desc, symbols)
}

fn fmt_type_desc(desc: &TypeDescription<Typed>, symbols: &SymbolTable) -> String {
    match desc {
        TypeDescription::Int => "Int".into(),
        TypeDescription::UInt => "UInt".into(),
        TypeDescription::Float => "Float".into(),
        TypeDescription::Bool(_) => "Bool".into(),
        TypeDescription::Char => "Char".into(),
        TypeDescription::Byte => "Byte".into(),
        TypeDescription::String(_) => "String".into(),
        TypeDescription::Statement => "()".into(),
        TypeDescription::Never => "!".into(),
        TypeDescription::Html => "Html".into(),
        TypeDescription::Any => "Any".into(),
        TypeDescription::Tag(s) => format!(".{s}"),
        TypeDescription::TypeOf(s) => format!("typeof({s})"),
        TypeDescription::TemplParam(s) => s.clone(),
        TypeDescription::Go(s) => format!("go({s})"),
        TypeDescription::KeyOf(inner) => format!("keyof({})", fmt_type_expr(inner, symbols)),
        TypeDescription::Tuple(ts) => {
            let inner = ts
                .iter()
                .map(|t| fmt_type_expr(t, symbols))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({inner})")
        }
        TypeDescription::Array(inner) => format!("[{}]", fmt_type_expr(inner, symbols)),
        TypeDescription::Indexed { base, index } => {
            format!(
                "{}[{}]",
                fmt_type_expr(base, symbols),
                fmt_type_expr(index, symbols)
            )
        }
        TypeDescription::Or(ts) => ts
            .iter()
            .map(|t| fmt_type_expr(t, symbols))
            .collect::<Vec<_>>()
            .join(" | "),
        TypeDescription::And(ts) => ts
            .iter()
            .map(|t| fmt_type_expr(t, symbols))
            .collect::<Vec<_>>()
            .join(" & "),
        TypeDescription::Fun {
            params,
            return_type,
            ..
        } => {
            let ps = params
                .iter()
                .map(|p| fmt_type_expr(&p.type_expr, symbols))
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({ps}) -> {}", fmt_type_expr(return_type, symbols))
        }
        TypeDescription::Ref(inner) => format!("&{}", fmt_type_expr(inner, symbols)),
        TypeDescription::RefMut(inner) => format!("&mut {}", fmt_type_expr(inner, symbols)),
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } => {
            let name = &symbols.get(*type_ref).name;
            if type_params.is_empty() {
                name.clone()
            } else {
                let ps = type_params
                    .iter()
                    .map(|t| fmt_type_expr(t, symbols))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}<{ps}>")
            }
        }
        TypeDescription::Duck(_) => "{ ... }".into(),
        TypeDescription::NamedDuck { name, .. } => format!("duck {name}"),
        TypeDescription::Struct { name, .. } => format!("struct {name}"),
        TypeDescription::GoPackage(pkg) => format!("go:{pkg}"),
        TypeDescription::GoNamed(s) => s.clone(),
    }
}

fn strip_noise(s: &str) -> String {
    let mut out = String::new();
    let mut skip: u32 = 0;
    for line in s.lines() {
        let trimmed = line.trim_start();
        if skip > 0 {
            for c in trimmed.chars() {
                match c {
                    '{' | '[' => skip += 1,
                    '}' | ']' => skip -= 1,
                    _ => {}
                }
            }
            continue;
        }
        let noise =
            trimmed.starts_with("span: Span") || trimmed == "ty: ()," || trimmed == "ty: ()";
        if noise {
            for c in trimmed.chars() {
                if c == '{' {
                    skip += 1;
                }
            }
            continue;
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}
