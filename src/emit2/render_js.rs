use super::js_ir::*;

pub fn render_js(file: &JsFile) -> String {
    let mut out = String::new();
    for decl in &file.decls {
        render_decl(&mut out, decl, 0);
        out.push('\n');
    }
    out
}

fn render_decl(out: &mut String, decl: &JsDecl, depth: usize) {
    match decl {
        JsDecl::Function {
            name,
            params,
            is_async,
            body,
        } => {
            if *is_async {
                out.push_str("async ");
            }
            out.push_str("function ");
            out.push_str(name);
            out.push('(');
            out.push_str(&params.join(", "));
            out.push_str(") {\n");
            for stmt in body {
                render_stmt(out, stmt, depth + 1);
            }
            out.push_str("}\n");
        }

        JsDecl::Const { name, value } => {
            out.push_str("const ");
            out.push_str(name);
            out.push_str(" = ");
            render_expr(out, value);
            out.push_str(";\n");
        }

        JsDecl::Class { name, methods } => {
            out.push_str("class ");
            out.push_str(name);
            out.push_str(" {\n");
            for m in methods {
                let pad = "\t";
                out.push_str(pad);
                if m.is_static {
                    out.push_str("static ");
                }
                if m.is_async {
                    out.push_str("async ");
                }
                out.push_str(&m.name);
                out.push('(');
                out.push_str(&m.params.join(", "));
                out.push_str(") {\n");
                for stmt in &m.body {
                    render_stmt(out, stmt, 2);
                }
                out.push_str(pad);
                out.push_str("}\n");
            }
            out.push_str("}\n");
        }

        JsDecl::Export(inner) => {
            out.push_str("export ");
            render_decl(out, inner, depth);
        }
    }
}

fn render_stmt(out: &mut String, stmt: &JsStmt, depth: usize) {
    let pad = "\t".repeat(depth);
    match stmt {
        JsStmt::Const { name, value } => {
            out.push_str(&pad);
            out.push_str("const ");
            out.push_str(name);
            out.push_str(" = ");
            render_expr(out, value);
            out.push_str(";\n");
        }

        JsStmt::Let { name, value } => {
            out.push_str(&pad);
            out.push_str("let ");
            out.push_str(name);
            if let Some(v) = value {
                out.push_str(" = ");
                render_expr(out, v);
            }
            out.push_str(";\n");
        }

        JsStmt::MultiConst { names, value } => {
            out.push_str(&pad);
            out.push_str("const [");
            out.push_str(&names.join(", "));
            out.push_str("] = ");
            render_expr(out, value);
            out.push_str(";\n");
        }

        JsStmt::Assign { target, value } => {
            out.push_str(&pad);
            render_expr(out, target);
            out.push_str(" = ");
            render_expr(out, value);
            out.push_str(";\n");
        }

        JsStmt::Return(v) => {
            out.push_str(&pad);
            match v {
                Some(e) => {
                    out.push_str("return ");
                    render_expr(out, e);
                    out.push_str(";\n");
                }
                None => out.push_str("return;\n"),
            }
        }

        JsStmt::If { cond, then, else_ } => {
            out.push_str(&pad);
            out.push_str("if (");
            render_expr(out, cond);
            out.push_str(") {\n");
            for s in then {
                render_stmt(out, s, depth + 1);
            }
            match else_ {
                None => {
                    out.push_str(&pad);
                    out.push_str("}\n");
                }
                Some(else_stmts) => {
                    out.push_str(&pad);
                    // Flatten `else { if (...) { ... } }` into `else if (...)` when the sole
                    // child is itself an `if` statement.
                    if else_stmts.len() == 1 {
                        if let JsStmt::If { .. } = &else_stmts[0] {
                            out.push_str("} else ");
                            // Re-render without the leading pad (we just wrote it).
                            render_stmt_no_pad(out, &else_stmts[0], depth, &pad);
                            return;
                        }
                    }
                    out.push_str("} else {\n");
                    for s in else_stmts {
                        render_stmt(out, s, depth + 1);
                    }
                    out.push_str(&pad);
                    out.push_str("}\n");
                }
            }
        }

        JsStmt::ForOf { val, iter, body } => {
            out.push_str(&pad);
            out.push_str("for (const ");
            out.push_str(val);
            out.push_str(" of ");
            render_expr(out, iter);
            out.push_str(") {\n");
            for s in body {
                render_stmt(out, s, depth + 1);
            }
            out.push_str(&pad);
            out.push_str("}\n");
        }

        JsStmt::While { cond, body } => {
            out.push_str(&pad);
            out.push_str("while (");
            render_expr(out, cond);
            out.push_str(") {\n");
            for s in body {
                render_stmt(out, s, depth + 1);
            }
            out.push_str(&pad);
            out.push_str("}\n");
        }

        JsStmt::Loop(body) => {
            out.push_str(&pad);
            out.push_str("while (true) {\n");
            for s in body {
                render_stmt(out, s, depth + 1);
            }
            out.push_str(&pad);
            out.push_str("}\n");
        }

        JsStmt::Break => out.push_str(&format!("{pad}break;\n")),
        JsStmt::Continue => out.push_str(&format!("{pad}continue;\n")),

        JsStmt::Expr(e) => {
            out.push_str(&pad);
            render_expr(out, e);
            out.push_str(";\n");
        }

        JsStmt::Block(stmts) => {
            out.push_str(&pad);
            out.push_str("{\n");
            for s in stmts {
                render_stmt(out, s, depth + 1);
            }
            out.push_str(&pad);
            out.push_str("}\n");
        }

        JsStmt::Throw(e) => {
            out.push_str(&pad);
            out.push_str("throw ");
            render_expr(out, e);
            out.push_str(";\n");
        }
    }
}

/// Render an `if` statement that was already introduced by an `else` keyword - skip the leading
/// pad since the caller already wrote `else `.
fn render_stmt_no_pad(out: &mut String, stmt: &JsStmt, depth: usize, pad: &str) {
    if let JsStmt::If { cond, then, else_ } = stmt {
        out.push_str("if (");
        render_expr(out, cond);
        out.push_str(") {\n");
        for s in then {
            render_stmt(out, s, depth + 1);
        }
        match else_ {
            None => {
                out.push_str(pad);
                out.push_str("}\n");
            }
            Some(else_stmts) => {
                out.push_str(pad);
                if else_stmts.len() == 1 {
                    if let JsStmt::If { .. } = &else_stmts[0] {
                        out.push_str("} else ");
                        render_stmt_no_pad(out, &else_stmts[0], depth, pad);
                        return;
                    }
                }
                out.push_str("} else {\n");
                for s in else_stmts {
                    render_stmt(out, s, depth + 1);
                }
                out.push_str(pad);
                out.push_str("}\n");
            }
        }
    }
}

fn render_expr(out: &mut String, expr: &JsExpr) {
    match expr {
        JsExpr::Ident(s) => out.push_str(s),
        JsExpr::Int(v) => out.push_str(&v.to_string()),
        JsExpr::Float(v) => out.push_str(&format!("{v:?}")),
        JsExpr::Bool(v) => out.push_str(if *v { "true" } else { "false" }),
        JsExpr::Str(s) => {
            out.push('"');
            out.push_str(
                &s.replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n"),
            );
            out.push('"');
        }
        JsExpr::Null => out.push_str("null"),
        JsExpr::Undefined => out.push_str("undefined"),
        JsExpr::Raw(s) => out.push_str(s.trim()),

        JsExpr::BinOp { op, lhs, rhs } => {
            out.push('(');
            render_expr(out, lhs);
            out.push(' ');
            out.push_str(bin_op_str(op));
            out.push(' ');
            render_expr(out, rhs);
            out.push(')');
        }

        JsExpr::UnaryOp { op, operand } => {
            out.push_str(unary_op_str(op));
            out.push('(');
            render_expr(out, operand);
            out.push(')');
        }

        JsExpr::Call { callee, args } => {
            render_expr(out, callee);
            out.push('(');
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                render_expr(out, a);
            }
            out.push(')');
        }

        JsExpr::New { callee, args } => {
            out.push_str("new ");
            render_expr(out, callee);
            out.push('(');
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                render_expr(out, a);
            }
            out.push(')');
        }

        JsExpr::Field { base, field } => {
            render_expr(out, base);
            out.push('.');
            out.push_str(field);
        }

        JsExpr::OptionalField { base, field } => {
            render_expr(out, base);
            out.push_str("?.");
            out.push_str(field);
        }

        JsExpr::Index { base, index } => {
            render_expr(out, base);
            out.push('[');
            render_expr(out, index);
            out.push(']');
        }

        JsExpr::Object { fields } => {
            out.push_str("{ ");
            for (i, (name, val)) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                // Shorthand: `{ x: x }` -> `{ x }` when key === value ident.
                if let JsExpr::Ident(ident) = val {
                    if ident == name {
                        out.push_str(name);
                        continue;
                    }
                }
                render_object_key(out, name);
                out.push_str(": ");
                render_expr(out, val);
            }
            out.push_str(" }");
        }

        JsExpr::Array { elems } => {
            out.push('[');
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                render_expr(out, e);
            }
            out.push(']');
        }

        JsExpr::Arrow {
            params,
            body,
            is_async,
        } => {
            if *is_async {
                out.push_str("async ");
            }
            if params.len() == 1 {
                out.push_str(&params[0]);
            } else {
                out.push('(');
                out.push_str(&params.join(", "));
                out.push(')');
            }
            out.push_str(" => ");
            match body {
                JsArrowBody::Expr(e) => render_expr(out, e),
                JsArrowBody::Block(stmts) => {
                    out.push_str("{\n");
                    for s in stmts {
                        render_stmt(out, s, 1);
                    }
                    out.push('}');
                }
            }
        }

        JsExpr::Template { parts } => {
            out.push('`');
            for part in parts {
                match part {
                    JsTemplatePart::Str(s) => {
                        out.push_str(&s.replace('`', "\\`").replace("${", "\\${"));
                    }
                    JsTemplatePart::Expr(e) => {
                        out.push_str("${");
                        render_expr(out, e);
                        out.push('}');
                    }
                }
            }
            out.push('`');
        }

        JsExpr::Instanceof { value, class } => {
            out.push('(');
            render_expr(out, value);
            out.push_str(" instanceof ");
            out.push_str(class);
            out.push(')');
        }

        JsExpr::Spread(inner) => {
            out.push_str("...");
            render_expr(out, inner);
        }

        JsExpr::Await(inner) => {
            out.push_str("await ");
            render_expr(out, inner);
        }

        JsExpr::Typeof(inner) => {
            out.push_str("typeof ");
            render_expr(out, inner);
        }

        JsExpr::Ternary { cond, then, else_ } => {
            out.push('(');
            render_expr(out, cond);
            out.push_str(" ? ");
            render_expr(out, then);
            out.push_str(" : ");
            render_expr(out, else_);
            out.push(')');
        }
    }
}

/// Render an object key, quoting it if it isn't a valid JS identifier.
fn render_object_key(out: &mut String, key: &str) {
    if key.starts_with('$') || key.chars().all(|c| c.is_alphanumeric() || c == '_') {
        out.push_str(key);
    } else {
        out.push('"');
        out.push_str(key);
        out.push('"');
    }
}

fn bin_op_str(op: &JsBinOp) -> &'static str {
    match op {
        JsBinOp::Add => "+",
        JsBinOp::Sub => "-",
        JsBinOp::Mul => "*",
        JsBinOp::Div => "/",
        JsBinOp::Mod => "%",
        JsBinOp::BitAnd => "&",
        JsBinOp::BitOr => "|",
        JsBinOp::BitXor => "^",
        JsBinOp::Shl => "<<",
        JsBinOp::Shr => ">>",
        JsBinOp::Eq => "==",
        JsBinOp::Neq => "!=",
        JsBinOp::StrictEq => "===",
        JsBinOp::StrictNeq => "!==",
        JsBinOp::Lt => "<",
        JsBinOp::Lte => "<=",
        JsBinOp::Gt => ">",
        JsBinOp::Gte => ">=",
        JsBinOp::And => "&&",
        JsBinOp::Or => "||",
        JsBinOp::NullCoalesce => "??",
    }
}

fn unary_op_str(op: &JsUnaryOp) -> &'static str {
    match op {
        JsUnaryOp::Neg => "-",
        JsUnaryOp::Not => "!",
        JsUnaryOp::BitNot => "~",
    }
}
