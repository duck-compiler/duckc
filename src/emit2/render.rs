use super::go_ir::*;

pub fn render(file: &GoFile) -> String {
    let mut out = String::new();
    out.push_str(&format!("package {}\n", file.package));

    if !file.imports.is_empty() {
        out.push('\n');
        if file.imports.len() == 1 {
            out.push_str(&format!("import \"{}\"\n", file.imports[0]));
        } else {
            out.push_str("import (\n");
            for imp in &file.imports {
                out.push_str(&format!("\t\"{imp}\"\n"));
            }
            out.push_str(")\n");
        }
    }

    for decl in &file.decls {
        out.push('\n');
        render_decl(&mut out, decl);
    }

    out
}

fn render_decl(out: &mut String, decl: &GoDecl) {
    match decl {
        GoDecl::Func {
            name,
            receiver,
            params,
            ret,
            body,
        } => {
            out.push_str("func ");
            if let Some(recv) = receiver {
                out.push('(');
                out.push_str(&recv.name);
                out.push(' ');
                render_type(out, &recv.ty);
                out.push_str(") ");
            }
            out.push_str(name);
            out.push('(');
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&p.name);
                out.push(' ');
                render_type(out, &p.ty);
            }
            out.push(')');
            if let Some(r) = ret {
                out.push(' ');
                render_type(out, r);
            }
            out.push_str(" {\n");
            for stmt in body {
                render_stmt(out, stmt, 1);
            }
            out.push_str("}\n");
        }

        GoDecl::Struct { name, fields } => {
            out.push_str(&format!("type {name} struct {{\n"));
            for f in fields {
                out.push('\t');
                out.push_str(&f.name);
                out.push(' ');
                render_type(out, &f.ty);
                out.push('\n');
            }
            out.push_str("}\n");
        }

        GoDecl::TypeAlias { name, ty } => {
            out.push_str(&format!("type {name} = "));
            render_type(out, ty);
            out.push('\n');
        }

        GoDecl::Var { name, ty, value } => {
            out.push_str(&format!("var {name} "));
            render_type(out, ty);
            out.push_str(" = ");
            render_expr(out, value);
            out.push('\n');
        }

        GoDecl::Interface { name, methods } => {
            out.push_str(&format!("type {name}[T any] interface {{\n"));
            for m in methods {
                out.push('\t');
                out.push_str(&m.name);
                out.push('(');
                for (i, p) in m.params.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&p.name);
                    out.push(' ');
                    render_type(out, &p.ty);
                }
                out.push(')');
                if let Some(r) = &m.ret {
                    out.push(' ');
                    render_type(out, r);
                }
                out.push('\n');
            }
            out.push_str("}\n");
        }

        GoDecl::Raw(s) => {
            out.push_str(s);
            out.push('\n');
        }
    }
}

fn render_stmt(out: &mut String, stmt: &GoStmt, depth: usize) {
    let pad = "\t".repeat(depth);
    match stmt {
        GoStmt::Declare { name, ty } => {
            out.push_str(&pad);
            out.push_str("var ");
            out.push_str(name);
            out.push(' ');
            render_type(out, ty);
            out.push('\n');
        }

        GoStmt::DeclareAssign { name, value } => {
            out.push_str(&pad);
            out.push_str(name);
            out.push_str(" := ");
            render_expr(out, value);
            out.push('\n');
        }

        GoStmt::MultiDeclareAssign { names, value } => {
            out.push_str(&pad);
            out.push_str(&names.join(", "));
            out.push_str(" := ");
            render_expr(out, value);
            out.push('\n');
        }

        GoStmt::Assign { target, value } => {
            out.push_str(&pad);
            render_expr(out, target);
            out.push_str(" = ");
            render_expr(out, value);
            out.push('\n');
        }

        GoStmt::Return(v) => {
            out.push_str(&pad);
            match v {
                Some(e) => {
                    out.push_str("return ");
                    render_expr(out, e);
                    out.push('\n');
                }
                None => out.push_str("return\n"),
            }
        }

        GoStmt::If { cond, then, else_ } => {
            out.push_str(&pad);
            out.push_str("if ");
            render_expr(out, cond);
            out.push_str(" {\n");
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
                    out.push_str("} else {\n");
                    for s in else_stmts {
                        render_stmt(out, s, depth + 1);
                    }
                    out.push_str(&pad);
                    out.push_str("}\n");
                }
            }
        }

        GoStmt::ForCond { cond, body } => {
            out.push_str(&pad);
            out.push_str("for ");
            render_expr(out, cond);
            out.push_str(" {\n");
            for s in body {
                render_stmt(out, s, depth + 1);
            }
            out.push_str(&pad);
            out.push_str("}\n");
        }

        GoStmt::ForRange { val, iter, body } => {
            out.push_str(&pad);
            out.push_str("for _, ");
            out.push_str(val);
            out.push_str(" := range ");
            render_expr(out, iter);
            out.push_str(" {\n");
            for s in body {
                render_stmt(out, s, depth + 1);
            }
            out.push_str(&pad);
            out.push_str("}\n");
        }

        GoStmt::Loop(body) => {
            out.push_str(&pad);
            out.push_str("for {\n");
            for s in body {
                render_stmt(out, s, depth + 1);
            }
            out.push_str(&pad);
            out.push_str("}\n");
        }

        GoStmt::Break => out.push_str(&format!("{pad}break\n")),
        GoStmt::Continue => out.push_str(&format!("{pad}continue\n")),

        GoStmt::Defer(e) => {
            out.push_str(&pad);
            out.push_str("defer ");
            render_expr(out, e);
            out.push('\n');
        }

        GoStmt::Expr(e) => {
            out.push_str(&pad);
            render_expr(out, e);
            out.push('\n');
        }

        GoStmt::Block(stmts) => {
            out.push_str(&pad);
            out.push_str("{\n");
            for s in stmts {
                render_stmt(out, s, depth + 1);
            }
            out.push_str(&pad);
            out.push_str("}\n");
        }

        GoStmt::TypeSwitch {
            value,
            arms,
            default,
        } => {
            out.push_str(&pad);
            let has_binding = arms.iter().any(|a| !a.binding.is_empty());
            if has_binding {
                out.push_str("switch _sw := (");
                render_expr(out, value);
                out.push_str(").(type) {\n");
            } else {
                out.push_str("switch (");
                render_expr(out, value);
                out.push_str(").(type) {\n");
            }
            for arm in arms {
                out.push_str(&pad);
                out.push_str("case ");
                render_type(out, &arm.ty);
                out.push_str(":\n");
                if !arm.binding.is_empty() {
                    out.push_str(&"\t".repeat(depth + 1));
                    out.push_str(&arm.binding);
                    out.push_str(" := _sw\n");
                }
                for s in &arm.body {
                    render_stmt(out, s, depth + 1);
                }
            }
            if let Some(def) = default {
                out.push_str(&pad);
                out.push_str("default:\n");
                for s in def {
                    render_stmt(out, s, depth + 1);
                }
            }
            out.push_str(&pad);
            out.push_str("}\n");
        }
    }
}

fn render_expr(out: &mut String, expr: &GoExpr) {
    match expr {
        GoExpr::Ident(s) => out.push_str(s),
        GoExpr::Int(v) => out.push_str(&v.to_string()),
        GoExpr::UInt(v) => out.push_str(&v.to_string()),
        GoExpr::Float(v) => out.push_str(&format!("{v:?}")),
        GoExpr::Bool(v) => out.push_str(if *v { "true" } else { "false" }),
        GoExpr::Char(c) => out.push_str(&format!("'{}'", c.escape_default())),
        GoExpr::Str(s) => {
            out.push('"');
            out.push_str(
                &s.replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n"),
            );
            out.push('"');
        }
        GoExpr::Nil => out.push_str("nil"),
        GoExpr::Raw(s) => out.push_str(s),

        GoExpr::BinOp { op, lhs, rhs } => {
            out.push('(');
            render_expr(out, lhs);
            out.push(' ');
            out.push_str(bin_op_str(op));
            out.push(' ');
            render_expr(out, rhs);
            out.push(')');
        }

        GoExpr::UnaryOp { op, operand } => {
            out.push_str(unary_op_str(op));
            out.push('(');
            render_expr(out, operand);
            out.push(')');
        }

        GoExpr::Call { callee, args } => {
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

        GoExpr::Field { base, field } => {
            render_expr(out, base);
            out.push('.');
            out.push_str(field);
        }

        GoExpr::Index { base, index } => {
            render_expr(out, base);
            out.push('[');
            render_expr(out, index);
            out.push(']');
        }

        GoExpr::StructLit { ty, fields } => {
            out.push_str(ty);
            out.push_str("{");
            for (i, (name, val)) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(name);
                out.push_str(": ");
                render_expr(out, val);
            }
            out.push('}');
        }

        GoExpr::SliceLit { ty, elems } => {
            out.push_str("[]");
            render_type(out, ty);
            out.push('{');
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                render_expr(out, e);
            }
            out.push('}');
        }

        GoExpr::Closure { params, ret, body } => {
            out.push_str("func(");
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&p.name);
                out.push(' ');
                render_type(out, &p.ty);
            }
            out.push(')');
            if let Some(r) = ret {
                out.push(' ');
                render_type(out, r);
            }
            out.push_str(" {\n");
            for s in body {
                render_stmt(out, s, 1);
            }
            out.push('}');
        }

        GoExpr::Cast { ty, value } => {
            render_type(out, ty);
            out.push('(');
            render_expr(out, value);
            out.push(')');
        }

        GoExpr::Ref(inner) => {
            out.push('&');
            render_expr(out, inner);
        }

        GoExpr::Deref(inner) => {
            out.push('*');
            render_expr(out, inner);
        }

        GoExpr::TypeAssert { value, ty } => {
            render_expr(out, value);
            out.push_str(".(");
            render_type(out, ty);
            out.push(')');
        }
    }
}

fn render_type(out: &mut String, ty: &GoType) {
    match ty {
        GoType::Int64 => out.push_str("int"),
        GoType::Uint64 => out.push_str("uint64"),
        GoType::Float64 => out.push_str("float64"),
        GoType::Bool => out.push_str("bool"),
        GoType::String => out.push_str("string"),
        GoType::Rune => out.push_str("rune"),
        GoType::Byte => out.push_str("byte"),
        GoType::Slice(elem) => {
            out.push_str("[]");
            render_type(out, elem);
        }
        GoType::Func { params, ret } => {
            out.push_str("func(");
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                render_type(out, p);
            }
            out.push(')');
            if let Some(r) = ret {
                out.push(' ');
                render_type(out, r);
            }
        }
        GoType::Ptr(inner) => {
            out.push('*');
            render_type(out, inner);
        }
        GoType::Named(name) => out.push_str(name),
        GoType::Any => out.push_str("any"),
        GoType::DuckInterface(constraints) => {
            out.push_str("interface {");
            for (iface_name, ty) in constraints {
                out.push_str("\n\t");
                out.push_str(iface_name);
                out.push('[');
                render_type(out, ty);
                out.push(']');
            }
            out.push_str("\n}");
        }
    }
}

fn bin_op_str(op: &GoBinOp) -> &'static str {
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

fn unary_op_str(op: &GoUnaryOp) -> &'static str {
    match op {
        GoUnaryOp::Neg => "-",
        GoUnaryOp::Not => "!",
        GoUnaryOp::BitNot => "^",
    }
}
