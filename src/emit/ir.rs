use std::collections::HashSet;

use crate::emit::{
    fix_ident_for_go,
    value::{Case, IrInstruction, IrValue},
};

impl IrInstruction {
    fn emit_as_go(&self) -> String {
        #![allow(clippy::format_in_format_args)]
        match self {
            IrInstruction::GlobalVarDecl {
                name,
                go_type,
                init_code,
            } => {
                format!(
                    "var {name} {go_type} = func() {go_type} {{ {} }}()",
                    join_ir(init_code)
                )
            }
            IrInstruction::Defer(d) => {
                let fun_call @ (IrInstruction::FunCall(None, ..) | IrInstruction::InlineGo(..)) =
                    d.as_ref()
                else {
                    panic!("Compiler Bug: can only defer a func call without result {d:?}")
                };
                format!("defer {}", fun_call.emit_as_go())
            }
            IrInstruction::ForRangeElem {
                ident: _,
                range_target,
                body,
                label,
            } => {
                format!(
                    "{{\nif false {{goto {label}}}\n{label}:\nfor DUCK_FOR_IDX := range {} {{\n_ = DUCK_FOR_IDX\n{}\n}}\n}}",
                    range_target.emit_as_go(),
                    body.iter()
                        .map(|i| i.emit_as_go())
                        .fold(String::new(), |mut acc, x| {
                            acc.push_str(&x);
                            acc.push('\n');
                            acc
                        })
                )
            }
            IrInstruction::StringConcat(target, v) => {
                format!(
                    "{target} = {}",
                    if v.is_empty() {
                        String::from("\"\"")
                    } else {
                        v.iter()
                            .map(|x| x.emit_as_go().to_string())
                            .collect::<Vec<_>>()
                            .join(" + ")
                    }
                )
            }
            IrInstruction::SwitchType(against, type_cases) => {
                fn emit_case_go(case: &Case, actual: &str, else_case: Option<&Case>) -> String {
                    fn format_case_output(case: &Case, instructions: &[IrInstruction]) -> String {
                        format!(
                            "case {}: {{\n{}\nbreak\n}}",
                            case.type_name,
                            instructions
                                .iter()
                                .map(|instr| instr.emit_as_go())
                                .collect::<Vec<_>>()
                                .join("\n")
                        )
                    }

                    let mut instructions = vec![];
                    let type_name = case.type_name.clone();

                    let Some(branches) = &case.conditional_branches else {
                        if let Some(identifier) = &case.identifier_binding {
                            instructions.push(IrInstruction::VarDecl(
                                identifier.clone(),
                                type_name.clone(),
                            ));
                            instructions.push(IrInstruction::VarAssignment(
                                identifier.clone(),
                                IrValue::Imm(format!("{actual}.({type_name})")),
                            ));
                        }
                        instructions.extend(case.instrs.clone());
                        return format_case_output(case, &instructions);
                    };

                    if branches.is_empty() {
                        if let Some(identifier) = &case.identifier_binding {
                            instructions.push(IrInstruction::VarDecl(
                                identifier.clone(),
                                type_name.clone(),
                            ));
                            instructions.push(IrInstruction::VarAssignment(
                                identifier.clone(),
                                IrValue::Imm(format!("{actual}.({type_name})")),
                            ));
                        }
                        instructions.extend(case.instrs.clone());
                        return format_case_output(case, &instructions);
                    }

                    for branch in branches {
                        let mut block_instructions = vec![];

                        if let Some(identifier) = &branch.1.identifier_binding {
                            block_instructions.push(IrInstruction::VarDecl(
                                identifier.clone(),
                                type_name.clone(),
                            ));
                            block_instructions.push(IrInstruction::VarAssignment(
                                identifier.clone(),
                                IrValue::Imm(format!("{actual}.({type_name})")),
                            ));
                        }

                        block_instructions.extend(branch.0.0.clone());

                        let mut if_body = branch.1.instrs.clone();
                        if_body.push(IrInstruction::Break(None));

                        block_instructions.push(IrInstruction::If(
                            branch
                                .0
                                .1
                                .clone()
                                .expect("compiler error: we expect that there's an value"),
                            if_body,
                            None,
                        ));

                        instructions.push(IrInstruction::Block(block_instructions));
                    }

                    if let Some(else_case) = else_case {
                        let Some(last_instruction) = instructions.last_mut() else {
                            return format_case_output(case, &instructions);
                        };
                        let IrInstruction::Block(block_instrs) = last_instruction else {
                            return format_case_output(case, &instructions);
                        };
                        let Some(last_block_instr) = block_instrs.last_mut() else {
                            return format_case_output(case, &instructions);
                        };
                        let IrInstruction::If(_, _, else_branch) = last_block_instr else {
                            return format_case_output(case, &instructions);
                        };
                        *else_branch = Some(else_case.instrs.clone());
                    }

                    format_case_output(case, &instructions)
                }

                let else_case = type_cases.iter().find(|case| case.type_name == "__else");

                let processed_cases: Vec<String> = type_cases
                    .iter()
                    .map(|case| emit_case_go(case, &against.emit_as_go(), else_case))
                    .collect();

                format!(
                    "switch ({}).(type) {{\n{}\n}}",
                    against.emit_as_go(),
                    processed_cases.join("\n"),
                )
            }
            IrInstruction::GoPackage(s) => format!("package {s}"),
            IrInstruction::Add(r, left, right, _type_expr) => {
                // TODO: check if this is correct
                format!("{r} = {} + {}", left.emit_as_go(), right.emit_as_go(),)
            }
            IrInstruction::Mul(r, v1, v2, _type_expr) => {
                // TODO: check if this is correct
                format!("{r} = {} * {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::Sub(r, v1, v2, _type_expr) => {
                format!("{r} = {} - {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::Div(r, v1, v2, _type_expr) => {
                format!("{r} = {} / {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::Mod(r, v1, v2, _type_expr) => {
                format!("{r} = {} % {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::Continue(label) => format!(
                "continue{}",
                label.as_ref().map(|l| format!(" {l}")).unwrap_or_default()
            ),
            IrInstruction::Break(label) => format!(
                "break{}",
                label.as_ref().map(|l| format!(" {l}")).unwrap_or_default()
            ),
            IrInstruction::Return(o) => format!(
                "return {}",
                o.as_ref()
                    .map(IrValue::emit_as_go)
                    .unwrap_or("".to_string())
            ),
            IrInstruction::Equals(r, v1, v2, _type_expr) => {
                format!("{r} = {} == {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::NotEquals(r, v1, v2, _type_expr) => {
                format!("{r} = {} != {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::LessThan(r, v1, v2, _type_expr) => {
                format!("{r} = {} < {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::LessThanOrEquals(r, v1, v2, _type_expr) => {
                format!("{r} = {} <= {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::GreaterThan(r, v1, v2, _type_expr) => {
                format!("{r} = {} > {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::GreaterThanOrEquals(r, v1, v2, _type_expr) => {
                format!("{r} = {} >= {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::And(r, v1, v2, _type_expr) => {
                format!("{r} = {} && {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::Or(r, v1, v2, _type_expr) => {
                format!("{r} = {} || {}", v1.emit_as_go(), v2.emit_as_go(),)
            }
            IrInstruction::Block(block_instr) => {
                format!("{{\n{}\n}}", join_ir(block_instr))
            }
            IrInstruction::FunCall(r, t, p) => {
                format!(
                    "{}{}({})",
                    r.as_ref().map(|x| format!("{x} = ")).unwrap_or_default(),
                    t.emit_as_go(),
                    p.iter()
                        .map(IrValue::emit_as_go)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            IrInstruction::VarDecl(name, ty) => {
                if ty == "Tup_" {
                    format!("var {name} {ty}\n{name} = Tup_{{}}\n_ = {name}")
                } else {
                    format!("var {name} {ty}\n_ = {name}")
                }
            }
            IrInstruction::VarAssignment(name, v) => format!("{name} = {}", v.emit_as_go()),
            IrInstruction::If(cond, then, els) => {
                format!(
                    "if ({}) {{\n{}\n}} {}",
                    cond.emit_as_go(),
                    then.iter()
                        .map(IrInstruction::emit_as_go)
                        .collect::<Vec<_>>()
                        .join("\n"),
                    els.as_ref()
                        .map(|x| {
                            format!(
                                "else {{\n{}\n}} ",
                                x.iter()
                                    .map(IrInstruction::emit_as_go)
                                    .collect::<Vec<_>>()
                                    .join("\n")
                            )
                        })
                        .unwrap_or("".to_string())
                )
            }
            IrInstruction::Loop(v, label) => {
                format!(
                    "if false {{goto {label}}}\n{label}:\nfor {{\n{}\n}}",
                    join_ir(v)
                )
            }
            IrInstruction::InlineGo(t) => t.to_string(),
            IrInstruction::GoImports(imports) => {
                format!(
                    "import (\n{}\n)",
                    imports
                        .iter()
                        .map(|(n, m)| format!("{} \"{m}\"", n.clone().unwrap_or_default()))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            IrInstruction::GenericFun(name, generics, params, return_type, body) => {
                format!(
                    "func {name}[{}]({}) {} {{\n{}\n}}",
                    generics
                        .iter()
                        .map(|(n, ty)| format!("{n} {ty}"))
                        .collect::<Vec<_>>()
                        .join(", "),
                    params
                        .iter()
                        .map(|(n, ty)| format!("{n} {ty}"))
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type
                        .as_ref()
                        .map(|return_type| if name == "main" {
                            String::new()
                        } else {
                            return_type.clone()
                        })
                        .unwrap_or(String::new()),
                    format!(
                        "{}\n{}",
                        params
                            .iter()
                            .map(|(name, _)| format!("_ = {name}"))
                            .collect::<Vec<_>>()
                            .join("\n"),
                        join_ir(body)
                    ),
                )
            }
            IrInstruction::FunDef(name, receiver, params, return_type, body) => {
                format!(
                    "func {} {name}({}) {} {{\n{}\n}}",
                    receiver
                        .as_ref()
                        .map(|(self_name, recv_type)| format!("({self_name} {recv_type})"))
                        .unwrap_or_default(),
                    params
                        .iter()
                        .map(|(n, ty)| format!("{n} {ty}"))
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type
                        .as_ref()
                        .map(|return_type| if name == "main" {
                            String::new()
                        } else {
                            return_type.clone()
                        })
                        .unwrap_or(String::new()),
                    format!(
                        "{}\n{}",
                        params
                            .iter()
                            .map(|(name, _)| format!("_ = {name}"))
                            .collect::<Vec<_>>()
                            .join("\n"),
                        join_ir(body)
                    ),
                )
            }
            IrInstruction::StructDef(name, fields) => {
                format!(
                    "type {name} struct {{\n{}\n}}",
                    fields
                        .iter()
                        .map(|(n, ty)| format!("{n} {ty}"))
                        .collect::<Vec<_>>()
                        .join("\n"),
                )
            }
            IrInstruction::InterfaceDef(name, generics, fields) => {
                format!(
                    "type {name}{} interface {{\n{}\n}}",
                    {
                        let generics = generics
                            .iter()
                            .map(|(type_param_name, type_name)| {
                                format!("{type_param_name} {type_name}")
                            })
                            .collect::<Vec<String>>()
                            .join(", ");
                        if !generics.is_empty() {
                            format!("[{generics}]")
                        } else {
                            "".to_string()
                        }
                    },
                    fields
                        .iter()
                        .map(|(n, params, ty)| format!(
                            "{n}({}) {}",
                            params
                                .iter()
                                .map(|(param_name, param_type)| format!(
                                    "{param_name} {param_type}"
                                ))
                                .collect::<Vec<_>>()
                                .join(", "),
                            ty.as_ref().unwrap_or(&String::new())
                        ))
                        .collect::<Vec<_>>()
                        .join("\n"),
                )
            }
        }
    }
}

pub fn fix_idents_in_ir_value(v: &mut IrValue, imports: &HashSet<String>) {
    match v {
        IrValue::Negate(v) => fix_idents_in_ir_value(v, imports),
        IrValue::Array(_ty, sub_values) => {
            for sub in sub_values {
                fix_idents_in_ir_value(sub, imports);
            }
        }

        IrValue::ArrayAccess(target, idx) => {
            fix_idents_in_ir_value(target, imports);
            fix_idents_in_ir_value(idx, imports);
        }

        IrValue::BoolNegate(neg) => {
            fix_idents_in_ir_value(neg, imports);
        }

        IrValue::Deref(v) => fix_idents_in_ir_value(v, imports),

        IrValue::FieldAccess(v, field_name) => {
            *field_name = fix_ident_for_go(field_name, imports);
            fix_idents_in_ir_value(v, imports);
        }

        IrValue::Imm(_s) => {
            // *s = fix_ident_for_go(s, imports);
        }

        IrValue::Lambda(params, _ret, body) => {
            for p in params {
                p.0 = fix_ident_for_go(&p.0, imports);
            }

            for body in body {
                fix_idents_in_ir(body, imports);
            }
        }

        IrValue::Struct(struct_name, fields) => {
            *struct_name = fix_ident_for_go(struct_name, imports);
            for field in fields {
                field.0 = fix_ident_for_go(&field.0, imports);
                fix_idents_in_ir_value(&mut field.1, imports);
            }
        }

        IrValue::Tuple(_tuple_struct_name, fields) => {
            for field in fields {
                fix_idents_in_ir_value(field, imports);
            }
        }

        IrValue::Duck(_duck_struct_name, fields) => {
            for field in fields {
                field.0 = fix_ident_for_go(&field.0, imports);
                fix_idents_in_ir_value(&mut field.1, imports);
            }
        }

        IrValue::Pointer(d) => fix_idents_in_ir_value(d, imports),
        IrValue::Var(var_name) => *var_name = fix_ident_for_go(var_name, imports),

        IrValue::Bool(..)
        | IrValue::Char(..)
        | IrValue::Nil
        | IrValue::String(..)
        | IrValue::Float(..)
        | IrValue::Int(..)
        | IrValue::Tag(..) => {}
    }
}

pub fn fix_idents_in_ir(v: &mut IrInstruction, imports: &HashSet<String>) {
    match v {
        IrInstruction::GlobalVarDecl {
            name,
            go_type: _,
            init_code,
        } => {
            *name = fix_ident_for_go(name, imports);
            for instr in init_code {
                fix_idents_in_ir(instr, imports);
            }
        }
        IrInstruction::Add(res, lhs, rhs, res_ty)
        | IrInstruction::Sub(res, lhs, rhs, res_ty)
        | IrInstruction::Mul(res, lhs, rhs, res_ty)
        | IrInstruction::Div(res, lhs, rhs, res_ty)
        | IrInstruction::Mod(res, lhs, rhs, res_ty)
        | IrInstruction::Equals(res, lhs, rhs, res_ty)
        | IrInstruction::NotEquals(res, lhs, rhs, res_ty)
        | IrInstruction::LessThan(res, lhs, rhs, res_ty)
        | IrInstruction::LessThanOrEquals(res, lhs, rhs, res_ty)
        | IrInstruction::GreaterThan(res, lhs, rhs, res_ty)
        | IrInstruction::GreaterThanOrEquals(res, lhs, rhs, res_ty)
        | IrInstruction::And(res, lhs, rhs, res_ty)
        | IrInstruction::Or(res, lhs, rhs, res_ty) => {
            let _res_type = res_ty;
            *res = fix_ident_for_go(res, imports);

            fix_idents_in_ir_value(lhs, imports);
            fix_idents_in_ir_value(rhs, imports);
        }

        IrInstruction::Block(sub) => {
            for sub in sub {
                fix_idents_in_ir(sub, imports);
            }
        }

        IrInstruction::Break(..) | IrInstruction::Continue(..) => {}

        IrInstruction::Defer(sub) => fix_idents_in_ir(sub, imports),
        IrInstruction::ForRangeElem {
            ident,
            range_target,
            body,
            label: _,
        } => {
            *ident = fix_ident_for_go(ident, imports);
            fix_idents_in_ir_value(range_target, imports);
            for body in body {
                fix_idents_in_ir(body, imports);
            }
        }

        IrInstruction::VarDecl(name, _ty) => {
            *name = fix_ident_for_go(name, imports);
        }

        IrInstruction::VarAssignment(name, v) => {
            *name = fix_ident_for_go(name, imports);
            fix_idents_in_ir_value(v, imports);
        }

        IrInstruction::FunCall(_result, target, params) => {
            fix_idents_in_ir_value(target, imports);
            for param in params {
                fix_idents_in_ir_value(param, imports);
            }
        }

        IrInstruction::StringConcat(_res, values) => {
            for v in values {
                fix_idents_in_ir_value(v, imports);
            }
        }

        IrInstruction::Return(v) => {
            if let Some(v) = v.as_mut() {
                fix_idents_in_ir_value(v, imports)
            }
        }

        IrInstruction::InlineGo(..) => {}
        IrInstruction::If(cond, then, el) => {
            fix_idents_in_ir_value(cond, imports);
            for instr in then
                .iter_mut()
                .chain(el.iter_mut().flat_map(|el| el.iter_mut()))
            {
                fix_idents_in_ir(instr, imports);
            }
        }

        IrInstruction::Loop(body, label) => {
            *label = fix_ident_for_go(label, imports);

            for body in body {
                fix_idents_in_ir(body, imports);
            }
        }

        IrInstruction::GoPackage(package_name) => {
            *package_name = fix_ident_for_go(package_name, imports);
        }

        IrInstruction::GoImports(..) => {}

        IrInstruction::GenericFun(name, generics, params, _ret, body) => {
            *name = fix_ident_for_go(name, imports);
            for param in generics {
                param.0 = fix_ident_for_go(&param.0, imports);
            }

            for param in params {
                param.0 = fix_ident_for_go(&param.0, imports);
            }

            for body in body {
                fix_idents_in_ir(body, imports);
            }
        }

        IrInstruction::FunDef(name, receiver, params, _ret, body) => {
            *name = fix_ident_for_go(name, imports);
            if let Some(receiver) = receiver.as_mut() {
                receiver.0 = fix_ident_for_go(&receiver.0, imports);
            }

            for param in params {
                param.0 = fix_ident_for_go(&param.0, imports);
            }

            for body in body {
                fix_idents_in_ir(body, imports);
            }
        }

        IrInstruction::StructDef(_name, fields) => {
            for field in fields {
                field.0 = fix_ident_for_go(&field.0, imports);
            }
        }

        IrInstruction::InterfaceDef(_name, type_params, methods) => {
            for type_param in type_params {
                type_param.0 = fix_ident_for_go(&type_param.0, imports);
            }

            for method in methods {
                method.0 = fix_ident_for_go(&method.0, imports);
                for param in &mut method.1 {
                    param.0 = fix_ident_for_go(&param.0, imports);
                }
            }
        }

        IrInstruction::SwitchType(v, cases) => {
            fix_idents_in_ir_value(v, imports);
            for case in cases {
                for branch in case
                    .conditional_branches
                    .iter_mut()
                    .flat_map(|v| v.iter_mut())
                {
                    for i in &mut branch.0.0 {
                        fix_idents_in_ir(i, imports);
                    }
                    if let Some(cond) = branch.0.1.as_mut() {
                        fix_idents_in_ir_value(cond, imports);
                    }
                    if let Some(ident) = branch.1.identifier_binding.as_mut() {
                        *ident = fix_ident_for_go(ident, imports);
                    }
                    for instr in &mut branch.1.instrs {
                        fix_idents_in_ir(instr, imports);
                    }
                }
                if let Some(ident) = case.identifier_binding.as_mut() {
                    *ident = fix_ident_for_go(ident, imports);
                }
                for instr in &mut case.instrs {
                    fix_idents_in_ir(instr, imports);
                }
            }
        }
    }
}

pub fn join_ir(v: &[IrInstruction]) -> String {
    v.iter()
        .map(IrInstruction::emit_as_go)
        .collect::<Vec<_>>()
        .join("\n")
}

impl IrValue {
    pub fn emit_as_go(&self) -> String {
        match self {
            IrValue::Negate(target) => format!("-{}", target.emit_as_go()),
            IrValue::Deref(target) => format!("*{}", target.emit_as_go()),
            IrValue::Pointer(target) => format!("&{}", target.emit_as_go()),
            IrValue::Imm(str) => str.to_string(),
            IrValue::ArrayAccess(target, idx) => {
                format!("{}[{}]", target.emit_as_go(), idx.emit_as_go())
            }
            IrValue::Array(arr_type, contents) => format!(
                "{arr_type}{{{}}}",
                contents
                    .iter()
                    .map(|x| x.emit_as_go())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IrValue::Bool(b) => format!("{b}"),
            IrValue::Int(i) => format!("{i}"),
            IrValue::Float(f) => format!("{f}"),
            IrValue::Char(c) => format!(
                "'{}'",
                c.to_string()
                    .replace("\\", "\\\\")
                    .replace("\\\\o", "\\")
                    .replace("\0", "\\x00")
                    .replace("\t", "\\t")
                    .replace("\n", "\\n")
                    .replace("\'", "\\'")
            ),
            IrValue::String(s, _is_const) => format!(
                "\"{}\"",
                s.replace("\\", "\\\\")
                    .replace("\0", "\\x00")
                    .replace("\\\\o", "\\")
                    .replace("\n", "\\n")
                    .replace("\"", "\\\"")
                    .replace("\t", "\\\t")
            ),
            IrValue::Var(v) => v.to_string(),
            IrValue::Struct(s, fields) => {
                format!(
                    // TODO: check if this should be a reference
                    "&{s}{{{}}}",
                    fields
                        .iter()
                        .map(|x| format!("{}: {}", x.0, x.1.emit_as_go()))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            IrValue::Tag(identifier) => {
                format!(
                    // TODO: check if this should be a reference
                    "{identifier}{{}}",
                )
            }
            IrValue::Duck(s, fields) => {
                format!(
                    // TODO: check if this should be a reference
                    "&{s}{{{}}}",
                    fields
                        .iter()
                        .map(|x| format!("{}: {}", x.0, x.1.emit_as_go()))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            IrValue::FieldAccess(o, field_name) => {
                format!("{}.{field_name}", o.emit_as_go())
            }
            IrValue::Tuple(go_struct, fields) => {
                format!(
                    "{go_struct}{{{}}}",
                    fields
                        .iter()
                        .map(IrValue::emit_as_go)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            IrValue::Nil => "nil".to_string(),
            IrValue::BoolNegate(o) => format!("!{}", o.emit_as_go()),
            IrValue::Lambda(params, return_type, body) => format!(
                "func({}) {} {{\n{}\n}} ",
                params
                    .iter()
                    .map(|(name, ty)| format!("{name} {ty}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                return_type.as_ref().cloned().unwrap_or_default(),
                body.iter()
                    .map(IrInstruction::emit_as_go)
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        }
    }
}
