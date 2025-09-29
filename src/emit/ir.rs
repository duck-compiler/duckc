use crate::emit::{
    types::{primitive_conc_type_name, primitive_native_type_name, string_to_byte_string},
    value::{Case, IrInstruction, IrValue},
};

impl IrInstruction {
    fn emit_as_go(&self) -> String {
        #![allow(clippy::format_in_format_args)]
        match self {
            IrInstruction::StringConcat(target, v) => {
                format!(
                    "{target} = {}",
                    if v.is_empty() {
                        String::from("\"\"")
                    } else {
                        v.iter()
                            .map(|x| format!("{}", x.emit_as_go()))
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
                    if case.type_name == "__else" {
                        let ir_instructions = case
                            .instrs
                            .iter()
                            .map(|ir| ir.emit_as_go())
                            .collect::<Vec<_>>()
                            .join("\n");
                        return format!("default: {{ {ir_instructions}\nbreak; }}");
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
                        if_body.push(IrInstruction::Break);

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
                    .filter(|case| case.type_name != "__else")
                    .map(|case| emit_case_go(case, &against.emit_as_go(), else_case))
                    .collect();

                format!(
                    "switch {}.(type) {{\n{}\n}}",
                    against.emit_as_go(),
                    processed_cases.join("\n"),
                )
            }
            IrInstruction::GoPackage(s) => format!("package {s}"),
            IrInstruction::Add(r, left, right, type_expr) => {
                // TODO: check if this is correct
                format!(
                    "{r} = {} {{ value: {}.as_dgo_{}() + {}.as_dgo_{}() }}",
                    primitive_conc_type_name(type_expr),
                    left.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    right.emit_as_go(),
                    primitive_native_type_name(type_expr),
                )
            }
            IrInstruction::Mul(r, v1, v2, type_expr) => {
                // TODO: check if this is correct
                format!(
                    "{r} = {} {{ {}.as_dgo_{}() * {}.as_dgo_{}() }}",
                    primitive_conc_type_name(type_expr),
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr),
                )
            }
            IrInstruction::Sub(r, v1, v2, type_expr) => {
                format!(
                    "{r} = {} {{ {}.as_dgo_{}() - {}.as_dgo_{}() }}",
                    primitive_conc_type_name(type_expr),
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr),
                )
            }
            IrInstruction::Div(r, v1, v2, type_expr) => {
                format!(
                    "{r} = {} {{ {}.as_dgo_{}() / {}.as_dgo_{}() }}",
                    primitive_conc_type_name(type_expr),
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr),
                )
            }
            IrInstruction::Mod(r, v1, v2, type_expr) => {
                format!(
                    "{r} = {} {{ {}.as_dgo_{}() % {}.as_dgo_{}() }}",
                    primitive_conc_type_name(type_expr),
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr),
                )
            }
            IrInstruction::Continue => "continue".to_string(),
            IrInstruction::Break => "break".to_string(),
            IrInstruction::Return(o) => format!(
                "return {}",
                o.as_ref()
                    .map(IrValue::emit_as_go)
                    .unwrap_or("".to_string())
            ),
            IrInstruction::Equals(r, v1, v2, type_expr) => {
                format!(
                    "{r} = ConcDuckBool {{ value: {}.as_dgo_{}() == {}.as_dgo_{}() }}",
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr)
                )
            }
            IrInstruction::NotEquals(r, v1, v2, type_expr) => {
                format!(
                    "{r} = ConcDuckBool {{ value: {}.as_dgo_{}() != {}.as_dgo_{}() }}",
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr)
                )
            }
            IrInstruction::LessThan(r, v1, v2, type_expr) => {
                format!(
                    "{r} = ConcDuckBool {{ value: {}.as_dgo_{}() < {}.as_dgo_{}() }}",
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr)
                )
            }
            IrInstruction::LessThanOrEquals(r, v1, v2, type_expr) => {
                format!(
                    "{r} = ConcDuckBool {{ value: {}.as_dgo_{}() <= {}.as_dgo_{}() }}",
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr)
                )
            }
            IrInstruction::GreaterThan(r, v1, v2, type_expr) => {
                format!(
                    "{r} = ConcDuckBool {{ value: {}.as_dgo_{}() > {}.as_dgo_{}() }}",
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr)
                )
            }
            IrInstruction::GreaterThanOrEquals(r, v1, v2, type_expr) => {
                format!(
                    "{r} = ConcDuckBool {{ value: {}.as_dgo_{}() >= {}.as_dgo_{}() }}",
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr)
                )
            }
            IrInstruction::And(r, v1, v2, type_expr) => {
                format!(
                    "{r} = ConcDuckBool {{ value: {}.as_dgo_{}() && {}.as_dgo_{}() }}",
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr)
                )
            }
            IrInstruction::Or(r, v1, v2, type_expr) => {
                format!(
                    "{r} = ConcDuckBool {{ value: {}.as_dgo_{}() || {}.as_dgo_{}() }}",
                    v1.emit_as_go(),
                    primitive_native_type_name(type_expr),
                    v2.emit_as_go(),
                    primitive_native_type_name(type_expr)
                )
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
            IrInstruction::VarDecl(name, ty) => format!("var {name} {ty}\n_ = {name}"),
            IrInstruction::VarAssignment(name, v) => format!("{name} = {}", v.emit_as_go()),
            IrInstruction::If(cond, then, els) => {
                format!(
                    "if ({}).as_dgo_bool() {{\n{}\n}} {}",
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
            IrInstruction::Loop(v) => {
                format!("for {{\n{}\n}}", join_ir(v))
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
                        .filter(|x| x != "Tup_")
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

pub fn join_ir(v: &[IrInstruction]) -> String {
    v.iter()
        .map(IrInstruction::emit_as_go)
        .collect::<Vec<_>>()
        .join("\n")
}

impl IrValue {
    pub fn emit_as_go(&self) -> String {
        match self {
            IrValue::Deref(target) => format!("*{}", target.emit_as_go()),
            IrValue::Pointer(target) => format!("&{}", target.emit_as_go()),
            IrValue::Imm(str) => str.to_string(),
            IrValue::ArrayAccess(target, idx) => {
                format!("{}[{}.as_dgo_int()]", target.emit_as_go(), idx.emit_as_go())
            }
            IrValue::Array(arr_type, contents) => format!(
                "{arr_type}{{{}}}",
                contents
                    .iter()
                    .map(|x| x.emit_as_go())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IrValue::Bool(b) => format!("ConcDuckBool {{ value: {b} }}"),
            IrValue::Int(i) => format!("ConcDuckInt {{ value: {i} }}"),
            IrValue::Float(f) => format!("ConcDuckFloat {{ value: {f} }}"),
            IrValue::Char(c) => format!("ConcDuckChar {{ value: '{c}' }}"),
            IrValue::String(s, _is_const) => format!("\"{}\"", s.replace("\n", "\\n")),
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
            IrValue::MethodCall(o, method_name, params) => {
                format!(
                    "{}.{method_name}({})",
                    o.emit_as_go(),
                    params
                        .iter()
                        .map(|x| x.emit_as_go())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
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
            IrValue::BoolNegate(o) => format!(
                "ConcDuckBool {{ value: !{}.as_dgo_bool() }}",
                o.emit_as_go()
            ),
            IrValue::Lambda(params, return_type, body) => format!(
                "func({}) {} {{\n{}\n}} ",
                params
                    .iter()
                    .map(|(name, ty)| format!("{name} {ty}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                return_type
                    .as_ref()
                    .filter(|x| x.as_str() != "Tup_")
                    .cloned()
                    .unwrap_or_default(),
                body.iter()
                    .map(IrInstruction::emit_as_go)
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        }
    }
}
