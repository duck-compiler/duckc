use crate::emit::value::{Case, IrInstruction, IrValue};

impl IrInstruction {
    fn emit_as_go(&self) -> String {
        #![allow(clippy::format_in_format_args)]
        match self {
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
                    .filter(|case| case.type_name != "__else")
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
            IrInstruction::VarDecl(name, ty) => format!("var {name} {ty}\n_ = {name}"),
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
                format!("if false {{goto {label}}}\n{label}:\nfor {{\n{}\n}}", join_ir(v))
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
            IrValue::Char(c) => format!("'{c}'"),
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
            IrValue::BoolNegate(o) => format!("!{}", o.emit_as_go()),
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
