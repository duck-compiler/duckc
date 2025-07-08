use crate::emit::{
    types::{primitive_conc_type_name, primitive_native_type_name},
    value::{Case, IrInstruction, IrValue},
};

impl IrInstruction {
    fn emit_as_go(&self) -> String {
        #![allow(clippy::format_in_format_args)]
        match self {
            IrInstruction::StringConcat(target, v) => {
                format!(
                    "{target} = ConcDuckString {{ value: {} }}",
                    if v.is_empty() {
                        String::from("\"\"")
                    } else {
                        v.iter()
                            .map(|x| format!("{}.as_dgo_string()", x.emit_as_go()))
                            .collect::<Vec<_>>()
                            .join(" + ")
                    }
                )
            }
            IrInstruction::SwitchType(against, type_cases) => {
                // TODO: should this be mangled???? LOLOLOLO I DON"T THINK SO
                fn emit_case_go(case: &Case, actual: &str) -> String {
                    format!(
                        "case {}:\n{}\n{}",
                        case.type_name,
                        format!(
                            "var {} {} = {}.({})\n_={}\n",
                            case.bound_to_identifier,
                            case.type_name,
                            actual,
                            case.type_name,
                            case.bound_to_identifier
                        ),
                        case.instrs
                            .iter()
                            .map(IrInstruction::emit_as_go)
                            .collect::<Vec<_>>()
                            .join("\n\t"),
                    )
                }

                format!(
                    "switch {}.(type) {{\n{}\n}}",
                    against.emit_as_go(),
                    type_cases
                        .iter()
                        .map(|case| emit_case_go(case, &against.emit_as_go()))
                        .collect::<Vec<_>>()
                        .join("\n"),
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
            IrInstruction::Continue => "continue".to_string(),
            IrInstruction::Break => "break".to_string(),
            IrInstruction::Return(o) => format!(
                "return {}",
                o.as_ref()
                    .map(IrValue::emit_as_go)
                    .unwrap_or("".to_string())
            ),
            IrInstruction::Equals(r, v1, v2) => {
                format!("{r} = {} == {}", v1.emit_as_go(), v2.emit_as_go())
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
                    "if {} {{\n{}\n}} {}",
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
            IrValue::String(s) => format!("ConcDuckString {{ \"{s}\" }}"),
            IrValue::Var(v) => v.to_string(),
            IrValue::Duck(s, fields) | IrValue::Struct(s, fields) => {
                format!(
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
            IrValue::BoolNegate(o) => format!("!{}", o.emit_as_go()),
            IrValue::Lambda(params, return_type, body) => format!(
                "func({}) {} {{\n{}\n}} ",
                params
                    .iter()
                    .map(|(name, ty)| format!("{name} {ty}"))
                    .collect::<Vec<_>>()
                    .join(" "),
                return_type.clone().unwrap_or_default(),
                body.iter()
                    .map(IrInstruction::emit_as_go)
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        }
    }
}
