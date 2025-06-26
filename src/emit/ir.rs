use crate::emit::value::{IrInstruction, IrValue};

impl IrInstruction {
    fn emit_as_go(&self) -> String {
        match self {
            IrInstruction::GoPaackage(s) => format!("package {s}"),
            IrInstruction::Add(r, v1, v2) => {
                format!("{r} = {} + {}", v1.emit_as_go(), v2.emit_as_go())
            }
            IrInstruction::Mul(r, v1, v2) => {
                format!("{r} = {} * {}", v1.emit_as_go(), v2.emit_as_go())
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
            IrInstruction::FunCall(r, t, p) => {
                format!(
                    "{r} = {t}({})",
                    p.iter()
                        .map(IrValue::emit_as_go)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            IrInstruction::VarDecl(name, ty) => format!("var {name} {ty}\n_ = name"),
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
                                "\nelse {{\n{}\n}} ",
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
                format!("for {{\n{}\n}}", join_instr(v))
            }
            IrInstruction::InlineGo(t) => {
                format!("{t}")
            }
            IrInstruction::GoImports(imports) => {
                format!(
                    "import (\n{}\n)",
                    imports
                        .iter()
                        .map(|(n, m)| format!(
                            "{} \"{m}\"",
                            n.as_ref().map(|x| x.clone()).unwrap_or_default()
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            IrInstruction::FunDef(name, params, return_type, body) => {
                format!(
                    "func {name}({}) {} {{\n{}\n}}",
                    params
                        .iter()
                        .map(|(n, ty)| format!("{} {}", n, ty))
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type.as_ref().unwrap_or(&String::new()),
                    join_instr(body),
                )
            }
            IrInstruction::StructDef(name, fields) => {
                format!(
                    "type {name} struct {{\n{}\n}}",
                    fields
                        .iter()
                        .map(|(n, ty)| format!("{} {}", n, ty))
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
            IrInstruction::InterfaceDef(name, fields) => {
                format!(
                    "type {name} interface {{\n{}\n}}",
                    fields
                        .iter()
                        .map(|(n, ty)| format!("{} {}", n, ty))
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
        }
    }
}

pub fn join_instr(v: &Vec<IrInstruction>) -> String {
    v.iter()
        .map(IrInstruction::emit_as_go)
        .collect::<Vec<_>>()
        .join("")
}

impl IrValue {
    pub fn emit_as_go(&self) -> String {
        match self {
            IrValue::Bool(b) => b.to_string(),
            IrValue::Int(i) => i.to_string(),
            IrValue::Float(f) => f.to_string(),
            IrValue::Char(c) => format!("'{c}'"),
            IrValue::String(s) => format!("\"{s}\""),
            IrValue::Var(v) => format!("{v}"),
            IrValue::Duck(s, fields) | IrValue::Struct(s, fields) => {
                format!(
                    "{s}{{{}}}",
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
                return_type.as_ref().map(|x| x.clone()).unwrap_or_default(),
                body.iter()
                    .map(IrInstruction::emit_as_go)
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        }
    }
}
