use crate::{
    emit::value::{IrInstruction, IrValue},
    parse::type_parser::{Duck, Field, Struct, TypeExpr},
    semantics::typechecker::TypeEnv,
};

pub fn emit_type_definitions(type_env: &mut TypeEnv) -> Vec<IrInstruction> {
    let summary = type_env.summarize();

    fn interface_implementations(
        typename: String,
        type_expr: &TypeExpr,
        type_env: &mut TypeEnv,
    ) -> Vec<IrInstruction> {
        return match type_expr {
            TypeExpr::Duck(duck) => duck
                .fields
                .iter()
                .map(|field| {
                    IrInstruction::FunDef(
                        format!("Get{}", field.name),
                        Some(("self".into(), typename.clone())),
                        vec![],
                        Some(field.type_expr.0.as_go_type_annotation(type_env)),
                        vec![IrInstruction::Return(Some(IrValue::FieldAccess(
                            IrValue::Var("self".into()).into(),
                            field.name.clone(),
                        )))],
                    )
                })
                .collect::<Vec<_>>(),
            TypeExpr::Struct(r#struct) => r#struct
                .fields
                .iter()
                .map(|field| {
                    IrInstruction::FunDef(
                        format!("Get{}", field.name),
                        Some(("self".into(), typename.clone())),
                        vec![],
                        Some(field.type_expr.0.as_go_type_annotation(type_env)),
                        vec![IrInstruction::Return(Some(IrValue::FieldAccess(
                            IrValue::Var("self".into()).into(),
                            field.name.clone(),
                        )))],
                    )
                })
                .collect::<Vec<_>>(),
            _ => vec![],
        };
    }

    let interface_defs = summary
        .param_names_used
        .iter()
        .map(|param_name| {
            IrInstruction::InterfaceDef(
                format!("Has{param_name}"),
                vec![("T".into(), "any".into())],
                vec![(format!("Get{param_name}"), "T".into())],
            )
        })
        .collect::<Vec<_>>();

    summary
        .types_used
        .iter()
        .filter(|type_expr| type_expr.is_object_like())
        .map(|type_expr| {
            let type_name = type_expr.as_clean_go_type_name(type_env);

            let mut i = interface_implementations(type_name.clone(), type_expr, type_env);

            i.push(match type_expr {
                TypeExpr::Tuple(t) => IrInstruction::StructDef(
                    type_name,
                    t.iter()
                        .enumerate()
                        .map(|(i, x)| (format!("field_{i}"), x.0.as_go_type_annotation(type_env)))
                        .collect::<Vec<_>>(),
                ),
                TypeExpr::Struct(Struct { fields }) | TypeExpr::Duck(Duck { fields }) => {
                    IrInstruction::StructDef(
                        type_name,
                        fields
                            .iter()
                            .map(
                                |Field {
                                     name,
                                     type_expr: (type_expr, _),
                                 }| {
                                    (name.clone(), type_expr.as_go_type_annotation(type_env))
                                },
                            )
                            .collect::<Vec<_>>(),
                    )
                }
                _ => panic!("cant create for {type_name}"),
            });
            i
        })
        .chain(vec![interface_defs])
        .fold(Vec::new(), |mut acc, x| {
            for y in x {
                if !acc.contains(&y) {
                    acc.push(y);
                }
            }
            acc
        })
}

impl TypeExpr {
    pub fn as_go_type_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::InlineGo => "any".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeNameInternal(name) => name.clone(),
            TypeExpr::TypeName(_, name) => type_env
                .resolve_type_alias(name)
                .as_go_type_annotation(type_env),
            TypeExpr::Fun(params, return_type) => format!(
                "func({}) {}",
                params
                    .iter()
                    .map(|(name, type_expr)| match name {
                        Some(name) =>
                            format!("{name}: {}", type_expr.0.as_go_type_annotation(type_env)),
                        None => type_expr.0.as_go_type_annotation(type_env),
                    })
                    .collect::<Vec<_>>()
                    .join(","),
                return_type
                    .clone()
                    .map_or("".to_string(), |return_type| return_type
                        .0
                        .as_go_type_annotation(type_env))
            ),
            TypeExpr::Struct(_struct) => self.as_clean_go_type_name(type_env),
            TypeExpr::Duck(duck) => {
                let mut fields = duck.fields.clone();
                fields.sort_by_key(|field| field.name.clone());

                format!(
                    "interface {{\n{}\n}}",
                    fields
                        .iter()
                        .map(|field| format!(
                            "   Has{}[{}]",
                            field.name,
                            field.type_expr.0.as_go_type_annotation(type_env)
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            TypeExpr::Tuple(_fields) => self.as_clean_go_type_name(type_env),
            TypeExpr::Or(_variants) => todo!("implement variants"),
        };
    }

    pub fn as_go_concrete_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            TypeExpr::TypeNameInternal(name) => name.clone(),
            TypeExpr::TypeName(_, name) => type_env
                .resolve_type_alias(name)
                .as_go_concrete_annotation(type_env),
            TypeExpr::Fun(params, return_type) => format!(
                "func({}) {}",
                params
                    .iter()
                    .map(|(name, type_expr)| match name {
                        Some(name) =>
                            format!("{name}: {}", type_expr.0.as_go_type_annotation(type_env)),
                        None => type_expr.0.as_go_type_annotation(type_env),
                    })
                    .collect::<Vec<_>>()
                    .join(","),
                return_type
                    .clone()
                    .map_or("".to_string(), |return_type| return_type
                        .0
                        .as_go_type_annotation(type_env))
            ),
            TypeExpr::Duck(Duck { fields }) | TypeExpr::Struct(Struct { fields }) => format!(
                "struct {{\n{}\n}}",
                fields
                    .iter()
                    .map(|field| format!(
                        "   {} {}",
                        field.name,
                        field.type_expr.0.as_go_type_annotation(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            TypeExpr::Tuple(fields) => {
                format!(
                    "struct {{\n{}\n}}",
                    fields
                        .iter()
                        .enumerate()
                        .map(|(i, type_expr)| format!(
                            "field_{i} {}",
                            type_expr.0.as_go_type_annotation(type_env)
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            TypeExpr::Or(_variants) => todo!("implement variants"),
        };
    }

    pub fn as_clean_go_type_name(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Any => "Any".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeName(_, name) => name.clone(),
            TypeExpr::TypeNameInternal(name) => name.clone(),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            TypeExpr::Fun(params, return_type) => format!(
                "Fun_From_{}{}",
                params
                    .iter()
                    .map(|(name, type_expr)| format!(
                        "{}_{}",
                        name.clone().unwrap_or_else(|| "".to_string()),
                        type_expr.0.as_clean_go_type_name(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_"),
                return_type
                    .as_ref()
                    .map(|type_expr| format!("_To_{}", type_expr.0.as_clean_go_type_name(type_env)))
                    .unwrap_or_else(|| "".to_string())
            ),
            TypeExpr::Struct(r#struct) if r#struct.fields.is_empty() => "Any".to_string(),
            TypeExpr::Struct(r#struct) => format!(
                "Struct_{}",
                r#struct
                    .fields
                    .iter()
                    .map(|field| format!(
                        "{}_{}",
                        field.name,
                        field.type_expr.0.as_clean_go_type_name(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Duck(duck) => format!(
                "Duck_{}",
                duck.fields
                    .iter()
                    .map(|field| format!(
                        "{}_{}",
                        field.name,
                        field.type_expr.0.as_clean_go_type_name(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Tuple(fields) => {
                format!(
                    "Tup_{}",
                    fields
                        .iter()
                        .map(|type_expr| type_expr.0.as_clean_go_type_name(type_env).to_string())
                        .collect::<Vec<_>>()
                        .join("_")
                )
            }
            TypeExpr::Or(_variants) => todo!("implement variants"),
        };
    }
}
