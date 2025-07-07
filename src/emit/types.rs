use crate::{
    emit::value::{IrInstruction, IrValue},
    parse::type_parser::{Duck, Field, Struct, TypeExpr},
    semantics::typechecker::TypeEnv,
};

pub fn primitive_native_type_name<'a>(primitive_type_expr: &TypeExpr) -> &'a str {
    match primitive_type_expr {
        TypeExpr::String => "string",
        TypeExpr::Int => "int",
        TypeExpr::Float => "float32",
        TypeExpr::Bool => "bool",
        TypeExpr::Char => "rune",
        _ => panic!("That's not a primitive"),
    }
}

pub fn primitive_type_name<'a>(primitive_type_expr: &TypeExpr) -> &'a str {
    match primitive_type_expr {
        TypeExpr::String => "DuckString",
        TypeExpr::Int => "DuckInt",
        TypeExpr::Float => "DuckFloat",
        TypeExpr::Bool => "DuckBool",
        TypeExpr::Char => "Char",
        _ => panic!("That's not a primitive"),
    }
}

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
                .flat_map(|field| {
                    vec![
                        IrInstruction::FunDef(
                            format!("Get{}", field.name),
                            Some(("self".into(), format!("*{}", typename.clone()))),
                            vec![],
                            Some(field.type_expr.0.as_go_type_annotation(type_env)),
                            vec![IrInstruction::Return(Some(IrValue::FieldAccess(
                                IrValue::Var("self".into()).into(),
                                field.name.clone(),
                            )))],
                        ),
                        IrInstruction::FunDef(
                            format!("Set{}", field.name),
                            Some(("self".into(), format!("*{}", typename.clone()))),
                            vec![(
                                "param".into(),
                                field.type_expr.0.as_go_type_annotation(type_env),
                            )],
                            None,
                            vec![IrInstruction::VarAssignment(
                                format!("self.{}", field.name),
                                IrValue::Var("param".into()),
                            )],
                        ),
                    ]
                    .into_iter()
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
            _ => {
                vec![]
            }
        };
    }

    let mut instructions = summary
        .param_names_used
        .iter()
        .map(|param_name| {
            IrInstruction::InterfaceDef(
                format!("Has{param_name}"),
                vec![("T".into(), "any".into())],
                vec![
                    (format!("Get{param_name}"), vec![], Some("T".into())),
                    (
                        format!("Set{param_name}"),
                        vec![("param".into(), "T".into())],
                        None,
                    ),
                ],
            )
        })
        .collect::<Vec<_>>();

    let mut primitive_types_instructions = summary
        .types_used
        .iter()
        .filter(|type_expr| type_expr.is_primitive())
        .map(|primitive_type_expr| {
            IrInstruction::StructDef(
                primitive_type_expr.as_clean_go_type_name(type_env),
                vec![(
                    "value".to_string(),
                    primitive_native_type_name(primitive_type_expr).to_string(),
                )],
            )
        })
        .collect::<Vec<_>>();

    instructions.append(&mut primitive_types_instructions);

    let mut variant_instructions = summary
        .types_used
        .iter()
        .filter(|type_expr| matches!(type_expr, TypeExpr::Or(..)))
        .flat_map(|variant_type_expr| {
            let TypeExpr::Or(variants) = variant_type_expr else {
                unreachable!()
            };
            let variant_type_name = variant_type_expr.as_clean_go_type_name(type_env);
            let variant_seal_fn_name = format!("Seal_{}", variant_type_name);

            let mut sealing_fn_instructions = variants
                .iter()
                .map(|variant| {
                    IrInstruction::FunDef(
                        variant_seal_fn_name.clone(),
                        Some((
                            "self".to_string(),
                            variant.0.as_clean_go_type_name(type_env),
                        )),
                        vec![],
                        None,
                        vec![],
                    )
                })
                .collect::<Vec<_>>();

            sealing_fn_instructions.push(IrInstruction::InterfaceDef(
                variant_type_name,
                vec![],
                vec![(variant_seal_fn_name.to_string(), vec![], None)],
            ));

            sealing_fn_instructions
        })
        .collect::<Vec<_>>();

    instructions.append(&mut variant_instructions);

    summary
        .types_used
        .iter()
        .filter(|type_expr| type_expr.is_object_like())
        .map(|type_expr| {
            let type_name = type_expr.as_clean_go_type_name(type_env);

            let mut instructions =
                interface_implementations(type_name.clone(), type_expr, type_env);

            instructions.push(match type_expr {
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
            instructions
        })
        .chain(vec![instructions])
        .fold(Vec::new(), |mut instructions_acc, instructions| {
            for instruction in instructions {
                if !instructions_acc.contains(&instruction) {
                    instructions_acc.push(instruction);
                }
            }
            instructions_acc
        })
}

impl TypeExpr {
    pub fn as_go_type_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Array(t) => format!("[]{}", t.0.as_go_type_annotation(type_env)),
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool => "DuckBool".to_string(),
            TypeExpr::InlineGo => "any".to_string(),
            TypeExpr::Int => "DuckInt".to_string(),
            TypeExpr::Float => "DuckFloat".to_string(),
            TypeExpr::Char => "DuckChar".to_string(),
            TypeExpr::String => "DuckString".to_string(),
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
            TypeExpr::Or(_variants) => self.as_clean_go_type_name(type_env),
            TypeExpr::IntLiteral(..) | TypeExpr::BoolLiteral(..) | TypeExpr::StringLiteral(..) => todo!()
        };
    }

    pub fn as_go_concrete_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::IntLiteral(..) | TypeExpr::BoolLiteral(..) | TypeExpr::StringLiteral(..) => todo!(),
            TypeExpr::Array(t) => format!("[]{}", t.0.as_go_concrete_annotation(type_env)),
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool => "DuckBool".to_string(),
            TypeExpr::Int => "DuckInt".to_string(),
            TypeExpr::Float => "DuckFloat".to_string(),
            TypeExpr::Char => "Char".to_string(),
            TypeExpr::String => "String".to_string(),
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
            TypeExpr::Or(variants) => {
                format!(
                    "struct {{\n{}\n}}",
                    variants
                        .iter()
                        .map(|type_expr| type_expr.0.as_go_type_annotation(type_env).to_string())
                        .collect::<Vec<_>>()
                        .join("_")
                )
            }
        };
    }

    pub fn as_clean_go_type_name(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::IntLiteral(..) | TypeExpr::BoolLiteral(..) | TypeExpr::StringLiteral(..) => todo!(),
            TypeExpr::Array(t) => format!("Array_{}", t.0.as_clean_go_type_name(type_env)),
            TypeExpr::Any => "Any".to_string(),
            TypeExpr::Bool => "DuckBool".to_string(),
            TypeExpr::Int => "DuckInt".to_string(),
            TypeExpr::Float => "DuckFloat".to_string(),
            TypeExpr::Char => "DuckChar".to_string(),
            TypeExpr::String => "DuckString".to_string(),
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
            TypeExpr::Or(variants) => {
                // mvmo 03.07.25: Check for double sort
                let mut variants = variants
                    .clone()
                    .iter()
                    .map(|variant| variant.0.as_clean_go_type_name(type_env))
                    .collect::<Vec<_>>();

                variants.sort();

                return format!("Union_{}", variants.join("_or_"));
            }
        };
    }
}
