use crate::{
    emit::value::{IrInstruction, IrValue, ToIr},
    parse::{
        Field,
        struct_parser::StructDefinition,
        type_parser::{Duck, TypeExpr},
    },
    semantics::type_resolve::TypeEnv,
};

pub fn primitive_native_type_name<'a>(primitive_type_expr: &TypeExpr) -> &'a str {
    match primitive_type_expr {
        TypeExpr::String => "string",
        TypeExpr::Int => "int",
        TypeExpr::Float => "float32",
        TypeExpr::Bool => "bool",
        TypeExpr::Char => "rune",
        TypeExpr::ConstInt(..) => "int",
        TypeExpr::ConstBool(..) => "bool",
        TypeExpr::ConstString(..) => "string",
        _ => panic!("That's not a primitive"),
    }
}

pub fn escape_string_literal(input_str: &str) -> String {
    input_str
        .chars()
        .map(|c| format!("{}_", c as u32))
        .collect()
}

pub fn primitive_conc_type_name<'a>(primitive_type_expr: &TypeExpr) -> &'a str {
    match primitive_type_expr {
        TypeExpr::String => "ConcDuckString",
        TypeExpr::Int => "ConcDuckInt",
        TypeExpr::Float => "ConcDuckFloat",
        TypeExpr::Bool => "ConcDuckBool",
        TypeExpr::Char => "ConcDuckChar",
        TypeExpr::ConstInt(int) => Box::leak(Box::new(format!("ConstInt_{int}"))),
        TypeExpr::ConstString(str) => Box::leak(Box::new(format!(
            "ConstString_{}",
            escape_string_literal(str)
        ))),
        TypeExpr::ConstBool(bool) => Box::leak(Box::new(format!("ConstBool_{bool}"))),
        _ => panic!("That's not a primitive"),
    }
}

pub fn primitive_type_name(primitive_type_expr: &TypeExpr) -> &'static str {
    match primitive_type_expr {
        TypeExpr::String => "DuckString",
        TypeExpr::Int => "DuckInt",
        TypeExpr::Float => "DuckFloat",
        TypeExpr::Bool => "DuckBool",
        TypeExpr::Char => "DuckChar",
        TypeExpr::ConstInt(int) => Box::leak(Box::new(format!("ConstInt_{int}"))),
        TypeExpr::ConstString(str) => Box::leak(Box::new(format!(
            "ConstString_{}",
            escape_string_literal(str)
        ))),
        TypeExpr::ConstBool(bool) => Box::leak(Box::new(format!("ConstBool_{bool}"))),
        _ => panic!("That's not a primitive"),
    }
}

pub fn emit_type_definitions(type_env: &mut TypeEnv, to_ir: &mut ToIr) -> Vec<IrInstruction> {
    let summary = type_env.summarize();

    fn interface_implementations(
        typename: String,
        type_expr: &TypeExpr,
        type_env: &mut TypeEnv,
        to_ir: &mut ToIr,
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
                            format!("GetPtr{}", field.name),
                            Some(("self".into(), format!("*{}", typename.clone()))),
                            vec![],
                            Some(format!(
                                "*{}",
                                field.type_expr.0.as_go_type_annotation(type_env)
                            )),
                            vec![IrInstruction::Return(Some(IrValue::Pointer(
                                IrValue::FieldAccess(
                                    IrValue::Var("self".into()).into(),
                                    field.name.clone(),
                                )
                                .into(),
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
            TypeExpr::Struct(s) => {
                if s.generics.is_some() {
                    return Vec::new();
                }

                let mut out = Vec::new();
                for field in &s.fields {
                    out.push(IrInstruction::FunDef(
                        format!("Get{}", field.name),
                        Some(("self".into(), format!("*{}", typename.clone()))),
                        vec![],
                        Some(field.type_expr.0.as_go_type_annotation(type_env)),
                        vec![IrInstruction::Return(Some(IrValue::FieldAccess(
                            IrValue::Var("self".into()).into(),
                            field.name.clone(),
                        )))],
                    ));
                    out.push(IrInstruction::FunDef(
                        format!("GetPtr{}", field.name),
                        Some(("self".into(), format!("*{}", typename.clone()))),
                        vec![],
                        Some(format!(
                            "*{}",
                            field.type_expr.0.as_go_type_annotation(type_env)
                        )),
                        vec![IrInstruction::Return(Some(IrValue::Pointer(
                            IrValue::FieldAccess(
                                IrValue::Var("self".into()).into(),
                                field.name.clone(),
                            )
                            .into(),
                        )))],
                    ));
                    out.push(IrInstruction::FunDef(
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
                    ));
                }

                for method in &s.methods {
                    if method.generics.is_some() {
                        println!("skip out {}", method.name);
                        continue;
                    }
                    println!("outing {} for {}", method.name, s.name);
                    out.push(method.emit(
                        Some(("self".to_string(), format!("*{}", s.name))),
                        type_env,
                        to_ir,
                    ));
                }

                for method in type_env.get_generic_methods(typename.clone()).clone() {
                    println!("outing instance {} for {}", method.name, s.name);
                    out.push(method.emit(
                        Some(("self".to_string(), format!("*{}", s.name))),
                        type_env,
                        to_ir,
                    ));
                }

                out
            }

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
                    (format!("GetPtr{param_name}"), vec![], Some("*T".into())),
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
        .flat_map(|primitive_type_expr| {
            if primitive_type_expr.is_literal() {
                let ir_value = IrValue::Imm(match primitive_type_expr.clone() {
                    TypeExpr::ConstString(value) => format!("\"{value}\""),
                    TypeExpr::ConstInt(int_value) => format!("{int_value}"),
                    TypeExpr::ConstBool(bool_value) => format!("{bool_value}"),
                    _ => unreachable!(),
                });

                return vec![
                    IrInstruction::StructDef(
                        primitive_type_expr.as_go_concrete_annotation(type_env),
                        vec![(
                            "value".to_string(),
                            primitive_native_type_name(primitive_type_expr).to_string(),
                        )],
                    ),
                    IrInstruction::FunDef(
                        format!("as_dgo_{}", primitive_native_type_name(primitive_type_expr)),
                        Some((
                            "self".to_string(),
                            primitive_type_expr
                                .as_go_concrete_annotation(type_env)
                                .to_string(),
                        )),
                        vec![],
                        Some(primitive_native_type_name(primitive_type_expr).to_string()),
                        vec![IrInstruction::Return(Some(ir_value))],
                    ),
                ];
            }

            vec![
                IrInstruction::InterfaceDef(
                    primitive_type_expr.as_clean_go_type_name(type_env),
                    vec![],
                    vec![(
                        format!("as_dgo_{}", primitive_native_type_name(primitive_type_expr)),
                        vec![],
                        Some(primitive_native_type_name(primitive_type_expr).to_string()),
                    )],
                ),
                IrInstruction::StructDef(
                    primitive_type_expr.as_go_concrete_annotation(type_env),
                    vec![(
                        "value".to_string(),
                        primitive_native_type_name(primitive_type_expr).to_string(),
                    )],
                ),
                IrInstruction::FunDef(
                    format!("as_dgo_{}", primitive_native_type_name(primitive_type_expr)),
                    Some((
                        "self".to_string(),
                        primitive_type_expr
                            .as_go_concrete_annotation(type_env)
                            .to_string(),
                    )),
                    vec![],
                    Some(primitive_native_type_name(primitive_type_expr).to_string()),
                    vec![IrInstruction::Return(Some(IrValue::Var(
                        "self.value".to_string(),
                    )))],
                ),
            ]
        })
        .collect::<Vec<_>>();

    instructions.append(&mut primitive_types_instructions);

    summary
        .types_used
        .iter()
        .filter(|type_expr| type_expr.is_object_like())
        .map(|type_expr| {
            let type_expr = type_env.try_resolve_type_expr(type_expr);
            let type_name = type_expr.as_clean_go_type_name(type_env);

            let mut instructions =
                interface_implementations(type_name.clone(), &type_expr, type_env, to_ir);

            instructions.push(match type_expr {
                TypeExpr::Tuple(t) => IrInstruction::StructDef(
                    type_name,
                    t.iter()
                        .enumerate()
                        .map(|(i, x)| (format!("field_{i}"), x.0.as_go_type_annotation(type_env)))
                        .collect::<Vec<_>>(),
                ),
                TypeExpr::Struct(StructDefinition { fields, .. }) => IrInstruction::StructDef(
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
                ),
                TypeExpr::Duck(Duck { fields }) => IrInstruction::StructDef(
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
                ),
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
            TypeExpr::GenericToBeReplaced(..) => {
                panic!("shouldn't access the GenericToBeReplaced as go type annotation")
            }
            TypeExpr::RawTypeName(..) => panic!(),
            TypeExpr::Array(t) => format!("[]{}", t.0.as_go_type_annotation(type_env)),
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::ConstInt(i) => primitive_type_name(&TypeExpr::ConstInt(*i)).to_string(),
            TypeExpr::ConstBool(b) => primitive_type_name(&TypeExpr::ConstBool(*b)).to_string(),
            TypeExpr::ConstString(_str) => {
                // primitive_type_name(&TypeExpr::ConstString(str.clone())).to_string()
                "DuckString".to_string()
            }
            TypeExpr::Bool => "DuckBool".to_string(),
            TypeExpr::InlineGo => "any".to_string(),
            TypeExpr::Int => "DuckInt".to_string(),
            TypeExpr::Float => "DuckFloat".to_string(),
            TypeExpr::Char => "DuckChar".to_string(),
            TypeExpr::String => "DuckString".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeNameInternal(name) => type_env
                .try_resolve_type_alias(name)
                .map(|x| x.as_go_type_annotation(type_env))
                .unwrap_or(name.clone()),
            // todo: type params
            TypeExpr::TypeName(_, name, _) => type_env
                .try_resolve_type_alias(name)
                .map(|x| x.as_go_type_annotation(type_env))
                .unwrap_or(name.clone()),
            TypeExpr::Fun(params, return_type) => format!(
                "func({}) {}",
                params
                    .iter()
                    .map(|(name, type_expr)| match name {
                        Some(name) =>
                            format!("{name} {}", type_expr.0.as_go_type_annotation(type_env)),
                        None => type_expr.0.as_go_type_annotation(type_env),
                    })
                    .collect::<Vec<_>>()
                    .join(","),
                return_type
                    .clone()
                    .filter(|x| {
                        match &x.0 {
                            TypeExpr::Tuple(fields) if fields.is_empty() => false,
                            _ => true,
                        }
                    })
                    .map_or("".to_string(), |return_type| return_type
                        .0
                        .as_go_type_annotation(type_env))
            ),
            TypeExpr::Struct(_struct) => format!("*{}", self.as_clean_go_type_name(type_env)),
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
                        .join("\n"),
                )
            }
            TypeExpr::Tuple(_fields) => self.as_clean_go_type_name(type_env),
            TypeExpr::Or(_variants) => "any".to_string(),
        };
    }

    pub fn as_go_concrete_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::GenericToBeReplaced(..) => panic!(),
            TypeExpr::RawTypeName(..) => panic!(),
            TypeExpr::ConstInt(i) => primitive_type_name(&TypeExpr::ConstInt(*i)).to_string(),
            TypeExpr::ConstBool(b) => primitive_type_name(&TypeExpr::ConstBool(*b)).to_string(),
            TypeExpr::ConstString(str) => {
                primitive_type_name(&TypeExpr::ConstString(str.clone())).to_string()
            }
            TypeExpr::Array(t) => format!("[]{}", t.0.as_go_concrete_annotation(type_env)),
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool => "ConcDuckBool".to_string(),
            TypeExpr::Int => "ConcDuckInt".to_string(),
            TypeExpr::Float => "ConcDuckFloat".to_string(),
            TypeExpr::Char => "ConcDuckChar".to_string(),
            TypeExpr::String => "ConcDuckString".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            TypeExpr::TypeNameInternal(name) => name.clone(),
            // todo: type params
            TypeExpr::TypeName(_, name, _type_params) => type_env
                .resolve_type_alias(name)
                .as_go_concrete_annotation(type_env),
            TypeExpr::Fun(params, return_type) => format!(
                "func({}) {}",
                params
                    .iter()
                    .map(|(name, type_expr)| match name {
                        Some(name) =>
                            format!("{name} {}", type_expr.0.as_go_type_annotation(type_env)),
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
            TypeExpr::Duck(Duck { fields }) => format!(
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
            TypeExpr::Struct(StructDefinition { fields, .. }) => format!(
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
            TypeExpr::Or(_) => "any".to_string(),
        };
    }

    pub fn unconst(&self) -> TypeExpr {
        match self {
            TypeExpr::ConstString(..) => TypeExpr::String,
            TypeExpr::ConstBool(..) => TypeExpr::Bool,
            TypeExpr::ConstInt(..) => TypeExpr::Int,
            _ => self.clone(),
        }
    }

    pub fn type_id(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::GenericToBeReplaced(x) => x.clone(),
            TypeExpr::RawTypeName(_, ident, _) => {
                panic!("{ident:?}")
            }
            TypeExpr::ConstInt(i) => primitive_type_name(&TypeExpr::ConstInt(*i)).to_string(),
            TypeExpr::ConstBool(b) => primitive_type_name(&TypeExpr::ConstBool(*b)).to_string(),
            TypeExpr::ConstString(str) => {
                primitive_type_name(&TypeExpr::ConstString(str.clone())).to_string()
            }
            TypeExpr::Array(t) => format!("Array_{}", t.0.as_clean_go_type_name(type_env)),
            TypeExpr::Any => "Any".to_string(),
            TypeExpr::Bool => "DuckBool".to_string(),
            TypeExpr::Int => "DuckInt".to_string(),
            TypeExpr::Float => "DuckFloat".to_string(),
            TypeExpr::Char => "DuckChar".to_string(),
            TypeExpr::String => "DuckString".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            // todo: type params
            TypeExpr::TypeName(_, name, _type_params) => name.clone(),
            TypeExpr::TypeNameInternal(name) => name.clone(),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            TypeExpr::Fun(params, return_type) => format!(
                "Fun_From_{}{}",
                params
                    .iter()
                    .map(|(name, type_expr)| format!(
                        "{}_{}",
                        name.clone().unwrap_or_else(|| "".to_string()),
                        type_expr.0.type_id(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_"),
                return_type
                    .as_ref()
                    .map(|type_expr| format!("_To_{}", type_expr.0.as_clean_go_type_name(type_env)))
                    .unwrap_or_else(|| "".to_string())
            ),
            TypeExpr::Struct(s) => s.name.clone(),
            TypeExpr::Duck(duck) => format!(
                "Duck_{}",
                duck.fields
                    .iter()
                    .map(|field| format!(
                        "{}_{}",
                        field.name,
                        field.type_expr.0.unconst().type_id(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Tuple(fields) => {
                format!(
                    "Tup_{}",
                    fields
                        .iter()
                        .map(|type_expr| type_expr.0.unconst().type_id(type_env))
                        .collect::<Vec<_>>()
                        .join("_")
                )
            }
            TypeExpr::Or(variants) => {
                // mvmo 03.07.25: Check for double sort
                let mut variants = variants
                    .clone()
                    .iter()
                    .map(|variant| variant.0.type_id(type_env))
                    .collect::<Vec<_>>();

                variants.sort();

                return format!("Union_{}", variants.join("_or_"));
            }
        };
    }

    pub fn as_clean_go_type_name(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::GenericToBeReplaced(x) => x.clone(),
            TypeExpr::RawTypeName(_, ident, _) => {
                panic!("{ident:?}")
            }
            TypeExpr::ConstInt(i) => primitive_type_name(&TypeExpr::ConstInt(*i)).to_string(),
            TypeExpr::ConstBool(b) => primitive_type_name(&TypeExpr::ConstBool(*b)).to_string(),
            TypeExpr::ConstString(str) => {
                primitive_type_name(&TypeExpr::ConstString(str.clone())).to_string()
            }
            TypeExpr::Array(t) => format!("Array_{}", t.0.as_clean_go_type_name(type_env)),
            TypeExpr::Any => "Any".to_string(),
            TypeExpr::Bool => "DuckBool".to_string(),
            TypeExpr::Int => "DuckInt".to_string(),
            TypeExpr::Float => "DuckFloat".to_string(),
            TypeExpr::Char => "DuckChar".to_string(),
            TypeExpr::String => "DuckString".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            // todo: type params
            TypeExpr::TypeName(_, name, _type_params) => type_env
                .resolve_type_alias(name)
                .as_clean_go_type_name(type_env),
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
            TypeExpr::Struct(s) => s.name.clone(),
            TypeExpr::Duck(duck) => format!(
                "Duck_{}",
                duck.fields
                    .iter()
                    .map(|field| format!(
                        "{}_{}",
                        field.name,
                        field.type_expr.0.unconst().as_clean_go_type_name(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Tuple(fields) => {
                format!(
                    "Tup_{}",
                    fields
                        .iter()
                        .map(|type_expr| type_expr
                            .0
                            .unconst()
                            .as_clean_go_type_name(type_env)
                            .to_string())
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
