use std::collections::HashSet;

use crate::{
    emit::value::{IrInstruction, IrValue, ToIr},
    parse::{
        Field,
        struct_parser::StructDefinition,
        type_parser::{Duck, TypeExpr},
        value_parser::empty_range,
    },
    semantics::{
        ident_mangler::MANGLE_SEP,
        type_resolve::{NeedsSearchResult, TypeEnv},
    },
};

pub fn primitive_native_type_name<'a>(primitive_type_expr: &TypeExpr) -> &'a str {
    match primitive_type_expr {
        TypeExpr::Float => "float32",
        TypeExpr::Char => "rune",
        TypeExpr::Int(..) => "int",
        TypeExpr::Bool(..) => "bool",
        TypeExpr::String(..) => "string",
        _ => panic!("That's not a primitive"),
    }
}

pub fn escape_string_for_go(input_str: &str) -> String {
    let mut out = String::new();
    for c in input_str.chars() {
        match c {
            '\\' | '"' => out.push('\\'),
            '\n' => {
                out.push_str("\\n");
                continue;
            }
            _ => {}
        }
        out.push(c);
    }
    out
}

pub fn string_to_byte_string(input_str: &str) -> String {
    input_str
        .chars()
        .map(|c| format!("{}_", c as u32))
        .collect()
}

pub fn primitive_conc_type_name<'a>(primitive_type_expr: &TypeExpr) -> &'a str {
    match primitive_type_expr {
        TypeExpr::String(..) => "string",
        TypeExpr::Int(..) => "int",
        TypeExpr::Float => "float32",
        TypeExpr::Bool(..) => "bool",
        TypeExpr::Char => "rune",
        _ => panic!("That's not a primitive"),
    }
}

pub fn primitive_type_name(primitive_type_expr: &TypeExpr) -> &'static str {
    match primitive_type_expr {
        TypeExpr::String(..) => "string",
        TypeExpr::Int(..) => "int",
        TypeExpr::Float => "float32",
        TypeExpr::Bool(..) => "bool",
        TypeExpr::Char => "rune",
        _ => panic!("That's not a primitive"),
    }
}

pub fn fixup_method_body(struct_name: &str, body: &mut Vec<IrInstruction>, insert_org_addr: bool) {
    let mut to_insert = Vec::new();

    if insert_org_addr {
        to_insert.push(IrInstruction::VarDecl(
            "Δorg_addr".to_string(),
            format!("*{struct_name}"),
        ));
        to_insert.push(IrInstruction::VarAssignment(
            "Δorg_addr".to_string(),
            IrValue::Imm("duck_internal_self".to_string()),
        ));
    }

    to_insert.push(IrInstruction::VarDecl(
        "self".to_string(),
        format!("**{struct_name}"),
    ));
    to_insert.push(IrInstruction::VarAssignment(
        "self".to_string(),
        IrValue::Imm("&duck_internal_self".to_string()),
    ));

    if insert_org_addr {
        to_insert.push(IrInstruction::InlineGo(
            "defer func() { *Δorg_addr = **self }()".to_string(),
        ));
    }

    to_insert
        .into_iter()
        .rev()
        .for_each(|elem| body.insert(0, elem));
}

pub fn emit_type_definitions(type_env: &mut TypeEnv, to_ir: &mut ToIr) -> Vec<IrInstruction> {
    let mut result = Vec::new();
    let mut emitted_types = HashSet::new();

    let all_tuples_and_ducks = type_env.find_ducks_and_tuples();

    for tuple_or_duck in &all_tuples_and_ducks {
        match tuple_or_duck {
            NeedsSearchResult::Duck { fields } => {
                let type_name = TypeExpr::Duck(Duck {
                    fields: fields.clone(),
                })
                .as_clean_go_type_name(type_env);
                result.push(IrInstruction::StructDef(
                    type_name.clone(),
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
                ));

                for field in fields.iter() {
                    let param_name = &field.name;
                    let interface_name = format!("Has{param_name}");
                    if emitted_types.insert(interface_name.clone()) {
                        result.push(IrInstruction::InterfaceDef(
                            interface_name,
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
                        ));
                    }
                    result.extend([
                        IrInstruction::FunDef(
                            format!("Get{}", field.name),
                            Some(("self".into(), format!("*{}", type_name.clone()))),
                            vec![],
                            Some(field.type_expr.0.as_go_type_annotation(type_env)),
                            vec![IrInstruction::Return(Some(IrValue::FieldAccess(
                                IrValue::Var("self".into()).into(),
                                field.name.clone(),
                            )))],
                        ),
                        IrInstruction::FunDef(
                            format!("GetPtr{}", field.name),
                            Some(("self".into(), format!("*{}", type_name.clone()))),
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
                            Some(("self".into(), format!("*{}", type_name.clone()))),
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
                    ]);
                }
            }
            NeedsSearchResult::Tag { name } => {
                result.push(IrInstruction::StructDef(
                    TypeExpr::Tag(name.clone()).as_clean_go_type_name(type_env),
                    vec![],
                ));
            }
            NeedsSearchResult::Tuple { fields } => {
                let type_name = TypeExpr::Tuple(fields.clone()).as_clean_go_type_name(type_env);
                result.push(IrInstruction::StructDef(
                    type_name,
                    fields
                        .iter()
                        .enumerate()
                        .map(|(i, x)| (format!("field_{i}"), x.0.as_go_type_annotation(type_env)))
                        .collect::<Vec<_>>(),
                ));
            }
        }
    }

    for s in type_env
        .struct_definitions
        .clone()
        .iter_mut()
        .chain(type_env.generic_structs_generated.clone().iter_mut())
        .filter(|s| s.generics.is_empty())
    {
        result.push(IrInstruction::StructDef(
            s.name.clone(),
            s.fields
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
        ));

        let StructDefinition {
            name: struct_name,
            fields,
            methods,
            mut_methods: _,
            generics: _,
            doc_comments: _,
        } = s;

        let mut instructions: Vec<IrInstruction> = fields
            .iter()
            .flat_map(|field| {
                vec![
                    IrInstruction::FunDef(
                        format!("Get{}", field.name),
                        Some(("self".into(), format!("*{}", struct_name.clone()))),
                        vec![],
                        Some(field.type_expr.0.as_go_type_annotation(type_env)),
                        vec![IrInstruction::Return(Some(IrValue::FieldAccess(
                            IrValue::Var("self".into()).into(),
                            field.name.clone(),
                        )))],
                    ),
                    IrInstruction::FunDef(
                        format!("GetPtr{}", field.name),
                        Some(("self".into(), format!("*{}", struct_name.clone()))),
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
                        Some(("self".into(), format!("*{}", struct_name.clone()))),
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
            .collect();

        for method in methods.iter() {
            if method.generics.is_some() {
                continue;
            }

            let func_type_str = format!(
                "func ({}) {}",
                method
                    .params
                    .iter()
                    .flat_map(|params| params.iter())
                    .map(|param| format!(
                        "{} {}",
                        param.0,
                        param.1.0.as_go_type_annotation(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join(","),
                method
                    .return_type
                    .as_ref()
                    .filter(|x| !x.0.is_unit())
                    .map(|(type_expr, _)| type_expr.as_go_type_annotation(type_env))
                    .unwrap_or_default()
            );

            let instructions_to_be_duck_conform = vec![
                IrInstruction::FunDef(
                    format!("Get{}", method.name),
                    Some(("self".into(), format!("*{}", struct_name.clone()))),
                    vec![],
                    Some(method.type_expr().0.as_go_return_type(type_env)),
                    vec![
                        IrInstruction::VarDecl("result".to_string(), func_type_str.clone()),
                        IrInstruction::VarAssignment(
                            "result".to_string(),
                            IrValue::Lambda(
                                method
                                    .params
                                    .iter()
                                    .flat_map(|params| params.iter())
                                    .map(|param| {
                                        (param.0.clone(), param.1.0.as_go_type_annotation(type_env))
                                    })
                                    .collect::<Vec<_>>(),
                                method.return_type.as_ref().map(|return_type| {
                                    return_type.0.as_go_type_annotation(type_env)
                                }),
                                vec![IrInstruction::InlineGo(format!(
                                    "{} self.{}({})",
                                    if method.return_type.is_some() {
                                        "return"
                                    } else {
                                        ""
                                    },
                                    method.name,
                                    method
                                        .params
                                        .iter()
                                        .flat_map(|params| params.iter())
                                        .map(|param| param.0.clone())
                                        .collect::<Vec<_>>()
                                        .join(", "),
                                ))],
                            ),
                        ),
                        IrInstruction::Return(Some(IrValue::Var("result".to_string()))),
                    ],
                ),
                IrInstruction::FunDef(
                    format!("GetPtr{}", method.name),
                    Some(("self".into(), format!("*{}", struct_name.clone()))),
                    vec![],
                    Some(format!(
                        "*{}",
                        method.type_expr().0.as_go_return_type(type_env)
                    )),
                    vec![IrInstruction::Return(Some(IrValue::Nil))],
                ),
                IrInstruction::FunDef(
                    format!("Set{}", method.name),
                    Some(("self".into(), format!("*{}", struct_name.clone()))),
                    vec![("param".into(), func_type_str)],
                    None,
                    vec![],
                ),
            ];

            instructions.extend(instructions_to_be_duck_conform);

            let mut body = method.emit(
                Some(("duck_internal_self".to_string(), format!("*{struct_name}"))),
                type_env,
                to_ir,
            );
            if let IrInstruction::FunDef(_, _, _, _, body) = &mut body {
                fixup_method_body(
                    struct_name.as_str(),
                    body,
                    s.mut_methods.contains(&method.name),
                );
            }

            instructions.push(body);
        }

        for generic_method in type_env.get_generic_methods(struct_name.clone()).clone() {
            let mut body = generic_method.emit(
                Some(("duck_internal_self".to_string(), format!("*{struct_name}"))),
                type_env,
                to_ir,
            );
            if let IrInstruction::FunDef(_, _, _, _, body) = &mut body {
                fixup_method_body(struct_name.as_str(), body, true);
            }
            instructions.push(body);
        }
        result.extend(instructions);
    }

    for named_duck_def in type_env
        .named_duck_definitions
        .clone()
        .iter()
        .chain(type_env.generic_ducks_generated.clone().iter())
        .filter(|s| s.generics.is_empty())
    {
        let mut interface_fields = vec![];

        for field in &named_duck_def.fields {
            interface_fields.extend([
                (
                    format!("Get{}", field.name),
                    vec![],
                    Some(field.type_expr.0.as_go_type_annotation(type_env)),
                ),
                (
                    format!("GetPtr{}", field.name),
                    vec![],
                    Some(format!(
                        "*{}",
                        field.type_expr.0.as_go_type_annotation(type_env)
                    )),
                ),
                (
                    format!("Set{}", field.name),
                    vec![(
                        "param".into(),
                        field.type_expr.0.as_go_type_annotation(type_env),
                    )],
                    None,
                ),
            ])
        }

        result.push(IrInstruction::InterfaceDef(
            named_duck_def.name.clone(),
            vec![],
            interface_fields,
        ));
    }

    result
}

impl TypeExpr {
    pub fn as_go_type_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Ref(t) | TypeExpr::RefMut(t) => {
                format!("*{}", t.0.as_go_type_annotation(type_env))
            }
            TypeExpr::Html => "func (env *TemplEnv) string".to_string(),
            TypeExpr::TypeOf(..) => panic!("typeof should be replace by now"),
            TypeExpr::KeyOf(..) => panic!("keyof should be replace by now"),
            TypeExpr::RawTypeName(..) => panic!(),
            TypeExpr::Array(t) => format!("[]{}", t.0.as_go_type_annotation(type_env)),
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Tag(..) => self.as_clean_go_type_name(type_env),
            TypeExpr::Bool(..) => "bool".to_string(),
            TypeExpr::InlineGo => "any".to_string(),
            TypeExpr::Int(..) => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String(..) => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),

            // todo: type params
            TypeExpr::TypeName(_, name, _) => panic!("type name should be replaced {name}"),
            TypeExpr::Fun(params, return_type, _) => format!(
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
                    .as_ref()
                    .map(|x| x.0.as_go_return_type(type_env))
                    .unwrap_or_default(),
            ),
            TypeExpr::Struct {
                name: _struct,
                type_params: _,
            } => format!("*{}", self.as_clean_go_type_name(type_env)),
            TypeExpr::NamedDuck { .. } => self.as_clean_go_type_name(type_env).to_string(),
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
            TypeExpr::And(_variants) => "any".to_string(),
        };
    }

    pub fn as_go_concrete_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Ref(t) | TypeExpr::RefMut(t) => t.0.as_go_concrete_annotation(type_env),
            TypeExpr::Html => "func (env *TemplEnv) string".to_string(),
            TypeExpr::TypeOf(..) => panic!("typeof should be replaced"),
            TypeExpr::KeyOf(..) => panic!("keyof should be replaced"),
            TypeExpr::Tag(..) => self.as_clean_go_type_name(type_env),
            TypeExpr::RawTypeName(..) => panic!(),
            TypeExpr::Array(t) => format!("[]{}", t.0.as_go_concrete_annotation(type_env)),
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool(..) => "bool".to_string(),
            TypeExpr::Int(..) => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String(..) => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            // todo: type params
            TypeExpr::TypeName(_, name, _type_params) => {
                panic!("type name should be replaced {name}")
            }
            TypeExpr::Fun(params, return_type, _) => format!(
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
                "Duck_{}",
                fields
                    .iter()
                    .map(|field| format!(
                        "{}_{}",
                        field.name,
                        field.type_expr.0.as_clean_go_type_name(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::NamedDuck { name, type_params } => type_env
                .get_duck_def_with_type_params_mut(name.as_str(), type_params, empty_range())
                .name
                .clone(),
            TypeExpr::Struct { name, type_params } => type_env
                .get_struct_def_with_type_params_mut(name, type_params, empty_range())
                .name
                .clone(),
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
            TypeExpr::Or(_) => "any".to_string(),
            TypeExpr::And(_) => panic!("and should be replaced"),
        };
    }

    pub fn type_id(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Ref(t) => format!("Ref_{}", t.0.type_id(type_env)),
            TypeExpr::RefMut(t) => format!("RefMut_{}", t.0.type_id(type_env)),
            TypeExpr::Html => "Html".to_string(),
            TypeExpr::TypeOf(..) => panic!("typeof should be replaced"),
            TypeExpr::KeyOf(..) => panic!("keyof should be replaced"),

            TypeExpr::RawTypeName(_, ident, _) => {
                panic!("{ident:?}")
            }
            TypeExpr::String(_) => "string".to_string(),
            TypeExpr::Array(t) => format!("Array_{}", t.0.type_id(type_env)),
            TypeExpr::Any => "Any".to_string(),
            TypeExpr::Bool(..) => "bool".to_string(),
            TypeExpr::Int(..) => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::Tag(..) => self.as_clean_go_type_name(type_env),
            TypeExpr::Go(identifier) => identifier.clone(),
            // todo: type params
            TypeExpr::TypeName(_, name, _type_params) => name.clone(),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            TypeExpr::Fun(params, return_type, _) => format!(
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
                    .map(|type_expr| format!("_To_{}", type_expr.0.type_id(type_env)))
                    .unwrap_or_else(|| "".to_string())
            ),
            TypeExpr::Struct { .. } => self.as_clean_go_type_name(type_env),
            TypeExpr::NamedDuck { .. } => self.as_clean_go_type_name(type_env),
            TypeExpr::Duck(duck) => format!(
                "Duck_{}",
                duck.fields
                    .iter()
                    .map(|field| format!("{}_{}", field.name, field.type_expr.0.type_id(type_env)))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Tuple(fields) => {
                format!(
                    "Tup_{}",
                    fields
                        .iter()
                        .map(|type_expr| type_expr.0.type_id(type_env))
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
            TypeExpr::And(variants) => {
                // mvmo 03.07.25: Check for double sort
                let mut variants = variants
                    .clone()
                    .iter()
                    .map(|variant| variant.0.type_id(type_env))
                    .collect::<Vec<_>>();

                variants.sort();

                return format!("Intersection_{}", variants.join("_and_"));
            }
        };
    }

    pub fn build_extension_access_function_name(
        &self,
        extension_name: &str,
        type_env: &mut TypeEnv,
    ) -> String {
        return format!(
            "Extend_{}_with_{}",
            self.as_clean_go_type_name(type_env),
            extension_name,
        );
    }

    pub fn as_clean_go_type_name(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Ref(t) => format!("Ref___{}", t.0.as_clean_go_type_name(type_env)),
            TypeExpr::RefMut(t) => format!("RefMut___{}", t.0.as_clean_go_type_name(type_env)),
            TypeExpr::Html => "Html".to_string(),
            TypeExpr::TypeOf(..) => panic!("typeof should be replaced"),
            TypeExpr::KeyOf(..) => panic!("keyof should be replaced"),

            TypeExpr::RawTypeName(_, ident, _) => {
                panic!("{ident:?}")
            }
            TypeExpr::Array(t) => format!("Array_{}", t.0.as_clean_go_type_name(type_env)),
            TypeExpr::Any => "Any".to_string(),
            TypeExpr::Bool(..) => "bool".to_string(),
            TypeExpr::Int(..) => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String(..) => "string".to_string(),
            TypeExpr::Tag(identifier) => format!("Tag__{identifier}"),
            TypeExpr::Go(identifier) => identifier.clone(),
            // todo: type params
            TypeExpr::TypeName(_, name, type_params) => TypeExpr::Struct {
                name: name.clone(),
                type_params: type_params.clone(),
            }
            .as_clean_go_type_name(type_env),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            TypeExpr::Fun(params, return_type, is_mut) => format!(
                "Fun_{}_From_{}{}",
                if *is_mut { "Mut" } else { "NotMut" },
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
            TypeExpr::Struct {
                name: s,
                type_params,
            } => vec![s.clone()]
                .into_iter()
                .chain(
                    type_params
                        .iter()
                        .map(|(x, _)| x.as_clean_go_type_name(type_env)),
                )
                .collect::<Vec<_>>()
                .join(MANGLE_SEP),
            TypeExpr::NamedDuck {
                name: s,
                type_params,
            } => vec![s.clone()]
                .into_iter()
                .chain(
                    type_params
                        .iter()
                        .map(|(x, _)| x.as_clean_go_type_name(type_env)),
                )
                .collect::<Vec<_>>()
                .join(MANGLE_SEP),
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
            TypeExpr::And(variants) => {
                // mvmo 03.07.25: Check for double sort
                let mut variants = variants
                    .clone()
                    .iter()
                    .map(|variant| variant.0.as_clean_go_type_name(type_env))
                    .collect::<Vec<_>>();

                variants.sort();

                return format!("Intersection_{}", variants.join("_and_"));
            }
        };
    }
}
