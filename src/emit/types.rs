use std::collections::HashMap;

use crate::{
    emit::value::{IrInstruction, IrValue},
    parse::{function_parser::LambdaFunctionExpr, type_parser::{Duck, Field, Struct, TypeDefinition, TypeExpr}, value_parser::{Assignment, Declaration, ValFmtStringContents, ValueExpr}},
    semantics::type_resolve::{GenericDefinition, TypeEnv},
};

pub fn primitive_native_type_name<'a>(primitive_type_expr: &TypeExpr) -> &'a str {
    match primitive_type_expr {
        TypeExpr::String => "string",
        TypeExpr::Int => "int",
        TypeExpr::Float => "float32",
        TypeExpr::Bool => "bool",
        TypeExpr::Char => "rune",
        TypeExpr::IntLiteral(..) => "int",
        TypeExpr::StringLiteral(..) => "string",
        TypeExpr::BoolLiteral(..) => "bool",
        _ => panic!("That's not a primitive"),
    }
}

pub fn escape_string_literal(input_str: &str) -> String {
    input_str
        .chars()
        .filter_map(|c| {
            if c.is_alphanumeric() {
                Some(c)
            } else if c == ' ' {
                Some('_')
            } else {
                None
            }
        })
        .collect()
}

pub fn primitive_conc_type_name<'a>(primitive_type_expr: &TypeExpr) -> &'a str {
    match primitive_type_expr {
        TypeExpr::String => "ConcDuckString",
        TypeExpr::Int => "ConcDuckInt",
        TypeExpr::Float => "ConcDuckFloat",
        TypeExpr::Bool => "ConcDuckBool",
        TypeExpr::Char => "ConcDuckChar",
        TypeExpr::IntLiteral(int) => Box::leak(Box::new(format!("IntLiteral_{int}"))),
        TypeExpr::StringLiteral(str) => Box::leak(Box::new(format!(
            "StringLiteral_{}",
            escape_string_literal(str)
        ))),
        TypeExpr::BoolLiteral(bool) => Box::leak(Box::new(format!("BoolLiteral_{bool}"))),
        _ => panic!("That's not a primitive"),
    }
}

pub fn primitive_type_name<'a>(primitive_type_expr: &TypeExpr) -> &'a str {
    match primitive_type_expr {
        TypeExpr::String => "DuckString",
        TypeExpr::Int => "DuckInt",
        TypeExpr::Float => "DuckFloat",
        TypeExpr::Bool => "DuckBool",
        TypeExpr::Char => "DuckChar",
        TypeExpr::IntLiteral(int) => Box::leak(Box::new(format!("IntLiteral_{int}"))),
        TypeExpr::StringLiteral(str) => Box::leak(Box::new(format!(
            "StringLiteral_{}",
            escape_string_literal(str)
        ))),
        TypeExpr::BoolLiteral(bool) => Box::leak(Box::new(format!("BoolLiteral_{bool}"))),
        _ => panic!("That's not a primitive"),
    }
}

pub fn replace_generics_in_value_expr(
    value_expr: &mut ValueExpr,
    generics_to_concrete_type_map: &HashMap<String, &TypeExpr>,
) {
    match value_expr {
        ValueExpr::Array(ty, exprs) => {
            if let Some(t) = ty {
                replace_generics_in_type_expr(&mut t.0, generics_to_concrete_type_map);
            }
            for expr in exprs {
                replace_generics_in_value_expr(&mut expr.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::VarDecl(d) => {
            let Declaration {
                type_expr,
                initializer,
                ..
            } = &mut d.0;
            replace_generics_in_type_expr(&mut type_expr.0, generics_to_concrete_type_map);
            if let Some(init) = initializer {
                replace_generics_in_value_expr(&mut init.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::Lambda(l) => {
            let LambdaFunctionExpr {
                params,
                return_type,
                value_expr,
            } = &mut **l;
            if let Some(rt) = return_type {
                replace_generics_in_type_expr(&mut rt.0, generics_to_concrete_type_map);
            }
            for (_, p) in params {
                replace_generics_in_type_expr(&mut p.0, generics_to_concrete_type_map);
            }
            replace_generics_in_value_expr(&mut value_expr.0, generics_to_concrete_type_map);
        }
        ValueExpr::Add(l, r)
        | ValueExpr::Mul(l, r)
        | ValueExpr::Equals(l, r)
        | ValueExpr::ArrayAccess(l, r) => {
            replace_generics_in_value_expr(&mut l.0, generics_to_concrete_type_map);
            replace_generics_in_value_expr(&mut r.0, generics_to_concrete_type_map);
        }
        ValueExpr::Block(exprs) | ValueExpr::Tuple(exprs) => {
            for expr in exprs {
                replace_generics_in_value_expr(&mut expr.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::Duck(fields) | ValueExpr::Struct(fields) => {
            for (_, field_val) in fields {
                replace_generics_in_value_expr(&mut field_val.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::BoolNegate(e) => {
            replace_generics_in_value_expr(&mut e.0, generics_to_concrete_type_map);
        }
        ValueExpr::FieldAccess { target_obj, .. } => {
            replace_generics_in_value_expr(&mut target_obj.0, generics_to_concrete_type_map);
        }
        ValueExpr::FormattedString(contents) => {
            for content in contents {
                if let ValFmtStringContents::Expr(e) = content {
                    replace_generics_in_value_expr(&mut e.0, generics_to_concrete_type_map);
                }
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
        } => {
            replace_generics_in_value_expr(&mut target.0, generics_to_concrete_type_map);
            for p in params {
                replace_generics_in_value_expr(&mut p.0, generics_to_concrete_type_map);
            }
            if let Some(t_params) = type_params {
                for tp in t_params {
                    replace_generics_in_type_expr(&mut tp.0, generics_to_concrete_type_map);
                }
            }
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            replace_generics_in_value_expr(&mut condition.0, generics_to_concrete_type_map);
            replace_generics_in_value_expr(&mut then.0, generics_to_concrete_type_map);
            if let Some(e) = r#else {
                replace_generics_in_value_expr(&mut e.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::Match { value_expr, arms } => {
            replace_generics_in_value_expr(&mut value_expr.0, generics_to_concrete_type_map);
            for arm in arms {
                replace_generics_in_type_expr(&mut arm.type_case.0, generics_to_concrete_type_map);
                replace_generics_in_value_expr(&mut arm.value_expr.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::Return(r) => {
            if let Some(e) = r {
                replace_generics_in_value_expr(&mut e.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::VarAssign(a) => {
            let Assignment { target, value_expr } = &mut a.0;
            replace_generics_in_value_expr(&mut target.0, generics_to_concrete_type_map);
            replace_generics_in_value_expr(&mut value_expr.0, generics_to_concrete_type_map);
        }
        ValueExpr::While { condition, body } => {
            replace_generics_in_value_expr(&mut condition.0, generics_to_concrete_type_map);
            replace_generics_in_value_expr(&mut body.0, generics_to_concrete_type_map);
        }

        ValueExpr::Break
        | ValueExpr::Continue
        | ValueExpr::InlineGo(..)
        | ValueExpr::Int(..)
        | ValueExpr::String(..)
        | ValueExpr::Char(..)
        | ValueExpr::Float(..)
        | ValueExpr::Bool(..)
        | ValueExpr::Variable(..)
        | ValueExpr::RawVariable(..) => {}
    }
}

pub fn replace_generics_in_type_expr(
    type_expr: &mut TypeExpr,
    generics_to_concrete_type_map: &HashMap<String, &TypeExpr>,
) {
    match type_expr {
        TypeExpr::GenericToBeReplaced(name) | TypeExpr::TypeName(_, name, _) => {
            if let Some(concrete_type) = generics_to_concrete_type_map.get(name) {
                *type_expr = (*concrete_type).clone();
            }
        }
        TypeExpr::Struct(Struct { fields }) | TypeExpr::Duck(Duck { fields }) => {
            for field in fields {
                replace_generics_in_type_expr(&mut field.type_expr.0, generics_to_concrete_type_map);
            }
        }
        TypeExpr::Tuple(elements) | TypeExpr::Or(elements) => {
            for (element_expr, _) in elements {
                replace_generics_in_type_expr(element_expr, generics_to_concrete_type_map);
            }
        }
        TypeExpr::Fun(params, return_type) => {
            for (_, param_type_expr) in params {
                replace_generics_in_type_expr(&mut param_type_expr.0, generics_to_concrete_type_map);
            }
            if let Some(rt_box) = return_type {
                replace_generics_in_type_expr(&mut rt_box.0, generics_to_concrete_type_map);
            }
        }
        TypeExpr::Array(element_type_expr) => {
            replace_generics_in_type_expr(&mut element_type_expr.0, generics_to_concrete_type_map);
        }
        TypeExpr::RawTypeName(_, _, type_params) => {
            if let Some(params) = type_params {
                for (param_expr, _) in params {
                    replace_generics_in_type_expr(param_expr, generics_to_concrete_type_map);
                }
            }
        }
        TypeExpr::Any
        | TypeExpr::InlineGo
        | TypeExpr::Go(_)
        | TypeExpr::TypeNameInternal(_)
        | TypeExpr::StringLiteral(_)
        | TypeExpr::IntLiteral(_)
        | TypeExpr::BoolLiteral(_)
        | TypeExpr::String
        | TypeExpr::Int
        | TypeExpr::Bool
        | TypeExpr::Char
        | TypeExpr::Float => {}
    }
}

pub fn emit_type_definitions(type_env: &mut TypeEnv) -> Vec<IrInstruction> {
    let summary = type_env.summarize();

    let concrete_generics_implementation = type_env
           .clone()
           .generics_used
           .iter()
           .flat_map(|(generic_def, with_type_params)| {
               type_env.push_type_aliases();

               generic_def.generics_names()
                   .iter()
                   .enumerate()
                   .for_each(|(index, generic_name)| {
                       type_env.insert_type_alias(
                           generic_name.clone(),
                           with_type_params.get(index)
                               .expect("type param expected")
                               .1.0.clone()
                       );
                   });

               type_env.push_type_aliases();
               let name = generic_def.typename_with_given_typeparams(type_env, with_type_params)
                   .as_clean_go_type_name(type_env);

               let ir_instructions = match generic_def {
                   GenericDefinition::Function(..) => todo!(),
                   GenericDefinition::Type(TypeDefinition { type_expression: (original_type_expr, _), .. }) => {
                       let replacements: HashMap<String, &TypeExpr> = with_type_params
                           .iter()
                           .map(|(name, expr)| (name.clone(), &expr.0))
                           .collect();

                       let mut concrete_type_expr = original_type_expr.clone();

                       replace_generics_in_type_expr(&mut concrete_type_expr, &replacements);
                       dbg!(&name);
                       dbg!(&concrete_type_expr);
                       let mut instructions =
                           interface_implementations(name.clone(), &concrete_type_expr, type_env);

                       instructions.push(match &concrete_type_expr {
                           TypeExpr::Tuple(t) => IrInstruction::StructDef(
                               name.clone(),
                               t.iter()
                                   .enumerate()
                                   .map(|(i, x)| (format!("field_{i}"), x.0.as_go_type_annotation(type_env)))
                                   .collect::<Vec<_>>(),
                           ),
                           TypeExpr::Struct(Struct { fields }) | TypeExpr::Duck(Duck { fields }) => {
                               IrInstruction::StructDef(
                                   name.clone(),
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
                           _ => panic!("cant create for {name}"),
                       });

                       instructions
                   }
               };

               type_env.pop_type_aliases();
               ir_instructions
           })
           .collect::<Vec<IrInstruction>>();

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

    // generics
    instructions.extend(concrete_generics_implementation);

    let mut primitive_types_instructions = summary
        .types_used
        .iter()
        .filter(|type_expr| type_expr.is_primitive())
        .flat_map(|primitive_type_expr| {
            if primitive_type_expr.is_literal() {
                let ir_value = IrValue::Imm(match primitive_type_expr.clone() {
                    TypeExpr::StringLiteral(value) => format!("\"{value}\""),
                    TypeExpr::IntLiteral(int_value) => format!("{int_value}"),
                    TypeExpr::BoolLiteral(bool_value) => format!("{bool_value}"),
                    _ => unreachable!(),
                });

                return vec![
                    IrInstruction::StructDef(
                        primitive_type_expr.as_go_concrete_annotation(type_env),
                        vec![],
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
            TypeExpr::GenericToBeReplaced(..) => panic!("shouldn't access the GenericToBeReplaced as go type annotation"),
            TypeExpr::RawTypeName(..) => panic!(),
            TypeExpr::Array(t) => format!("[]{}", t.0.as_go_type_annotation(type_env)),
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::IntLiteral(i) => primitive_type_name(&TypeExpr::IntLiteral(*i)).to_string(),
            TypeExpr::BoolLiteral(b) => primitive_type_name(&TypeExpr::BoolLiteral(*b)).to_string(),
            TypeExpr::StringLiteral(str) => {
                primitive_type_name(&TypeExpr::StringLiteral(str.clone())).to_string()
            }
            TypeExpr::Bool => "DuckBool".to_string(),
            TypeExpr::InlineGo => "any".to_string(),
            TypeExpr::Int => "DuckInt".to_string(),
            TypeExpr::Float => "DuckFloat".to_string(),
            TypeExpr::Char => "DuckChar".to_string(),
            TypeExpr::String => "DuckString".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeNameInternal(name) => name.clone(),
            // todo: type params
            TypeExpr::TypeName(_, name, type_params) => {
                type_env
                    .resolve_type_alias(name)
                    .as_go_type_annotation(type_env)
            },
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
            TypeExpr::IntLiteral(i) => primitive_type_name(&TypeExpr::IntLiteral(*i)).to_string(),
            TypeExpr::BoolLiteral(b) => primitive_type_name(&TypeExpr::BoolLiteral(*b)).to_string(),
            TypeExpr::StringLiteral(str) => {
                primitive_type_name(&TypeExpr::StringLiteral(str.clone())).to_string()
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
            TypeExpr::Or(_) => "any".to_string(),
        };
    }

    pub fn as_clean_go_type_name(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::GenericToBeReplaced(x) => x.clone(),
            TypeExpr::RawTypeName(..) => panic!(),
            TypeExpr::IntLiteral(i) => primitive_type_name(&TypeExpr::IntLiteral(*i)).to_string(),
            TypeExpr::BoolLiteral(b) => primitive_type_name(&TypeExpr::BoolLiteral(*b)).to_string(),
            TypeExpr::StringLiteral(str) => {
                primitive_type_name(&TypeExpr::StringLiteral(str.clone())).to_string()
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
