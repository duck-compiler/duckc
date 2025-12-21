use std::collections::HashSet;

use crate::{
    emit::{
        fix_ident_for_go,
        value::{IrInstruction, IrValue, ToIr},
    },
    parse::{
        Field,
        schema_def_parser::SchemaDefinition,
        struct_parser::StructDefinition,
        type_parser::{Duck, TypeExpr},
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

pub fn fixup_method_body(
    _struct_name: &str,
    fixed_struct_method: &str,
    body: &mut Vec<IrInstruction>,
    insert_org_addr: bool,
) {
    let mut to_insert = Vec::new();

    if insert_org_addr {
        to_insert.push(IrInstruction::VarDecl(
            "Δorg_addr".to_string(),
            format!("*{fixed_struct_method}"),
        ));
        to_insert.push(IrInstruction::VarAssignment(
            "Δorg_addr".to_string(),
            IrValue::Imm("duck_internal_self".to_string()),
        ));
    }

    to_insert.push(IrInstruction::VarDecl(
        "self".to_string(),
        format!("**{fixed_struct_method}"),
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

pub fn fix_type_name(s: &str, imports: &HashSet<String>) -> String {
    fix_ident_for_go(s, imports)
}

pub fn emit_type_definitions(
    type_env: &mut TypeEnv,
    to_ir: &mut ToIr,
    // imports: &'static HashSet<String>,
) -> Vec<IrInstruction> {
    let imports = type_env.all_go_imports;

    let mut result = Vec::new();
    let mut emitted_types = HashSet::new();

    let mut all_tuples_and_ducks = type_env.find_ducks_and_tuples();

    let tags_to_push = ["greater", "smaller", "equal"];

    for tag in tags_to_push {
        if !all_tuples_and_ducks.iter().any(|t| match t {
            NeedsSearchResult::Tag { name } => name.as_str() == tag,
            _ => false,
        }) {
            all_tuples_and_ducks.push(NeedsSearchResult::Tag {
                name: tag.to_string(),
            });
        }
    }

    for tuple_or_duck in &all_tuples_and_ducks {
        match tuple_or_duck {
            NeedsSearchResult::Array { type_expr } => {
                let array_type = TypeExpr::Array(type_expr.clone().into());

                let array_type_name = array_type.as_clean_go_type_name(type_env);
                let array_type_ano = array_type.as_go_type_annotation(type_env);

                if array_type.implements_eq(type_env) {
                    let fun_name = format!("{array_type_name}_Eq");

                    let go_code = r#"
                        if len(self) != len(other) {
                            return false
                        }

                        for i := range self {
                            a := self[i]
                            b := other[i]

                            if !($%$%$%) {
                                return false
                            }
                        }

                        return true
                    "#
                    .replace("$%$%$%", &type_expr.0.call_eq("a", "b", type_env));

                    result.push(IrInstruction::FunDef(
                        fun_name,
                        None,
                        vec![
                            ("self".to_string(), array_type_ano.clone()),
                            ("other".to_string(), array_type_ano.clone()),
                        ],
                        Some("bool".to_string()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }

                if array_type.implements_to_string(type_env) {
                    let fun_name = format!("{array_type_name}_ToString");

                    let go_code = r#"
                        res := ""

                        for i := range self {
                            a := self[i]
                            _ = a

                            if i != 0 {
                                res = res + ", "
                            }

                            a_x := ($%$%$%)
                            res = res + a_x
                        }

                        return fmt.Sprintf("[%s]", res)
                    "#
                    .replace("$%$%$%", &type_expr.0.call_to_string("a", type_env));

                    result.push(IrInstruction::FunDef(
                        fun_name,
                        None,
                        vec![("self".to_string(), array_type_ano.clone())],
                        Some("string".to_string()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }

                if array_type.implements_clone(type_env) {
                    let fun_name = format!("{array_type_name}_Clone");

                    let go_code = r#"
                        res := make($$$ARRAY_TYPE, len(self))

                        for i := range self {
                            a := self[i]
                            a_x := ($%$%$%)
                            res[i] = a_x
                        }

                        return res
                    "#
                    .replace("$$$ARRAY_TYPE", &array_type_ano)
                    .replace("$%$%$%", &type_expr.0.call_clone("a", type_env));

                    result.push(IrInstruction::FunDef(
                        fun_name,
                        None,
                        vec![("self".to_string(), array_type_ano.clone())],
                        Some(array_type_ano.clone()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }

                if array_type.implements_hash(type_env) {
                    let fun_name = format!("{array_type_name}_Hash");

                    let go_code = r#"
                        var res int
                        res = 1

                        for i := range self {
                            a := self[i]
                            a_x := ($%$%$%)
                            res = (31 * res) + a_x
                        }

                        return res
                    "#
                    .replace("$%$%$%", &type_expr.0.call_hash("a", type_env));

                    result.push(IrInstruction::FunDef(
                        fun_name,
                        None,
                        vec![("self".to_string(), array_type_ano.clone())],
                        Some("int".to_string()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }

                if array_type.implements_ord(type_env) {
                    let fun_name = format!("{array_type_name}_Ord");

                    let go_code = r#"
                        other := *other_param

                        if len(self) < len(other) {
                            return Tag__smaller{}
                        } else if len(self) > len(other) {
                            return Tag__greater{}
                        }

                        for i := range self {
                            a := self[i]
                            b := other[i]
                            inter_res := ($%$%$%)

                            var mm any
                            mm = inter_res
                            switch mm.(type) {
                            case Tag__greater:
                                return Tag__greater{}
                            case Tag__smaller:
                                return Tag__smaller{}
                            }
                        }

                        return Tag__equal{}
                    "#
                    .replace("$%$%$%", &type_expr.0.call_ord("a", "b", type_env));

                    result.push(IrInstruction::FunDef(
                        fun_name,
                        None,
                        vec![
                            ("self".to_string(), array_type_ano.clone()),
                            ("other_param".to_string(), format!("*{array_type_ano}")),
                        ],
                        Some("any".to_string()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }
            }
            NeedsSearchResult::Duck { fields } => {
                let duck_type_expr = TypeExpr::Duck(Duck {
                    fields: fields.clone(),
                });

                let type_name = duck_type_expr.as_clean_go_type_name(type_env);

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

                result.push(SchemaDefinition::emit_from_json_fn_from_duck(
                    &duck_type_expr,
                    type_env,
                    to_ir,
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
                                format!("self.{}", fix_ident_for_go(&field.name, imports)),
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
                let tuple_type = TypeExpr::Tuple(fields.clone());
                let type_name = tuple_type.as_clean_go_type_name(type_env);
                let type_anno = tuple_type.as_go_type_annotation(type_env);
                result.push(IrInstruction::StructDef(
                    type_name.clone(),
                    fields
                        .iter()
                        .enumerate()
                        .map(|(i, x)| (format!("field_{i}"), x.0.as_go_type_annotation(type_env)))
                        .collect::<Vec<_>>(),
                ));
                if tuple_type.implements_eq(type_env) {
                    let mut comparisons = Vec::new();

                    for (i, f) in fields.iter().enumerate() {
                        let field_name = format!("field_{i}");
                        comparisons.push(f.0.call_eq(
                            &format!("self.{field_name}"),
                            &format!("other.{field_name}"),
                            type_env,
                        ));
                    }

                    if comparisons.is_empty() {
                        comparisons.push(String::from("true"));
                    }

                    result.push(IrInstruction::FunDef(
                        "eq".to_string(),
                        Some(("self".to_string(), format!("*{type_name}"))),
                        vec![("other".to_string(), format!("*{type_name}"))],
                        Some(String::from("bool".to_string())),
                        vec![IrInstruction::Return(Some(IrValue::Imm(
                            comparisons.join(" && "),
                        )))],
                    ));
                }

                if tuple_type.implements_to_string(type_env) {
                    let mut go_code = String::from(
                        r#"
                        res := ""
                    "#,
                    );

                    for (i, field) in fields.iter().enumerate() {
                        if i != 0 {
                            go_code.push_str("\nres = res + \", \"");
                        }

                        let to_string_call = field
                            .0
                            .call_to_string(&format!("self.field_{}", i), type_env);
                        go_code.push_str(&format!("\nres = res + ({to_string_call})"))
                    }

                    go_code.push_str("\nreturn fmt.Sprintf(\"(%s)\", res)");

                    result.push(IrInstruction::FunDef(
                        "to_string".to_string(),
                        Some(("self".to_string(), type_name.clone())),
                        vec![],
                        Some(String::from("string".to_string())),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }

                if tuple_type.implements_clone(type_env) {
                    let mut go_code = String::from("res := *new($$$TUPLE_TYPE)")
                        .replace("$$$TUPLE_TYPE", &type_name);

                    for (i, field) in fields.iter().enumerate() {
                        let clone_call = field.0.call_clone(&format!("self.field_{}", i), type_env);
                        go_code.push_str(&format!("\nres.field_{i} = ({clone_call})"))
                    }

                    go_code.push_str("\nreturn res");

                    result.push(IrInstruction::FunDef(
                        "clone".to_string(),
                        Some(("self".to_string(), type_anno.clone())),
                        vec![],
                        Some(type_anno.clone()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }

                if tuple_type.implements_hash(type_env) {
                    let mut go_code = String::from(
                        r#"
                        var res int
                        res = 1
                        "#,
                    );

                    for (i, field) in fields.iter().enumerate() {
                        let hash_call = field.0.call_hash(&format!("self.field_{}", i), type_env);
                        go_code.push_str(&format!("\nres = (31 * res) + ({hash_call})"))
                    }

                    go_code.push_str("\nreturn res");

                    result.push(IrInstruction::FunDef(
                        "hash".to_string(),
                        Some(("self".to_string(), type_anno.clone())),
                        vec![],
                        Some("int".to_string()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }

                if tuple_type.implements_ord(type_env) {
                    let mut go_code = String::new();

                    go_code.push_str("var r any\nr = Tag__equal{}\n");

                    for (i, field) in fields.iter().enumerate() {
                        go_code.push('\n');
                        let ord_call = field.0.call_ord(
                            &format!("self.field_{}", i),
                            &format!("other.field_{}", i),
                            type_env,
                        );
                        go_code.push_str(&format!(
                            r#"
                            r = {ord_call}
                            switch r.(type) {{
                            case Tag__greater:
                            return Tag__greater{{}}
                            case Tag__smaller:
                            return Tag__smaller{{}}
                            }}
                            "#
                        ));
                    }

                    go_code.push_str("\nreturn r");

                    result.push(IrInstruction::FunDef(
                        "ord".to_string(),
                        Some(("self".to_string(), type_anno.clone())),
                        vec![("other".to_string(), format!("*{type_anno}"))],
                        Some("any".to_string()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }
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
        let fixed_struct_name = format!("Struct_{}", s.name);
        result.push(IrInstruction::StructDef(
            fixed_struct_name.clone(),
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
            derived,
        } = s;

        let mut instructions: Vec<IrInstruction> = fields
            .iter()
            .flat_map(|field| {
                vec![
                    IrInstruction::FunDef(
                        format!("Get{}", field.name),
                        Some(("self".into(), format!("*{fixed_struct_name}"))),
                        vec![],
                        Some(field.type_expr.0.as_go_type_annotation(type_env)),
                        vec![IrInstruction::Return(Some(IrValue::FieldAccess(
                            IrValue::Var("self".into()).into(),
                            field.name.clone(),
                        )))],
                    ),
                    IrInstruction::FunDef(
                        format!("GetPtr{}", field.name),
                        Some(("self".into(), format!("*{fixed_struct_name}"))),
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
                        Some(("self".into(), format!("*{fixed_struct_name}"))),
                        vec![(
                            "param".into(),
                            field.type_expr.0.as_go_type_annotation(type_env),
                        )],
                        None,
                        vec![IrInstruction::VarAssignment(
                            format!("self.{}", fix_ident_for_go(&field.name, imports)),
                            IrValue::Var("param".into()),
                        )],
                    ),
                ]
                .into_iter()
            })
            .collect();

        for method in methods.iter() {
            if !method.generics.is_empty() {
                continue;
            }

            if !type_env.is_resolved(struct_name, &method.name) {
                continue;
            }

            let func_type_str = format!(
                "func ({}) {}",
                method
                    .params
                    .iter()
                    .map(|param| format!(
                        "{} {}",
                        param.0,
                        param.1.0.as_go_type_annotation(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join(","),
                method.return_type.0.as_go_type_annotation(type_env),
            );

            let instructions_to_be_duck_conform = vec![
                IrInstruction::FunDef(
                    format!("Get{}", method.name),
                    Some(("self".into(), format!("*{fixed_struct_name}"))),
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
                                    .map(|param| {
                                        (param.0.clone(), param.1.0.as_go_type_annotation(type_env))
                                    })
                                    .collect::<Vec<_>>(),
                                Some(method.return_type.0.as_go_type_annotation(type_env)),
                                vec![IrInstruction::InlineGo(format!(
                                    "return self.{}({})",
                                    fix_ident_for_go(&method.name, imports),
                                    method
                                        .params
                                        .iter()
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
                    Some(("self".into(), format!("*{fixed_struct_name}"))),
                    vec![],
                    Some(format!(
                        "*{}",
                        method.type_expr().0.as_go_return_type(type_env)
                    )),
                    vec![IrInstruction::Return(Some(IrValue::Nil))],
                ),
                IrInstruction::FunDef(
                    format!("Set{}", method.name),
                    Some(("self".into(), format!("*{fixed_struct_name}"))),
                    vec![("param".into(), func_type_str)],
                    None,
                    vec![],
                ),
            ];

            instructions.extend(instructions_to_be_duck_conform);

            let mut body = method.emit(
                Some((
                    "duck_internal_self".to_string(),
                    format!("*{fixed_struct_name}"),
                )),
                type_env,
                to_ir,
            );
            if let IrInstruction::FunDef(_, _, _, _, body) = &mut body {
                fixup_method_body(
                    struct_name,
                    &fixed_struct_name,
                    body,
                    s.mut_methods.contains(&method.name),
                );
            }

            instructions.push(body);
        }

        for derived_interface in derived.iter() {
            match *derived_interface {
                crate::parse::struct_parser::DerivableInterface::Eq => {
                    let receiver = fixed_struct_name.clone();

                    let mut comparisons = Vec::new();

                    for f in fields.iter() {
                        comparisons.push(f.type_expr.0.call_eq(
                            &format!("self.{}", f.name),
                            &format!("(*other).{}", f.name),
                            type_env,
                        ));
                    }

                    if comparisons.is_empty() {
                        comparisons.push("true".to_string());
                    }

                    instructions.push(IrInstruction::FunDef(
                        "eq".to_string(),
                        Some(("self".to_string(), format!("*{receiver}"))),
                        vec![("other".to_string(), format!("**{receiver}"))],
                        Some("bool".to_string()),
                        vec![IrInstruction::Return(Some(IrValue::Imm(
                            comparisons.join(" && "),
                        )))],
                    ));
                }
                crate::parse::struct_parser::DerivableInterface::ToString => {
                    let receiver = fixed_struct_name.clone();

                    let mut string_parts = Vec::new();

                    for f in fields.iter() {
                        string_parts.push(format!(
                            "\"{}: \" + {}",
                            f.name,
                            f.type_expr
                                .0
                                .call_to_string(&format!("self.{}", f.name), type_env),
                        ));
                    }

                    if string_parts.is_empty() {
                        string_parts.push("\"\"".to_string());
                    }

                    instructions.push(IrInstruction::FunDef(
                        "to_string".to_string(),
                        Some(("self".to_string(), format!("*{receiver}"))),
                        vec![],
                        Some("string".to_string()),
                        vec![IrInstruction::Return(Some(IrValue::Imm(format!(
                            r#"fmt.Sprintf("{}{{%s}}", {})"#,
                            struct_name,
                            string_parts.join(" + \" \" + ")
                        ))))],
                    ));
                }
                crate::parse::struct_parser::DerivableInterface::Clone => {
                    let receiver = fixed_struct_name.clone();

                    let mut clone_string = format!("&{receiver}{{\n");

                    for f in fields.iter() {
                        clone_string.push_str(&format!(
                            "{}: {},\n",
                            f.name,
                            f.type_expr
                                .0
                                .call_clone(&format!("self.{}", f.name), type_env),
                        ));
                    }
                    clone_string.push_str("}\n");

                    instructions.push(IrInstruction::FunDef(
                        "clone".to_string(),
                        Some(("self".to_string(), format!("*{receiver}"))),
                        vec![],
                        Some(format!("*{receiver}")),
                        vec![IrInstruction::Return(Some(IrValue::Imm(clone_string)))],
                    ));
                }
                crate::parse::struct_parser::DerivableInterface::Hash => {
                    let receiver = fixed_struct_name.clone();
                    let mut go_code = String::from(
                        r#"
                        var res int
                        res = 1
                        "#,
                    );

                    for field in fields.iter() {
                        let hash_call = field
                            .type_expr
                            .0
                            .call_hash(&format!("self.{}", field.name), type_env);
                        go_code.push_str(&format!("\nres = (31 * res) + ({hash_call})"))
                    }

                    go_code.push_str("\nreturn res");

                    result.push(IrInstruction::FunDef(
                        "hash".to_string(),
                        Some(("self".to_string(), format!("*{receiver}"))),
                        vec![],
                        Some("int".to_string()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }
                crate::parse::struct_parser::DerivableInterface::Ord => {
                    let receiver = fixed_struct_name.clone();
                    let mut go_code = String::new();

                    go_code.push_str("var r any\nr = Tag__equal{}\n");

                    for field in fields.iter() {
                        go_code.push('\n');
                        let ord_call = field.type_expr.0.call_ord(
                            &format!("self.{}", field.name),
                            &format!("(*other).{}", field.name),
                            type_env,
                        );
                        go_code.push_str(&format!(
                            r#"
                            r = {ord_call}
                            switch r.(type) {{
                            case Tag__greater:
                            return Tag__greater{{}}
                            case Tag__smaller:
                            return Tag__smaller{{}}
                            }}
                            "#
                        ));
                    }

                    go_code.push_str("\nreturn r");

                    result.push(IrInstruction::FunDef(
                        "ord".to_string(),
                        Some(("self".to_string(), format!("*{receiver}"))),
                        vec![("other".to_string(), format!("**{receiver}"))],
                        Some("any".to_string()),
                        vec![IrInstruction::InlineGo(go_code)],
                    ));
                }
            }
        }

        for generic_method in type_env.get_generic_methods(struct_name.clone()).clone() {
            let mut body = generic_method.emit(
                Some((
                    "duck_internal_self".to_string(),
                    format!("*{fixed_struct_name}"),
                )),
                type_env,
                to_ir,
            );
            if let IrInstruction::FunDef(_, _, _, _, body) = &mut body {
                fixup_method_body(struct_name, &fixed_struct_name, body, true);
            }
            instructions.push(body);
        }
        result.extend(instructions);
    }

    result.push(IrInstruction::StructDef("Never".to_string(), vec![]));
    result.push(IrInstruction::StructDef("Statement".to_string(), vec![]));

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

    result.push(IrInstruction::FunDef(
        "Int_Hash".to_string(),
        None,
        vec![("self".to_string(), "int".to_string())],
        Some("int".to_string()),
        vec![IrInstruction::InlineGo("return self".to_string())],
    ));

    result.push(IrInstruction::FunDef(
        "Int_Ord".to_string(),
        None,
        vec![
            ("self".to_string(), "int".to_string()),
            ("other".to_string(), "*int".to_string()),
        ],
        Some("any".to_string()),
        vec![IrInstruction::InlineGo("if self < *other { return Tag__smaller{} } else if self > *other { return Tag__greater{} } else { return Tag__equal{} }".to_string())],
    ));

    result.push(IrInstruction::FunDef(
        "Bool_Hash".to_string(),
        None,
        vec![("self".to_string(), "bool".to_string())],
        Some("int".to_string()),
        vec![IrInstruction::InlineGo(
            "if self {\nreturn 1\n} else {\nreturn 2\n}".to_string(),
        )],
    ));

    result.push(IrInstruction::FunDef(
        "Bool_Ord".to_string(),
        None,
        vec![
            ("self".to_string(), "bool".to_string()),
            ("other".to_string(), "*bool".to_string()),
        ],
        Some("any".to_string()),
        vec![IrInstruction::InlineGo(
            "if !self && *other { return Tag__smaller{} } else if self && !*other { return Tag__greater{} } else { return Tag__equal{} }".to_string(),
        )],
    ));

    result.push(IrInstruction::FunDef(
        "String_Hash".to_string(),
        None,
        vec![("self".to_string(), "string".to_string())],
        Some("int".to_string()),
        vec![IrInstruction::InlineGo(
            r#"
                    var h maphash.Hash
                    h.WriteString(self)
                    return int(h.Sum64())
                    "#
            .to_string(),
        )],
    ));

    result.push(IrInstruction::FunDef(
        "String_Ord".to_string(),
        None,
        vec![
            ("self".to_string(), "string".to_string()),
            ("other_param".to_string(), "*string".to_string()),
        ],
        Some("any".to_string()),
        vec![IrInstruction::InlineGo(
            r#"
                other := *other_param
                if len(self) < len(other) {
                    return Tag__smaller{}
                } else if len(self) > len(other) {
                    return Tag__greater{}
                } else {
                    runes_self := []rune(self)
                    runes_other := []rune(self)

                    for i := range runes_self {
                        if runes_self[i] < runes_other[i] {
                            return Tag__smaller{}
                        } else if runes_self[i] > runes_other[i] {
                            return Tag__greater{}
                        }
                    }

                    return Tag__equal{}
                }

                    "#
            .to_string(),
        )],
    ));

    result.push(IrInstruction::FunDef(
        "Float_Hash".to_string(),
        None,
        vec![("self".to_string(), "float32".to_string())],
        Some("int".to_string()),
        vec![IrInstruction::InlineGo(
            r#"
                            return int(self)
                        "#
            .to_string(),
        )],
    ));

    result.push(IrInstruction::FunDef(
        "Float_Ord".to_string(),
        None,
        vec![
            ("self".to_string(), "float32".to_string()),
            ("other".to_string(), "*float32".to_string()),
        ],
        Some("any".to_string()),
        vec![IrInstruction::InlineGo(
            r#"
                x := self
                y := *other
                if x > y {
                    return Tag__greater{}
                } else if x < y {
                    return Tag__smaller{}
                } else {
                    return Tag__equal{}
                }
                        "#
            .to_string(),
        )],
    ));

    result.push(IrInstruction::FunDef(
        "Char_Hash".to_string(),
        None,
        vec![("self".to_string(), "rune".to_string())],
        Some("int".to_string()),
        vec![IrInstruction::InlineGo(
            r#"

                        return int(self)

                    "#
            .to_string(),
        )],
    ));

    result.push(IrInstruction::GenericFun(
        "IDENTITY".to_string(),
        vec![("T".to_string(), "any".to_string())],
        vec![("x".to_string(), "T".to_string())],
        Some("T".to_string()),
        vec![IrInstruction::InlineGo("return x".to_string())],
    ));

    result.push(IrInstruction::FunDef(
        "Char_Ord".to_string(),
        None,
        vec![
            ("self".to_string(), "rune".to_string()),
            ("other".to_string(), "*rune".to_string()),
        ],
        Some("any".to_string()),
        vec![IrInstruction::InlineGo(
            r#"
                        x := self
                        y := *other

                        if x > y {
                            return Tag__greater{}
                        } else if x < y {
                            return Tag__smaller{}
                        } else {
                            return Tag__equal{}
                        }
                    "#
            .to_string(),
        )],
    ));

    result
}

impl TypeExpr {
    pub fn as_go_type_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Statement => "Tup_".to_string(),
            TypeExpr::Never => "any".to_string(),
            TypeExpr::TemplParam(name) => panic!("should not be here {name}"),
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
                return_type.0.as_go_return_type(type_env),
            ),
            TypeExpr::Struct {
                name: _struct,
                type_params: _,
            } => format!(
                "*{}",
                fix_type_name(
                    &self.as_clean_go_type_name(type_env),
                    type_env.all_go_imports
                )
            ),
            TypeExpr::NamedDuck { .. } => fix_type_name(
                &self.as_clean_go_type_name(type_env),
                type_env.all_go_imports,
            ),
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

    pub fn type_id(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Statement => "Statement".to_string(),
            TypeExpr::Never => "Never".to_string(),
            TypeExpr::TemplParam(name) => panic!("should not be here {name}"),
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
            TypeExpr::Fun(params, return_type, _) => format!(
                "Fun_From_{}_To_{}",
                params
                    .iter()
                    .map(|(name, type_expr)| format!(
                        "{}_{}",
                        name.clone().unwrap_or_else(|| "".to_string()),
                        type_expr.0.type_id(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_"),
                return_type.0.type_id(type_env),
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

    pub fn is_never(&self) -> bool {
        matches!(self, TypeExpr::Never)
    }

    pub fn is_statement(&self) -> bool {
        matches!(self, TypeExpr::Statement)
    }

    pub fn as_clean_go_type_name(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Statement => "Statement".to_string(),
            TypeExpr::Never => "Never".to_string(),
            TypeExpr::TemplParam(name) => panic!("should not be here {name}"),
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
            TypeExpr::Fun(params, return_type, is_mut) => format!(
                "Fun_{}_From_{}_To_{}",
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
                return_type.0.as_clean_go_type_name(type_env),
            ),
            TypeExpr::Struct {
                name: s,
                type_params,
            } => format!(
                "Struct_{}",
                vec![s.clone()]
                    .into_iter()
                    .chain(
                        type_params
                            .iter()
                            .map(|(x, _)| x.as_clean_go_type_name(type_env)),
                    )
                    .collect::<Vec<_>>()
                    .join(MANGLE_SEP)
            ),
            TypeExpr::NamedDuck {
                name: s,
                type_params,
            } => format!(
                "Interface_{}",
                vec![s.clone()]
                    .into_iter()
                    .chain(
                        type_params
                            .iter()
                            .map(|(x, _)| x.as_clean_go_type_name(type_env)),
                    )
                    .collect::<Vec<_>>()
                    .join(MANGLE_SEP),
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
