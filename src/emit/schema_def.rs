use crate::{
    emit::{
        ir::join_ir,
        value::{Emit, IrInstruction, ToIr},
    },
    parse::{
        schema_def_parser::{SchemaDefinition, SchemaField},
        type_parser::TypeExpr,
        value_parser::{Assignment, ValueExpr},
    },
    semantics::{type_resolve::TypeEnv, type_resolve2::ValueExprWithType},
};

impl SchemaDefinition {
    fn emit_field(&self, field: &SchemaField, type_env: &mut TypeEnv) -> String {
        let omit_empty = if field.else_branch_value_expr.is_some() {
            ",omitempty"
        } else {
            ""
        };

        let is_duck = matches!(field.type_expr.0, TypeExpr::Duck(_));
        let is_array_of_duck = matches!(&field.type_expr.0, TypeExpr::Array(inner) if matches!(inner.as_ref(), (TypeExpr::Duck(_), ..)));

        if is_duck || is_array_of_duck {
            return format!(
                "F_{0} json.RawMessage `json:\"{0}{1}\"`",
                field.name, omit_empty
            );
        }

        return format!(
            "F_{0} *{1} `json:\"{0}{2}\"`",
            field.name,
            field.type_expr.0.as_go_type_annotation(type_env),
            omit_empty
        );
    }

    pub fn emit(&self, type_env: &mut TypeEnv, to_ir: &mut ToIr) -> IrInstruction {
        let schema_fn_return_type = self.out_type.as_ref().unwrap().0.clone();

        let mut body_instructions = vec![];

        let mut struct_construction_fields = vec![];
        for schema_field in &self.fields {
            struct_construction_fields.push(self.emit_field(schema_field, type_env));
        }

        let struct_construction_src = format!(
            "var ref_struct struct {{ {0} }} = struct {{ {0} }}{{}};",
            struct_construction_fields
                .iter()
                .map(|field_src| field_src.to_string())
                .collect::<Vec<_>>()
                .join("\n"),
        );

        let mut schema_struct_access_srcs = vec![];
        for schema_field in &self.fields {
            let field_name = &schema_field.name;
            let field_type_annotation = if schema_field.else_branch_value_expr.is_some() {
                "any".to_string()
            } else {
                schema_field.type_expr.0.as_go_type_annotation(type_env)
            };

            let null_action_emitted = if let Some(value_expr) = &schema_field.else_branch_value_expr
            {
                ValueExpr::VarAssign(Box::new((
                    Assignment {
                        target: ValueExprWithType::n((
                            ValueExpr::Variable(
                                false,
                                format!("ref_struct.F_{field_name}"),
                                Some(schema_field.type_expr.0.clone()),
                                None,
                                false,
                            ),
                            schema_field.span,
                        )),
                        value_expr: value_expr.clone(),
                    },
                    schema_field.span,
                )))
                .into_empty_span()
                .emit(type_env, to_ir)
            } else {
                ValueExpr::InlineGo(
                    "return Tag__err {}".to_string(),
                    Some((TypeExpr::Never, schema_field.span)),
                )
                .into_empty_span()
                .emit(type_env, to_ir)
            };

            let null_action_src = join_ir(&null_action_emitted.0);

            let src = match &schema_field.type_expr.0 {
                TypeExpr::Duck(_) => {
                    let duck_type_name = schema_field.type_expr.0.as_clean_go_type_name(type_env);
                    format!("
                        var field_{field_name} {field_type_annotation}
                        if len(ref_struct.F_{field_name}) == 0 {{
                             {null_action_src}
                        }}

                        if len(ref_struct.F_{field_name}) > 0 {{
                            var parsed_any any = {duck_type_name}_FromJson(string(ref_struct.F_{field_name}))
                            switch v := parsed_any.(type) {{
                                case Tag__err:
                                    return Tag__err{{}}
                                case {field_type_annotation}:
                                    field_{field_name} = v
                                default:
                                    return Tag__err{{}}
                            }}
                        }}
                    ")
                }
                TypeExpr::Array(inner) if matches!(inner.as_ref(), (TypeExpr::Duck(_), ..)) => {
                    let duck_type_expr = inner.as_ref();
                    let duck_type_name = duck_type_expr.0.as_clean_go_type_name(type_env);
                    let element_type_annotation = duck_type_expr.0.as_go_type_annotation(type_env);

                    format!("
                        var field_{field_name} {field_type_annotation}
                        if len(ref_struct.F_{field_name}) == 0 {{
                             {null_action_src}
                        }}

                        if len(ref_struct.F_{field_name}) > 0 {{
                            var raw_slice []json.RawMessage
                            if err := json.Unmarshal(ref_struct.F_{field_name}, &raw_slice); err != nil {{
                                return Tag__err{{}}
                            }}

                            field_{field_name} = make([]{element_type_annotation}, len(raw_slice))
                            for i, raw_elem := range raw_slice {{
                                var parsed_any any = {duck_type_name}_FromJson(string(raw_elem))
                                switch v := parsed_any.(type) {{
                                    case Tag__err:
                                        return Tag__err{{}}
                                    case {element_type_annotation}:
                                        field_{field_name}[i] = v
                                    default:
                                        return Tag__err{{}}
                                }}
                            }}
                        }}
                    ")
                }
                _ => {
                    format!(
                        "
                        if ref_struct.F_{field_name} == nil {{
                            {null_action_src}
                        }}

                        var field_{field_name} {field_type_annotation} = *ref_struct.F_{field_name}
                    "
                    )
                }
            };

            schema_struct_access_srcs.push(src);

            if let Some((branch, span)) = &schema_field.if_branch {
                let emitted_condition = branch.condition.emit(type_env, to_ir);
                let condition_src = join_ir(&emitted_condition.0);
                let condition_var_src =
                    emitted_condition.1.expect("expect result var").emit_as_go();

                let condition_based_value_emitted = if let Some(value_expr) = &branch.value_expr {
                    ValueExprWithType::n((
                        ValueExpr::Return(Some(Box::new(value_expr.clone()))),
                        *span,
                    ))
                    .emit(type_env, to_ir)
                } else {
                    ValueExprWithType::n((ValueExpr::InlineGo("".to_string(), None), *span))
                        .emit(type_env, to_ir)
                };

                let condition_based_src = join_ir(&condition_based_value_emitted.0);

                let src = format!(
                    "
                    {condition_src}
                    if {condition_var_src} {{
                        {condition_based_src}
                    }}
                "
                );
                schema_struct_access_srcs.push(src);
            }
        }

        let return_duck = ValueExpr::Return(Some(Box::new(ValueExprWithType::n((
            ValueExpr::Duck(
                self.fields
                    .iter()
                    .map(|schema_field| {
                        (
                            schema_field.name.clone(),
                            ValueExprWithType::n((
                                ValueExpr::Variable(
                                    false,
                                    format!("field_{}", schema_field.name),
                                    Some(schema_field.type_expr.0.clone()),
                                    None,
                                    false,
                                ),
                                schema_field.span,
                            )),
                        )
                    })
                    .collect::<Vec<_>>(),
            ),
            self.span,
        )))));

        let emitted_duck = return_duck.into_empty_span().emit(type_env, to_ir);
        let return_duck_src = join_ir(&emitted_duck.0);
        let schema_struct_access_src = schema_struct_access_srcs.join("\n");

        let from_json_body = format!(
            "
                {struct_construction_src}
                err := json.Unmarshal([]byte(str), &ref_struct)
                if err != nil {{
                    fmt.Println(err)
                    return Tag__err{{}}
                }}

                {schema_struct_access_src}
                {return_duck_src}

                return \"\"
            "
        );

        {};

        let instr = IrInstruction::InlineGo(format!(
            "return &{}{{
                    from_json: func(str string) any {{
                        {from_json_body}
                    }},
                }}",
            schema_fn_return_type.as_clean_go_type_name(type_env)
        ));
        body_instructions.push(instr);

        IrInstruction::FunDef(
            self.name.clone(),
            None,
            vec![],
            Some(schema_fn_return_type.as_go_return_type(type_env)),
            body_instructions,
        )
    }
}
