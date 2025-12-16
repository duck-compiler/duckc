use crate::{
    emit::{
        ir::join_ir,
        value::{IrInstruction, ToIr},
    },
    parse::{
        schema_def_parser::{SchemaDefinition, SchemaField},
        type_parser::{TypeExpr},
        value_parser::{Assignment, ValueExpr, empty_range},
    },
    semantics::type_resolve::TypeEnv,
};

/*
 * var result struct {
 * username: string,
 * } = struct{}{
 * username: string,
 * }
 */
impl SchemaDefinition {
    fn emit_field(&self, field: &SchemaField, type_env: &mut TypeEnv) -> String {
        let omit_empty = if field.else_branch_value_expr.is_some() {
            ",omitempty"
        } else {
            ""
        };

        if let TypeExpr::Duck(_) = field.type_expr.0 {
            return format!(
                "F_{0} json.RawMessage `json:\"{0}{1}\"`",
                field.name,
                omit_empty
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
        // let mut validation_srcs = vec![];
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
                        target: (
                            ValueExpr::Variable(
                                false,
                                format!("ref_struct.F_{field_name}"),
                                Some(schema_field.type_expr.0.clone()),
                                None,
                                false,
                            ),
                            schema_field.span,
                        ),
                        value_expr: value_expr.clone(),
                    },
                    schema_field.span,
                )))
                .emit(type_env, to_ir, schema_field.span)
            } else {
                ValueExpr::InlineGo(
                    "return Tag__err {}".to_string(),
                    Some((TypeExpr::Never, schema_field.span)),
                )
                .emit(type_env, to_ir, schema_field.span)
            };

            let null_action_src = join_ir(&null_action_emitted.0);

            let src = if let TypeExpr::Duck(_) = schema_field.type_expr.0 {
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
            } else {
                format!("
                    if ref_struct.F_{field_name} == nil {{
                        {null_action_src}
                    }}

                    var field_{field_name} {field_type_annotation} = *ref_struct.F_{field_name}
                ")
            };

            schema_struct_access_srcs.push(src);

            if let Some((branch, span)) = &schema_field.if_branch {
                let emitted_condition = branch.condition.0.emit(type_env, to_ir, *span);
                let condition_src = join_ir(&emitted_condition.0);

                let condition_var_src =
                    emitted_condition.1.expect("expect result var").emit_as_go();

                let condition_based_value_emitted = if let Some(value_expr) = &branch.value_expr {
                    ValueExpr::Return(Some(Box::new(value_expr.clone())))
                        .emit(type_env, to_ir, *span)
                } else {
                    ValueExpr::InlineGo("".to_string(), None).emit(type_env, to_ir, *span)
                };

                let condition_based_src = join_ir(&condition_based_value_emitted.0);

                let src = format!("
                    {condition_src}
                    if {condition_var_src} {{
                        {condition_based_src}
                    }}
                ");

                schema_struct_access_srcs.push(src);
            }
        }

        let return_duck = ValueExpr::Return(Some(Box::new((
            ValueExpr::Duck(
                self.fields
                    .iter()
                    .map(|schema_field| {
                        (
                            schema_field.name.clone(),
                            (
                                ValueExpr::Variable(
                                    false,
                                    format!("field_{}", schema_field.name),
                                    Some(schema_field.type_expr.0.clone()),
                                    None,
                                    false,
                                ),
                                schema_field.span,
                            ),
                        )
                    })
                    .collect::<Vec<_>>(),
            ),
            self.span,
        ))));

        let emitted_duck = return_duck.emit(type_env, to_ir, self.span);
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

    pub fn emit_from_json_fn_from_duck(duck_type_expr: &TypeExpr, type_env: &mut TypeEnv, to_ir: &mut ToIr) -> IrInstruction {
        let TypeExpr::Duck(duck) = &duck_type_expr else { panic!("should only be called with TypeExpr::Duck") };
        let sd = SchemaDefinition {
            name: duck_type_expr.as_go_type_annotation(type_env),
            fields: duck.fields.iter().map(|field| SchemaField {
                name: field.name.clone(),
                type_expr: field.type_expr.clone(),
                if_branch: None,
                else_branch_value_expr: None,
                span: empty_range()
            }).collect::<Vec<_>>(),
            comments: vec![],
            out_type: Some((TypeExpr::Or(vec![(TypeExpr::Duck(duck.clone()), empty_range()), (TypeExpr::Tag("err".to_string()), empty_range())]), empty_range())),
            schema_fn_type: Some((TypeExpr::Fun(vec![], Box::new((TypeExpr::String(None), empty_range())), false), empty_range())),
            span: empty_range()
        };

        let schema_fn_return_type = sd.out_type.as_ref().unwrap().0.clone();

        let mut body_instructions = vec![];

        let mut struct_construction_fields = vec![];
        for schema_field in &sd.fields {
            struct_construction_fields.push(sd.emit_field(schema_field, type_env));
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
        // let mut validation_srcs = vec![];
        for schema_field in &sd.fields {
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
                        target: (
                            ValueExpr::Variable(
                                false,
                                format!("ref_struct.F_{field_name}"),
                                Some(schema_field.type_expr.0.clone()),
                                None,
                                false,
                            ),
                            schema_field.span,
                        ),
                        value_expr: value_expr.clone(),
                    },
                    schema_field.span,
                )))
                .emit(type_env, to_ir, schema_field.span)
            } else {
                ValueExpr::InlineGo(
                    "return Tag__err {}".to_string(),
                    Some((TypeExpr::Never, schema_field.span)),
                )
                .emit(type_env, to_ir, schema_field.span)
            };

            let null_action_src = join_ir(&null_action_emitted.0);

            let src = if let TypeExpr::Duck(_) = schema_field.type_expr.0 {
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
            } else {
                format!("
                    if ref_struct.F_{field_name} == nil {{
                        {null_action_src}
                    }}

                    var field_{field_name} {field_type_annotation} = *ref_struct.F_{field_name}
                ")
            };

            schema_struct_access_srcs.push(src);

            if let Some((branch, span)) = &schema_field.if_branch {
                let emitted_condition = branch.condition.0.emit(type_env, to_ir, *span);
                let condition_src = join_ir(&emitted_condition.0);

                let condition_var_src =
                    emitted_condition.1.expect("expect result var").emit_as_go();

                let condition_based_value_emitted = if let Some(value_expr) = &branch.value_expr {
                    ValueExpr::Return(Some(Box::new(value_expr.clone())))
                        .emit(type_env, to_ir, *span)
                } else {
                    ValueExpr::InlineGo("".to_string(), None).emit(type_env, to_ir, *span)
                };

                let condition_based_src = join_ir(&condition_based_value_emitted.0);

                let src = format!(
                    "
                    {condition_src}
                    if {condition_var_src} {{
                        {condition_based_src}
                    }}
                ",
                );

                schema_struct_access_srcs.push(src);
            }
        }

        let return_duck = ValueExpr::Return(Some(Box::new((
            ValueExpr::Duck(
                sd.fields
                    .iter()
                    .map(|schema_field| {
                        (
                            schema_field.name.clone(),
                            (
                                ValueExpr::Variable(
                                    false,
                                    format!("field_{}", schema_field.name),
                                    Some(schema_field.type_expr.0.clone()),
                                    None,
                                    false,
                                ),
                                schema_field.span,
                            ),
                        )
                    })
                    .collect::<Vec<_>>(),
            ),
            sd.span,
        ))));

        let emitted_duck = return_duck.emit(type_env, to_ir, sd.span);
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
            "{from_json_body}",
        ));
        body_instructions.push(instr);

        IrInstruction::FunDef(
            format!("{}_FromJson", duck_type_expr.as_clean_go_type_name(type_env)),
            None,
            vec![("str".into(), "string".into())],
            Some(schema_fn_return_type.as_go_return_type(type_env)),
            body_instructions,
        )
    }
}
