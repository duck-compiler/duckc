use crate::{
    emit::{
        ir::join_ir,
        value::{IrInstruction, ToIr},
    },
    parse::{
        schema_def_parser::{SchemaDefinition, SchemaField},
        value_parser::{Assignment, ValueExpr},
    },
    semantics::type_resolve::TypeEnv,
};

/*
 * var result struct {
 *     username: string,
 * } = struct{}{
 *     username: string,
 * }
 */
impl SchemaDefinition {
    fn emit_field(&self, field: &SchemaField, type_env: &mut TypeEnv) -> String {
        let omit_empty = if field.else_branch_value_expr.is_some() {
            ",omitempty"
        } else {
            ""
        };

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
                .join(","),
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
                ValueExpr::InlineGo("return Tag__err {}".to_string()).emit(
                    type_env,
                    to_ir,
                    schema_field.span,
                )
            };

            let null_action_src = join_ir(&null_action_emitted.0);

            let src = format!(
                "
                if ref_struct.F_{field_name} == nil {{
                    {null_action_src}
                }}

                var field_{field_name} {field_type_annotation} = *ref_struct.F_{field_name}
                ",
            );

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
                    ValueExpr::InlineGo("".to_string()).emit(type_env, to_ir, *span)
                };

                let condition_based_src = join_ir(&condition_based_value_emitted.0);

                let src = format!(
                    "
                    {condition_src}
                    if field_{field_name} != nil && {condition_var_src} {{
                        {condition_based_src}
                    }}
                ",
                );

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
}
