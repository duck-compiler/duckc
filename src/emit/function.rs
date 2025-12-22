use crate::{
    emit::value::{IrInstruction, IrValue, ToIr},
    parse::{
        failure_with_occurence,
        function_parser::FunctionDefintion,
        type_parser::TypeExpr,
        value_parser::{ValueExpr, empty_range},
    },
    semantics::type_resolve::TypeEnv,
};

pub fn function_epilogue_2(t: &str) -> IrInstruction {
    if t == "Tup_" {
        IrInstruction::InlineGo("return Tup_{}".to_string())
    } else {
        IrInstruction::InlineGo(format!("var ΔΔΔretΔΔΔ *{t}\nreturn *ΔΔΔretΔΔΔ"))
    }
}

pub fn function_epilogue(t: &TypeExpr, type_env: &mut TypeEnv) -> IrInstruction {
    let t = t.as_go_type_annotation(type_env);

    function_epilogue_2(&t)
}

impl FunctionDefintion {
    pub fn emit(
        &self,
        receiver: Option<(String, String)>,
        type_env: &mut TypeEnv,
        to_ir: &mut ToIr,
    ) -> IrInstruction {
        let ValueExpr::Return(Some(what)) = &self.value_expr.0 else {
            panic!(
                "Compiler Bug: every function needs to return something {} {:?}",
                self.name, self.value_expr.0
            )
        };

        let (mut emitted_body, result_ir_value) = what.0.emit(type_env, to_ir, self.span);

        if let Some(result) = result_ir_value {
            emitted_body.push(IrInstruction::Return(Some(result)));
        }

        if self.name != "main" {
            emitted_body.push(function_epilogue(&self.return_type.0, type_env));
        } else {
            if !self.return_type.0.is_unit() {
                let msg = "Main must not have a return type";
                failure_with_occurence(msg, self.span, [(msg, self.span)]);
            }

            let wrapped_in_lambda = IrValue::Lambda(
                vec![],
                Some(self.return_type.0.as_go_return_type(type_env)),
                emitted_body,
            );
            emitted_body = vec![IrInstruction::FunCall(None, wrapped_in_lambda, vec![])];
        }

        IrInstruction::FunDef(
            self.name.clone(),
            receiver,
            self.params
                .iter()
                .map(|(name, (ty, _))| (name.clone(), ty.as_go_type_annotation(type_env)))
                .collect::<Vec<_>>(),
            if self.name == "main" {
                None
            } else {
                Some(self.return_type.0.as_go_return_type(type_env))
            },
            emitted_body,
        )
    }

    pub fn emit_as_extension_fun(
        &self,
        type_env: &mut TypeEnv,
        to_ir: &mut ToIr,
        target_type: &TypeExpr,
    ) -> IrInstruction {
        dbg!(&self.name);
        let (emitted_body, _result_var) = self.value_expr.0.emit(type_env, to_ir, self.span);

        let mut final_params = vec![("self".to_string(), (target_type.clone(), empty_range()))];
        final_params.extend_from_slice(&self.params);

        IrInstruction::FunDef(
            target_type.build_extension_access_function_name(&self.name, type_env),
            None,
            final_params
                .first()
                .iter()
                .map(|(name, (ty, _))| (name.clone(), ty.as_go_type_annotation(type_env)))
                .collect::<Vec<_>>(),
            Some(format!(
                "func ({}) {}",
                final_params
                    .iter()
                    .filter(|(name, ..)| name != "self")
                    .map(|(_name, (ty, _))| ty.as_go_type_annotation(type_env))
                    .collect::<Vec<_>>()
                    .join(","),
                self.return_type.0.as_go_return_type(type_env),
            )),
            vec![IrInstruction::Return(Some(IrValue::Lambda(
                self.params
                    .iter()
                    .map(|(name, (ty, _))| (name.clone(), ty.as_go_type_annotation(type_env)))
                    .collect::<Vec<_>>(),
                Some(self.return_type.0.as_go_return_type(type_env)),
                emitted_body,
            )))],
        )
    }
}
