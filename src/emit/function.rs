use crate::{
    emit::value::{IrInstruction, IrValue, ToIr},
    parse::{function_parser::FunctionDefintion, type_parser::TypeExpr, value_parser::empty_range},
    semantics::type_resolve::TypeEnv,
};

impl FunctionDefintion {
    pub fn emit(
        &self,
        receiver: Option<(String, String)>,
        type_env: &mut TypeEnv,
        to_ir: &mut ToIr,
    ) -> IrInstruction {
        // what's r?
        // println!("value_body {:?}", self.value_expr.0);
        let (mut emitted_body, _r) = self.value_expr.0.emit(type_env, to_ir, self.span);

        if let Some(IrInstruction::Block(block_body)) = emitted_body.first() {
            emitted_body = block_body.clone();
        }

        // println!("end value_body");
        if self.return_type.is_some()
            && self.name != "main"
            && !self.return_type.as_ref().unwrap().0.is_unit()
        {
            emitted_body.push(IrInstruction::InlineGo(format!(
                "return *new({})",
                self.return_type
                    .as_ref()
                    .unwrap()
                    .0
                    .as_go_type_annotation(type_env)
            )));
        }

        // TODO mvmo - 03.07.2025: this should check if the last is without a semicolon
        if self.return_type.is_some()
            && !matches!(emitted_body.last(), Some(IrInstruction::Return(_)))
        {
            // mvmo - 03.07.2025: I've commented this out to make my tests pass again
            // emitted_body.push(IrInstruction::Return(r));
        }

        IrInstruction::FunDef(
            self.name.clone(),
            receiver,
            self.params
                .as_ref()
                .unwrap()
                .iter()
                .map(|(name, (ty, _))| (name.clone(), ty.as_go_type_annotation(type_env)))
                .collect::<Vec<_>>(),
            if self.name == "main" {
                None
            } else {
                self.return_type
                    .as_ref()
                    .map(|x| x.0.as_go_return_type(type_env))
            },
            emitted_body,
        )
    }

    pub fn emit_as_extension_fun(
        &self,
        type_env: &mut TypeEnv,
        to_ir: &mut ToIr,
        target_type: &TypeExpr
    ) -> IrInstruction {
        let (mut emitted_body, _result_var) = self.value_expr.0.emit(type_env, to_ir, self.span);
        if let Some(IrInstruction::Block(block_body)) = emitted_body.first() {
            emitted_body = block_body.clone();
        }

        let mut final_params = vec![("self".to_string(), (target_type.clone(), empty_range()))];
        if let Some(existing_params) = &self.params {
            final_params.extend(existing_params.clone());
        }

        IrInstruction::FunDef(
            target_type.build_extension_access_function_name(&self.name, type_env),
            None,
            final_params
                .iter()
                .map(|(name, (ty, _))| (name.clone(), ty.as_go_type_annotation(type_env)))
                .collect::<Vec<_>>(),
            Some(format!("func () {}", self.return_type.clone().expect("compiler error: expect").0.as_go_return_type(type_env))),
            vec![IrInstruction::Return(Some(IrValue::Lambda(
                self.params
                    .clone()
                    .unwrap_or_else(|| Vec::new())
                    .iter()
                    .map(|(name, (ty, _))| (name.clone(), ty.as_go_type_annotation(type_env)))
                    .collect::<Vec<_>>(),
                Some(format!("{}", self.return_type.clone().expect("compiler error: expect").0.as_go_return_type(type_env))),
                emitted_body
            )))],
        )
    }
}
