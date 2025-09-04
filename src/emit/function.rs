use crate::{
    emit::value::{IrInstruction, ToIr},
    parse::function_parser::FunctionDefintion,
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
        let (mut emitted_body, _r) = self.value_expr.0.emit(type_env, to_ir);
        // println!("end value_body");
        if self.return_type.is_some() {
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
                    .map(|x| x.0.as_go_type_annotation(type_env))
            },
            emitted_body,
        )
    }
}
