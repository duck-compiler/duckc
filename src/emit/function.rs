use crate::{
    emit::value::{IrInstruction, ToIr},
    parse::function_parser::FunctionDefintion,
    semantics::typechecker::TypeEnv,
};

impl FunctionDefintion {
    pub fn emit(&self, type_env: &mut TypeEnv, to_ir: &mut ToIr) -> IrInstruction {
        let (mut emitted_body, r) = self.value_expr.0.emit(type_env, to_ir);

        if self.return_type.is_some()
            && !matches!(emitted_body.last(), Some(IrInstruction::Return(_)))
        {
            emitted_body.push(IrInstruction::Return(r));
        }

        IrInstruction::FunDef(
            self.name.clone(),
            None,
            self.params
                .as_ref()
                .unwrap()
                .iter()
                .map(|(name, (ty, _))| (name.clone(), ty.as_go_type_annotation(type_env)))
                .collect::<Vec<_>>(),
            if self.name == "main" { None } else {
                self.return_type
                    .as_ref()
                    .map(|x| x.0.as_go_type_annotation(type_env))
            },
            emitted_body,
        )
    }
}
