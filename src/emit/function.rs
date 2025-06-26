use crate::{
    emit::value::{IrInstruction, ToIr},
    parse::function_parser::FunctionDefintion,
    semantics::typechecker::TypeEnv,
};

impl FunctionDefintion {
    pub fn emit(&self, type_env: &mut TypeEnv, to_ir: &mut ToIr) -> IrInstruction {
        let (emitted_body, _) = self.value_expr.0.emit(type_env, to_ir);

        IrInstruction::FunDef(
            self.name.clone(),
            None,
            self.params
                .as_ref()
                .unwrap()
                .iter()
                .map(|(name, (ty, _))| {
                    (name.clone(), ty.as_go_type_annotation(type_env))
                }).
                collect::<Vec<_>>(),
            self.return_type.as_ref().map(|x| x.0.as_go_type_annotation(type_env)),
            emitted_body,
        )
    }
}
