use crate::{
    emit::value::{EmitEnvironment, emit},
    parse::function_parser::FunctionDefintion,
    semantics::typechecker::TypeEnv,
};

impl FunctionDefintion {
    pub fn emit(&self, env: EmitEnvironment, type_env: &mut TypeEnv) -> String {
        let (mut emitted_body, res_name) = emit(self.value_expr.clone().0, env.clone(), type_env);

        if let Some(res_name) = res_name {
            emitted_body.push(format!("_ = {res_name}\n"));
        }
        let body = emitted_body.join("");
        [
            format!(
                "func {}({}) {} {}\n",
                self.name,
                self.params
                    .as_ref()
                    .map(|x| x
                        .iter()
                        .map(|(name, type_expr)| format!("{} {}", name, type_expr.as_go_type_annotation(type_env)))
                        .collect::<Vec<_>>()
                        .join(", "))
                    .unwrap_or_default(),
                self.return_type
                    .as_ref()
                    .map(|x| x.emit().0)
                    .unwrap_or_default(),
                "{"
            ),
            body,
            "}\n".to_string(),
        ]
        .join("")
    }
}
