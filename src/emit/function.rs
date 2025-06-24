use crate::{
    emit::value::{EmitEnvironment, emit},
    parse::function_parser::FunctionDefintion,
    semantics::typechecker::TypeEnv,
};

impl FunctionDefintion {
    pub fn emit(&self, env: EmitEnvironment, type_env: &mut TypeEnv) -> String {
        let (mut emitted_body, res_name) = emit(self.value_expr.clone().0, env.clone(), type_env);

        // TODO: remove this as this should be located in the typechecking
        if let Some(params) = &self.params {
            for p in params {
                p.1.to_go_type_str(type_env);
            }
        }

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
                        .map(|x| format!("{} {}", x.0, x.1.emit().0))
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
