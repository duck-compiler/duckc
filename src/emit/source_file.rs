use crate::{
    emit::{types::emit_type_definitions, value::EmitEnvironment},
    parse::{source_file_parser::SourceFile, use_statement_parser::UseStatement},
    semantics::typechecker::TypeEnv,
};

impl SourceFile {
    pub fn emit(mut self, pkg_name: String, type_env: &mut TypeEnv) -> String {
        let emit_env = EmitEnvironment::new();

        emit_env.emit_imports_and_types();

        let type_definitions = emit_type_definitions(type_env);

        let functions = self
            .function_definitions
            .iter()
            .map(|x| format!("\n{}\n", x.emit(emit_env.clone(), type_env)))
            .collect::<Vec<_>>()
            .join("");

        for i in emit_env.imports.borrow().clone().into_iter() {
            self.push_use(&UseStatement::Go(i.path, i.alias));
        }

        let go_imports = self
            .use_statements
            .iter()
            .filter_map(|x| match x {
                UseStatement::Go(go_mod, alias) => Some(format!(
                    "{} \"{go_mod}\"\n",
                    alias.clone().unwrap_or_default()
                )),
                _ => None,
            })
            .collect::<Vec<_>>()
            .join("");

        format!(
            "package {pkg_name}\n\nimport (\n{go_imports}\n)\n{type_definitions}\n\n{functions}",
        )
    }
}
