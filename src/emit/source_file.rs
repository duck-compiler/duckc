use crate::{
    emit::{
        types::emit_type_definitions,
        value::{IrInstruction, ToIr},
    },
    parse::{source_file_parser::SourceFile, use_statement_parser::UseStatement}, semantics::type_resolve::TypeEnv,
};

impl SourceFile {
    pub fn emit(self, pkg_name: String, type_env: &mut TypeEnv) -> Vec<IrInstruction> {
        let type_definitions = emit_type_definitions(type_env);

        let mut instructions = Vec::new();
        instructions.push(IrInstruction::GoPackage(pkg_name));

        let mut go_imports = Vec::new();
        for u in self.use_statements {
            if let UseStatement::Go(name, alias) = u {
                go_imports.push((alias, name));
            }
        }
        instructions.push(IrInstruction::GoImports(go_imports));

        let mut to_ir = ToIr::default();

        for f in self.function_definitions {
            instructions.push(f.emit(type_env, &mut to_ir));
        }

        instructions.extend(type_definitions);

        instructions
    }
}
