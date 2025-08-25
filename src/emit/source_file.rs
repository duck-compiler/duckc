use crate::{
    emit::{
        types::emit_type_definitions,
        value::{IrInstruction, ToIr},
    },
    parse::{source_file_parser::SourceFile, use_statement_parser::UseStatement},
    semantics::type_resolve::TypeEnv,
};

impl SourceFile {
    pub fn emit(self, pkg_name: String, type_env: &mut TypeEnv) -> Vec<IrInstruction> {
        let mut to_ir = ToIr::default();

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


        let mut generic_function_defs = type_env.generic_fns_generated.clone();
        for f in generic_function_defs.iter_mut() {
            instructions.push(f.1.0.emit(None, type_env, &mut to_ir));
        }

        for f in self.function_definitions {
            // generic functions shouldn't be emitted, as they have incomplete type information
            if f.generics.is_some() {
                continue;
            }
            instructions.push(f.emit(None, type_env, &mut to_ir));
        }

         for s in self.struct_definitions {
             for method in &s.methods {
                 instructions.push(method.emit(Some(("self".to_string(), format!("*{}", s.name))), type_env, &mut to_ir));
             }
         }

        instructions.extend(type_definitions);

        instructions
    }
}
