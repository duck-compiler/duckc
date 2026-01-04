use std::collections::HashSet;

use crate::{
    emit::{
        ir::fix_idents_in_ir,
        types::emit_type_definitions,
        value::{IrInstruction, IrValue, ToIr},
    },
    parse::{SS, source_file_parser::SourceFile, use_statement_parser::UseStatement},
    semantics::type_resolve::TypeEnv,
};

const JSON_UTILITIES: &'static str = include_str!("json_util.go");

impl SourceFile {
    pub fn emit(
        mut self,
        pkg_name: String,
        type_env: &mut TypeEnv,
        span: SS,
    ) -> Vec<IrInstruction> {
        let mut to_ir = ToIr::default();

        let mut go_imports = vec![];
        go_imports.push((None, "hash/maphash".to_string()));
        go_imports.push((None, "errors".to_string()));

        let mut imports = HashSet::new();

        for use_statement in &self.use_statements {
            if let UseStatement::Go(name, alias) = use_statement {
                let import_name = alias
                    .as_ref()
                    .cloned()
                    .unwrap_or_else(|| name.split("/").last().unwrap().to_string());

                if !import_name.is_empty() && import_name != "." && import_name != "_" {
                    imports.insert(import_name.clone());
                }

                go_imports.push((alias.to_owned(), name.to_owned()));
            }
        }

        let imports = Box::leak(Box::new(imports)) as &'static HashSet<String>;
        type_env.all_go_imports = imports;

        let type_definitions = emit_type_definitions(type_env, &mut to_ir, &self);

        let mut instructions = Vec::new();
        instructions.push(IrInstruction::GoPackage(pkg_name));

        instructions.push(IrInstruction::GoImports(go_imports));

        for global_var in &self.global_var_decls {
            let (mut init_code, Some(IrValue::Var(res_name) | IrValue::Imm(res_name))) =
                global_var.initializer.0.emit(type_env, &mut to_ir, span)
            else {
                panic!(
                    "Compiler Bug: need a var (global declaration {} {:?})",
                    global_var.name, global_var.initializer.0
                )
            };
            let go_type = global_var.type_expr.0.as_go_type_annotation(type_env);
            init_code.push(IrInstruction::Return(Some(IrValue::Var(res_name))));
            instructions.push(IrInstruction::GlobalVarDecl {
                name: global_var.name.clone(),
                go_type,
                init_code,
            });
        }

        let mut emitted = HashSet::new();

        for function_definition in self
            .function_definitions
            .iter_mut()
            .chain(type_env.generic_fns_generated.clone().iter_mut())
        {
            // generic functions shouldn't be emitted, as they have incomplete type information
            if !function_definition.generics.is_empty() {
                continue;
            }

            if emitted.insert(function_definition.name.clone()) {
                let mut fn_instr = function_definition.emit(None, type_env, &mut to_ir);

                if function_definition.name.as_str() == "main" {
                    let IrInstruction::FunDef(_, _, _, _, _body) = &mut fn_instr else {
                        panic!("how")
                    };
                }
                instructions.push(fn_instr);
            }
        }

        let mut emitted = HashSet::new();
        for schema_def in self.schema_defs {
            if emitted.insert(schema_def.name.clone()) {
                let fn_instr = schema_def.emit(type_env, &mut to_ir);
                instructions.push(fn_instr);
            }
        }

        instructions.push(IrInstruction::StructDef(
            "RenderCall".to_string(),
            vec![
                ("Jsx".to_string(), "string".to_string()),
                ("Id".to_string(), "string".to_string()),
            ],
        ));

        instructions.push(IrInstruction::StructDef(
            "TemplEnv".to_string(),
            vec![
                ("ClientComponents".to_string(), "[]string".to_string()),
                ("RenderCalls".to_string(), "[]RenderCall".to_string()),
            ],
        ));

        instructions.push(IrInstruction::FunDef(
            "push_client_component".to_string(),
            Some(("self".to_string(), ("*TemplEnv".to_string()))),
            vec![("comp".to_string(), "string".to_string())],
            None,
            vec![IrInstruction::InlineGo(
                r#"
                    if !slices.Contains(self.ClientComponents, comp) {
                        self.ClientComponents = append(self.ClientComponents, comp)
                    }
                "#
                .to_string(),
            )],
        ));

        instructions.push(IrInstruction::FunDef(
            "push_render".to_string(),
            Some(("self".to_string(), ("*TemplEnv".to_string()))),
            vec![
                ("js".to_string(), "string".to_string()),
                ("id".to_string(), "string".to_string()),
            ],
            None,
            vec![IrInstruction::InlineGo(
                r#"
                    for _, e := range self.RenderCalls {
		if e.Id == id {
			return
		}
	}

	self.RenderCalls = append(self.RenderCalls, RenderCall{js, id})
                    "#
                .to_string(),
            )],
        ));

        for c in self.jsx_compontents {
            instructions.push(c.emit(type_env));
        }

        for c in self.duckx_components {
            instructions.push(c.emit(type_env, &mut to_ir, span));
        }

        for extensions_def in self.extensions_defs {
            for fn_def in extensions_def.function_definitions {
                instructions.push(fn_def.0.emit_as_extension_fun(
                    type_env,
                    &mut to_ir,
                    &extensions_def.target_type_expr.0,
                ))
            }
        }

        instructions.extend(type_definitions);
        for i in &mut instructions {
            fix_idents_in_ir(i, imports);
        }
        instructions.push(IrInstruction::InlineGo(JSON_UTILITIES.to_string()));
        instructions
    }
}
