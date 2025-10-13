use std::collections::HashSet;

use crate::{
    emit::{
        types::emit_type_definitions,
        value::{IrInstruction, ToIr},
    },
    parse::{SS, source_file_parser::SourceFile, use_statement_parser::UseStatement},
    semantics::type_resolve::TypeEnv,
};

impl SourceFile {
    pub fn emit(self, pkg_name: String, type_env: &mut TypeEnv, span: SS) -> Vec<IrInstruction> {
        let mut to_ir = ToIr::default();

        let type_definitions = emit_type_definitions(type_env, &mut to_ir);

        let mut instructions = Vec::new();
        instructions.push(IrInstruction::GoPackage(pkg_name));

        let mut go_imports = vec![];

        for use_statement in self.use_statements {
            if let UseStatement::Go(name, alias) = use_statement {
                go_imports.push((alias, name));
            }
        }

        instructions.push(IrInstruction::GoImports(go_imports));

        let mut emitted = HashSet::new();

        for function_definition in self.function_definitions {
            // generic functions shouldn't be emitted, as they have incomplete type information
            if function_definition.generics.is_some() {
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

        instructions.push(IrInstruction::FunDef(
            "emit_go_to_js".to_string(),
            None,
            vec![("target".to_string(), "any".to_string())],
            Some("string".to_string()),
            vec![IrInstruction::InlineGo(
                r#"
	switch target.(type) {
	case string:
		s := target.(string)
		return fmt.Sprintf("\"%s\"", s)
	case int:
		s := target.(int)
		return fmt.Sprintf("%d", s)
	case float32:
		s := target.(float32)
		return fmt.Sprintf("%f", s)
	case float64:
		s := target.(float64)
		return fmt.Sprintf("%f", s)
	case bool:
		s := target.(bool)
		if s {
			return "true"
		} else {
			return "false"
		}
	case string:
		s := target.(string)
		return fmt.Sprintf("\"%s\"", s)
	case int:
		s := target.(int)
		return fmt.Sprintf("%d", s)
	case float32:
		s := target.(float32)
		return fmt.Sprintf("%f", s)
	case bool:
		s := target.(bool)
		if s {
			return "true"
		} else {
			return "false"
		}
	default:
		v := reflect.ValueOf(target)
		if v.Kind() == reflect.Array || v.Kind() == reflect.Slice {
			arr_str := "["
			l := v.Len()
			for i := 0; i < l; i++ {
				emitted := emit_go_to_js(v.Index(i).Interface())
				arr_str += emitted
				if i < l-1 {
					arr_str += ","
				}
			}
			arr_str += "]"
			return arr_str
		} else if v.Kind() == reflect.Pointer {
			obj_str := "{"
			v := v.Elem()
			l := v.NumField()

			for i := 0; i < l; i++ {
				field_name := v.Type().Field(i).Name
				v_field := v.Field(i)
				field_ptr := unsafe.Pointer(v_field.UnsafeAddr())
				field_value := reflect.NewAt(v_field.Type(), field_ptr).Elem().Interface()
				obj_str += fmt.Sprintf("%s:%s", field_name, emit_go_to_js(field_value))
				if i < l-1 {
					obj_str += ","
				}
			}
			obj_str += "}"
			return obj_str
			} else if v.Kind() == reflect.Struct {
				t := v.Type()

				if strings.HasPrefix(t.Name(), "Const") {
					as_str := fmt.Sprintf("%s", target)
					sliced := as_str[1:len(as_str)-1]
					if strings.Contains(t.Name(), "String") {
						return fmt.Sprintf("\"%s\"", sliced)
					} else {
						return fmt.Sprintf("%v", sliced)
					}
				}
			}
	}
	panic(fmt.Sprintf("can't emit %v", target))
                    "#
                .to_string(),
            )],
        ));

        for c in self.tsx_components {
            instructions.push(c.emit(type_env));
        }

        for c in self.duckx_components {
            instructions.push(c.emit(type_env, &mut to_ir, span));
        }

        instructions.extend(type_definitions);

        instructions
    }
}
