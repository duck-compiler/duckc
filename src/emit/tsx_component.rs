use crate::{
    TypeExpr,
    emit::{types::escape_string_for_go, value::IrInstruction},
    parse::{component_parser::TsxComponent, type_parser::Duck},
    semantics::type_resolve::TypeEnv,
};

fn emit_duck_to_js_obj(ty: &TypeExpr, start_path: Vec<String>) -> String {
    fn rec(
        ty: &TypeExpr,
        out_string: &mut String,
        out_params: &mut Vec<String>,
        current_path: Vec<String>,
    ) {
        match ty {
            TypeExpr::String => {
                out_string.push_str("\"%s\"");
                out_params.push(format!(
                    "html.EscapeString({}.as_dgo_string())",
                    current_path.join(".")
                ));
            }
            TypeExpr::Int => {
                out_string.push_str("%v");
                out_params.push(format!("{}.as_dgo_int()", current_path.join(".")));
            }
            TypeExpr::Bool => {
                out_string.push_str("%v");
                out_params.push(format!("{}.as_dgo_bool()", current_path.join(".")));
            }
            TypeExpr::Float => {
                out_string.push_str("%v");
                out_params.push(format!("{}.as_dgo_float()", current_path.join(".")));
            }
            TypeExpr::Duck(Duck { fields }) => {
                out_string.push('{');
                for (i, f) in fields.iter().enumerate() {
                    out_string.push_str(&f.name);
                    out_string.push(':');
                    let mut current_path = current_path.clone();
                    current_path.push(format!("Get{}()", f.name));
                    rec(&f.type_expr.0, out_string, out_params, current_path);
                    if i != fields.len() - 1 {
                        out_string.push(',');
                    }
                }
                out_string.push('}');
            }
            TypeExpr::Array(t) => {
                out_string.push('[');

                out_params.push(match &t.0 {
                    TypeExpr::String => {
                        let go_code = format!(r#"
                                strings.Join(func (s []DuckString) []string {{
                                    res := make([]string, len(s))
                                    for i := range s {{
                                        res[i] = fmt.Sprintf("\"%s\"", html.EscapeString(s[i].as_dgo_string()))
                                    }}
                                    return res
                                }}({}), ", "),
                            "#, current_path.join("."));
                        go_code
                    }
                    TypeExpr::Float => {
                        let go_code = format!(r#"
                            strings.Join(func (s []DuckFloat) []string {{
                                res := make([]int, len(s))
                                for i := range s {{
                                    res[i] = fmt.Sprintf("%v", s[i].as_dgo_float())
                                }}
                                return res
                            }}({}), ", "),
                            "#, current_path.join("."));
                        go_code
                    }
                    TypeExpr::Int => {
                        let go_code = format!(r#"
                            strings.Join(func (s []DuckInt) []string {{
                                res := make([]int, len(s))
                                for i := range s {{
                                    res[i] = fmt.Sprintf("%v", s[i].as_dgo_int())
                                }}
                                return res
                            }}({}), ", "),
                            "#, current_path.join("."));
                        go_code
                    }
                    TypeExpr::Bool => {
                        let go_code = format!(r#"
                            strings.Join(func (s []DuckBool) []string {{
                                res := make([]int, len(s))
                                for i := range s {{
                                    res[i] = fmt.Sprintf("%v", s[i].as_dgo_bool())
                                }}
                                return res
                            }}({}), ", "),
                            "#, current_path.join("."));
                        go_code
                    }
                    _ => panic!("not compatible with js"),
                });
                out_string.push_str("%s");

                out_string.push(']');
            }
            _ => panic!("not compatible with js {ty:?}"),
        }
    }
    let mut s = String::new();
    let mut p = Vec::new();
    rec(ty, &mut s, &mut p, start_path);
    format!(
        "fmt.Sprintf(\"{}\", {})",
        escape_string_for_go(&s),
        p.join(", ")
    )
}

impl TsxComponent {
    pub fn emit(&self, type_env: &mut TypeEnv) -> IrInstruction {
        // let final_go_str = IrInstruction::InlineGo(format!(
        //     "return fmt.Sprintf(\"function {}(props){{{}\\n{}\\n}}\", {})",
        //     self.name,
        //     extracted_vars
        //         .iter()
        //         .map(|_| "%s")
        //         .collect::<Vec<_>>()
        //         .join("\\n"),
        //     escape_string_for_go(self.typescript_source.0.as_str()),
        //     extracted_vars
        //         .iter()
        //         .map(|x| x.0.as_str())
        //         .collect::<Vec<_>>()
        //         .join(", ")
        // ));

        // let body = extracted_vars
        //     .into_iter()
        //     .map(|x| x.1)
        //     .chain([final_go_str].into_iter())
        //     .collect();

        let emitted_props = emit_duck_to_js_obj(&self.props_type.0, vec!["props".to_string()]);

        let props = format!(
            "fmt.Sprintf(\"const props = {{...%s,...props2}}\", {})",
            emitted_props
        );
        let all = format!(
            "fmt.Sprintf(\"function {}(props2){{\\n%s\\n%s}}\", {props}, \"{}\")",
            self.name,
            escape_string_for_go(&self.typescript_source.0)
        );

        println!("{emitted_props}");
        println!("{props}");
        println!("{all}");

        let ir_def = IrInstruction::FunDef(
            self.name.clone(),
            None,
            vec![(
                "props".to_string(),
                self.props_type.0.as_go_type_annotation(type_env),
            )],
            Some("Tup_DuckString_DuckString".to_string()),
            vec![IrInstruction::InlineGo(format!("return Tup_DuckString_DuckString {{ field_0: ConcDuckString {{ value: \"{}\" }}, field_1: ConcDuckString {{ value: {all} }} }}", self.name))],
        );
        ir_def
    }
}
