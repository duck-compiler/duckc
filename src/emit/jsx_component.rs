use crate::{
    TypeExpr,
    emit::{types::escape_string_for_go, value::IrInstruction},
    parse::{
        Spanned, failure_with_occurence, jsx_component_parser::JsxComponent, type_parser::Duck,
    },
    semantics::type_resolve::TypeEnv,
};

fn emit_duck_to_js_obj(ty: &Spanned<TypeExpr>, start_path: Vec<String>) -> String {
    fn rec(
        ty: &Spanned<TypeExpr>,
        out_string: &mut String,
        out_params: &mut Vec<String>,
        current_path: Vec<String>,
    ) {
        match &ty.0 {
            TypeExpr::String(..) => {
                out_string.push_str("\"%s\"");
                out_params.push(format!("html.EscapeString({})", current_path.join(".")));
            }
            TypeExpr::Int(..) => {
                out_string.push_str("%v");
                out_params.push(current_path.join(".").to_string());
            }
            TypeExpr::Bool(..) => {
                out_string.push_str("%v");
                out_params.push(current_path.join(".").to_string());
            }
            TypeExpr::Float => {
                out_string.push_str("%v");
                out_params.push(current_path.join(".").to_string());
            }
            TypeExpr::Duck(Duck { fields }) => {
                out_string.push('{');
                for (i, f) in fields.iter().enumerate() {
                    out_string.push_str(&f.name);
                    out_string.push(':');
                    let mut current_path = current_path.clone();
                    current_path.push(format!("Get{}()", f.name));
                    rec(&f.type_expr, out_string, out_params, current_path);
                    if i != fields.len() - 1 {
                        out_string.push(',');
                    }
                }
                out_string.push('}');
            }
            TypeExpr::Array(t) => {
                out_string.push('[');

                out_params.push(match &t.0 {
                    TypeExpr::String(..) => {
                        let go_code = format!(
                            r#"
                                strings.Join(func (s []string) []string {{
                                    res := make([]string, len(s))
                                    for i := range s {{
                                        res[i] = fmt.Sprintf("\"%s\"", html.EscapeString(s[i]))
                                    }}
                                    return res
                                }}({}), ", "),
                            "#,
                            current_path.join(".")
                        );
                        go_code
                    }
                    TypeExpr::Float => {
                        let go_code = format!(
                            r#"
                            strings.Join(func (s []float32) []string {{
                                res := make([]int, len(s))
                                for i := range s {{
                                    res[i] = fmt.Sprintf("%v", s[i])
                                }}
                                return res
                            }}({}), ", "),
                            "#,
                            current_path.join(".")
                        );
                        go_code
                    }
                    TypeExpr::Int(..) => {
                        let go_code = format!(
                            r#"
                            strings.Join(func (s []int) []string {{
                                res := make([]int, len(s))
                                for i := range s {{
                                    res[i] = fmt.Sprintf("%v", s[i])
                                }}
                                return res
                            }}({}), ", "),
                            "#,
                            current_path.join(".")
                        );
                        go_code
                    }
                    TypeExpr::Bool(..) => {
                        let go_code = format!(
                            r#"
                            strings.Join(func (s []bool) []string {{
                                res := make([]int, len(s))
                                for i := range s {{
                                    res[i] = fmt.Sprintf("%v", s[i])
                                }}
                                return res
                            }}({}), ", "),
                            "#,
                            current_path.join(".")
                        );
                        go_code
                    }
                    _ => {
                        let msg = "This type is not compatbile with JavaScript";
                        failure_with_occurence(msg, t.1, [(msg, t.1)]);
                    }
                });
                out_string.push_str("%s");

                out_string.push(']');
            }
            _ => {
                let msg = "This type is not compatbile with JavaScript";
                failure_with_occurence(msg, ty.1, [(msg, ty.1)]);
            }
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

impl JsxComponent {
    fn emit_js(&self) -> String {
        format!(
            "function {}(props){{{}}}",
            self.name, self.javascript_source.0
        )
    }

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

        let emitted_props = emit_duck_to_js_obj(&self.props_type, vec!["props".to_string()]);

        let props = format!("fmt.Sprintf(\"const props = {{...%s,...props2}}\", {emitted_props})");
        let all = format!(
            "fmt.Sprintf(\"{}\\nfunction {}(props2){{\\n%s\\n%s}}\", {props}, \"{}\")",
            escape_string_for_go(
                &type_env
                    .get_component_dependencies(self.name.clone())
                    .client_components
                    .clone()
                    .into_iter()
                    .map(|x| type_env.get_component(x.as_str()).unwrap().emit_js())
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            self.name,
            escape_string_for_go(&self.javascript_source.0)
        );

        IrInstruction::FunDef(
            self.name.clone(),
            None,
            vec![(
                "props".to_string(),
                self.props_type.0.as_go_type_annotation(type_env),
            )],
            Some("Tup_string_string".to_string()),
            vec![IrInstruction::InlineGo(format!(
                "return Tup_string_string {{ field_0: \"{}\", field_1: {all} }}",
                self.name
            ))],
        )
    }
}
