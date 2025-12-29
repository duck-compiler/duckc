use crate::{
    emit::{types::escape_string_for_go, value::IrInstruction},
    parse::jsx_component_parser::JsxComponent,
    semantics::type_resolve::TypeEnv,
};

impl JsxComponent {
    fn emit_js(&self) -> String {
        format!(
            "function {}(props){{{}}}",
            self.name, self.javascript_source.0
        )
    }

    pub fn emit(&self, type_env: &mut TypeEnv) -> IrInstruction {
        let props = escape_string_for_go(&self.props_type.0.call_to_json("props", type_env));
        let all = format!(
            "fmt.Sprintf(\"{}\\nfunction {}(props){{\\nprops = (%s)\\n%s}}\", {props}, \"{}\")",
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
