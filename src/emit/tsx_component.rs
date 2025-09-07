use crate::{
    TypeExpr,
    emit::{types::escape_string_for_go, value::IrInstruction},
    parse::component_parser::TsxComponent,
    semantics::type_resolve::TypeEnv,
};

impl TsxComponent {
    pub fn emit(&self, type_env: &mut TypeEnv) -> IrInstruction {
        let extracted_vars = self
            .params
            .iter()
            .map(|x| {
                (
                    x.0.clone(),
                    match &x.1.0 {
                        TypeExpr::String => format!(
                            "fmt.Sprintf(\"\\\"%s\\\"\", html.EscapeString({}.as_dgo_string()))",
                            x.0
                        ),
                        TypeExpr::Int => format!("fmt.Sprintf(\"%v\", {}.as_dgo_int())", x.0,),
                        TypeExpr::Bool => format!("fmt.Sprintf(\"%v\", {}.as_dgo_bool())", x.0,),
                        TypeExpr::Float => format!("fmt.Sprintf(\"%v\", {}.as_dgo_float())", x.0,),
                        _ => panic!("invalid component param type"),
                    },
                )
            })
            .map(|x| {
                let var_name = format!("extracted_{}", x.0);
                let go_code = format!(
                    "{} := fmt.Sprintf(\"const %s = %s\", \"{}\", {})\n_ = {}",
                    var_name, x.0, x.1, var_name
                );
                (var_name, IrInstruction::InlineGo(go_code))
            })
            .collect::<Vec<_>>();

        let final_go_str = IrInstruction::InlineGo(format!(
            "return fmt.Sprintf(\"function {}(props){{{}\\n{}\\n}}\", {})",
            self.name,
            extracted_vars
                .iter()
                .map(|_| "%s")
                .collect::<Vec<_>>()
                .join("\\n"),
            escape_string_for_go(self.typescript_source.0.as_str()),
            extracted_vars
                .iter()
                .map(|x| x.0.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ));

        let body = extracted_vars
            .into_iter()
            .map(|x| x.1)
            .chain([final_go_str].into_iter())
            .collect();

        let ir_def = IrInstruction::FunDef(
            self.name.clone(),
            None,
            self.params
                .iter()
                .map(|x| (x.0.clone(), x.1.0.as_go_type_annotation(type_env)))
                .collect(),
            Some("string".to_string()),
            body,
        );
        ir_def
    }
}
