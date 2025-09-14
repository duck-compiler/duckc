use crate::{
    emit::value::{IrInstruction, ToIr},
    parse::{duckx_component_parser::DuckxComponent, SS},
    semantics::type_resolve::TypeEnv,
};

impl DuckxComponent {
    pub fn emit(&self, type_env: &mut TypeEnv, to_ir: &mut ToIr, span: SS) -> IrInstruction {
        let (instr, _) = self.value_expr.0.emit(type_env, to_ir, span);
        IrInstruction::FunDef(
            self.name.clone(),
            None,
            vec![(
                "props".to_string(),
                self.props_type.0.as_go_type_annotation(type_env),
            )],
            Some("func (env *TemplEnv) string".to_string()),
            instr,
        )
    }
}
