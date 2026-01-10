use crate::{
    emit::value::{Emit, IrInstruction, ToIr},
    parse::{SS, duckx_component_parser::DuckxComponent},
    semantics::type_resolve::TypeEnv,
};

impl DuckxComponent {
    pub fn emit(&self, type_env: &mut TypeEnv, to_ir: &mut ToIr, _span: SS) -> IrInstruction {
        let (instr, _) = self.value_expr.expr.emit(type_env, to_ir);
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
