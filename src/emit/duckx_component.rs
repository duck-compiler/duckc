use crate::{emit::value::{IrInstruction, ToIr}, parse::duckx_component_parser::DuckxComponent, semantics::type_resolve::TypeEnv};

impl DuckxComponent {
    pub fn emit(&self, type_env: &mut TypeEnv) -> IrInstruction {
        let mut to_ir = &mut ToIr::default();
        self.value_expr.0.emit(type_env, to_ir);
        todo!()
    }
}
