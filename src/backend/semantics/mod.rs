use crate::backend::semantics::{context::SemanticsContext, module::ModuleId};

pub mod r#type;
pub mod context;
pub mod symbol;
pub mod module;
pub mod passes;
pub mod diagnostic;
pub mod go_resolve;
pub mod go_map;

#[cfg(test)]
mod semantics_test;

/// Runs every semantic pass in order over a single module
pub fn analyze_module<'src>(context: &mut SemanticsContext<'src>, module: ModuleId) {
    passes::collect_module(module, context);
    passes::resolve_module(module, context);
    passes::typecheck_module(module, context);
}
