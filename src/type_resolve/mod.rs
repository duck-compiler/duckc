use crate::ast::TypeExpression;

mod collect;
mod type_env;
mod types;
mod scope_resolver;

pub use collect::collect_module;
pub use scope_resolver::resolve_scopes;
pub use type_env::{TypeEnv, ModuleId, ModuleTables};

struct FunctionHeader<'src> {
    name: &'src str,
    args: Vec<TypeExpression<'src>>,
}

struct StructHeader<'src> {
    name: &'src str,
    fields: Vec<TypeExpression<'src>>,
    methods: Vec<FunctionHeader<'src>>,
}

type Stack<T> = Vec<T>;
