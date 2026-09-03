use std::collections::HashMap;

use crate::{
    ast::{NodeId, Span, TypeExpression},
    backend::semantics::{
        diagnostic::Diagnostic,
        module::FreeFunctionMember,
        symbol::SymbolId,
        r#type::{Type, TypeId, TypeParamId},
    },
};

use super::TypeChecker;

pub(super) type TypeParamBindings = HashMap<TypeParamId, TypeId>;

impl<'a, 'src> TypeChecker<'a, 'src> {
    pub(super) fn substitute_type(&mut self, type_id: TypeId, bindings: &TypeParamBindings) -> TypeId {
        if bindings.is_empty() {
            return type_id;
        }

        let substituted = match self.context.types[type_id.0 as usize].clone() {
            Type::TypeParam(type_param) => return bindings.get(&type_param).copied().unwrap_or(type_id),
            Type::Array(inner) => Type::Array(self.substitute_type(inner, bindings)),
            Type::Pointer(inner) => Type::Pointer(self.substitute_type(inner, bindings)),
            Type::Tuple(elements) => Type::Tuple(self.substitute_types(&elements, bindings)),
            Type::Struct(symbol, arguments) => Type::Struct(symbol, self.substitute_types(&arguments, bindings)),
            Type::Fn { params, return_type } => Type::Fn {
                params: self.substitute_types(&params, bindings),
                return_type: self.substitute_type(return_type, bindings),
            },
            Type::Unit | Type::Int | Type::Int8 | Type::Int16
            | Type::Int32 | Type::Int64 | Type::Uint | Type::Uint8
            | Type::Uint16 | Type::Uint32 | Type::Uint64 | Type::Float
            | Type::Float32 | Type::Bool | Type::String
            | Type::Never | Type::TypeError => return type_id,
        };

        self.context.intern(substituted)
    }

    fn substitute_types(&mut self, type_ids: &[TypeId], bindings: &TypeParamBindings) -> Vec<TypeId> {
        type_ids
            .iter()
            .map(|type_id| self.substitute_type(*type_id, bindings))
            .collect()
    }

    pub(super) fn contains_type_param(&self, type_id: TypeId, wanted: &[TypeParamId]) -> bool {
        match &self.context.types[type_id.0 as usize] {
            Type::TypeParam(type_param) => wanted.contains(type_param),
            Type::Array(inner) | Type::Pointer(inner) => self.contains_type_param(*inner, wanted),
            Type::Tuple(elements) => elements.iter().any(|element| self.contains_type_param(*element, wanted)),
            Type::Struct(_, arguments) => arguments.iter().any(|argument| self.contains_type_param(*argument, wanted)),
            Type::Fn { params, return_type } => {
                self.contains_type_param(*return_type, wanted)
                    || params.iter().any(|param| self.contains_type_param(*param, wanted))
            }
            Type::Unit | Type::Int | Type::Int8 | Type::Int16
            | Type::Int32 | Type::Int64 | Type::Uint | Type::Uint8
            | Type::Uint16 | Type::Uint32 | Type::Uint64 | Type::Float
            | Type::Float32 | Type::Bool | Type::String
            | Type::Never | Type::TypeError => false,
        }
    }

    pub(super) fn bind_type_params(
        &mut self,
        declared: TypeId,
        found: TypeId,
        span: Span<'src>,
        bindings: &mut TypeParamBindings,
    ) -> bool {
        let declared_type = self.context.types[declared.0 as usize].clone();

        if let Type::TypeParam(type_param) = declared_type {
            return self.bind_type_param(type_param, found, span, bindings);
        }

        match (declared_type, self.context.types[found.0 as usize].clone()) {
            (Type::Array(declared_inner), Type::Array(found_inner))
            | (Type::Pointer(declared_inner), Type::Pointer(found_inner)) => {
                self.bind_type_params(declared_inner, found_inner, span, bindings)
            }
            (Type::Tuple(declared_elements), Type::Tuple(found_elements)) => {
                self.bind_type_params_pair(&declared_elements, &found_elements, span, bindings)
            }
            (Type::Struct(declared_symbol, declared_arguments), Type::Struct(found_symbol, found_arguments))
                if declared_symbol == found_symbol =>
            {
                self.bind_type_params_pair(&declared_arguments, &found_arguments, span, bindings)
            }
            (
                Type::Fn { params: declared_params, return_type: declared_return },
                Type::Fn { params: found_params, return_type: found_return },
            ) => {
                let params_bound = self.bind_type_params_pair(&declared_params, &found_params, span, bindings);
                let return_bound = self.bind_type_params(declared_return, found_return, span, bindings);

                params_bound && return_bound
            }
            _ => {
                if self.compatible(declared, found) {
                    return true;
                }

                self.report_mismatch(declared, found, span);
                false
            }
        }
    }

    fn bind_type_params_pair(
        &mut self,
        declared: &[TypeId],
        found: &[TypeId],
        span: Span<'src>,
        bindings: &mut TypeParamBindings,
    ) -> bool {
        let mut bound = true;
        for (declared, found) in declared.iter().zip(found.iter()) {
            bound &= self.bind_type_params(*declared, *found, span, bindings);
        }

        bound
    }

    fn bind_type_param(
        &mut self,
        type_param: TypeParamId,
        found: TypeId,
        span: Span<'src>,
        bindings: &mut TypeParamBindings,
    ) -> bool {
        let Some(previous) = bindings.insert(type_param, found) else {
            return true;
        };

        if previous == found || self.is_poisoned(previous) {
            return true;
        }

        bindings.insert(type_param, previous);

        if self.is_poisoned(found) {
            return true;
        }

        let name = self.context.type_param_name(type_param);
        let previous_name = self.type_name(previous);
        let found_name = self.type_name(found);

        self.context.report(Diagnostic::conflicting_type_param(
            name,
            &previous_name,
            &found_name,
            span
        ));

        false
    }

    pub(super) fn bind_explicit_type_args(
        &mut self,
        type_args: &[TypeExpression<'src>],
        type_params: &[TypeParamId],
        span: Span<'src>,
        bindings: &mut TypeParamBindings,
    ) {
        let arguments = type_args
            .iter()
            .map(|type_arg| self.type_id_from_type_expr(type_arg))
            .collect::<Vec<_>>();

        if arguments.len() != type_params.len() {
            self.context.report(Diagnostic::wrong_type_arg_count(type_params.len(), arguments.len(), span));
            self.poison_unbound_type_params(type_params, bindings);

            return;
        }

        for (type_param, argument) in type_params.iter().zip(arguments) {
            bindings.insert(*type_param, argument);
        }
    }

    pub(super) fn poison_unbound_type_params(
        &mut self,
        type_params: &[TypeParamId],
        bindings: &mut TypeParamBindings,
    ) {
        let poison = self.context.intern(Type::TypeError);
        for type_param in type_params {
            bindings.entry(*type_param).or_insert(poison);
        }
    }

    pub(super) fn poison_type_params_of(
        &mut self,
        declared: TypeId,
        type_params: &[TypeParamId],
        bindings: &mut TypeParamBindings,
    ) {
        let occurring = type_params
            .iter()
            .copied()
            .filter(|type_param| self.contains_type_param(declared, std::slice::from_ref(type_param)))
            .collect::<Vec<_>>();

        self.poison_unbound_type_params(&occurring, bindings);
    }

    pub(super) fn resolved_type_arguments(
        &mut self,
        type_params: &[TypeParamId],
        bindings: &TypeParamBindings,
        span: Span<'src>,
    ) -> Option<Vec<TypeId>> {
        let mut arguments = Vec::with_capacity(type_params.len());

        for type_param in type_params {
            let Some(argument) = bindings.get(type_param) else {
                let name = self.context.type_param_name(*type_param);
                self.context.report(Diagnostic::cannot_infer_type_param(name, span));
                return None;
            };

            arguments.push(*argument);
        }

        Some(arguments)
    }

    pub(super) fn type_param_bindings(&self, type_params: &[TypeParamId], arguments: &[TypeId]) -> TypeParamBindings {
        debug_assert!(
            arguments.len() <= type_params.len(),
            "{} type arguments for {} type parameters would be dropped silently",
            arguments.len(),
            type_params.len(),
        );

        type_params.iter().copied().zip(arguments.iter().copied()).collect()
    }

    pub(super) fn struct_bindings(&self, struct_symbol: SymbolId, arguments: &[TypeId]) -> TypeParamBindings {
        self.type_param_bindings(self.context.type_params_of(struct_symbol), arguments)
    }

    pub(super) fn declared_struct_type(&mut self, struct_symbol: SymbolId) -> TypeId {
        let type_params = self.context.type_params_of(struct_symbol).to_vec();
        let arguments = type_params
            .into_iter()
            .map(|type_param| self.context.intern(Type::TypeParam(type_param)))
            .collect::<Vec<_>>();

        self.context.intern(Type::Struct(struct_symbol, arguments))
    }

    pub(super) fn holds_type_error(&self, type_ids: &[TypeId]) -> bool {
        type_ids.iter().any(|type_id| matches!(self.context.types[type_id.0 as usize], Type::TypeError))
    }

    pub(super) fn record_type_arguments(&mut self, node: NodeId, arguments: Vec<TypeId>) {
        if arguments.is_empty() || self.holds_type_error(&arguments) {
            return;
        }

        self.context.modules[self.module.0 as usize].type_arguments.insert(node, arguments);
    }

    pub(super) fn store_free_function(
        &mut self,
        node: NodeId,
        name: &'src str,
        receiver_type: Option<TypeId>,
        value_type: TypeId,
    ) {
        self.context.modules[self.module.0 as usize]
            .free_function_members
            .insert(node, FreeFunctionMember { name, receiver_type, value_type });
    }
}
