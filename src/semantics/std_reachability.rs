//! reachability analysis that keeps only the parts of standard library that are actually being referenced
//! so unused std defs are not resolved, typechecked or emitted

use std::collections::{HashMap, HashSet};

use crate::parse::{
    Spanned,
    extensions_def_parser::ExtensionsDef,
    function_parser::{FunctionDefintion, LambdaFunctionExpr},
    generics_parser::Generic,
    schema_def_parser::SchemaDefinition,
    source_file_parser::{GlobalVariableDeclaration, SourceFile},
    struct_parser::StructDefinition,
    type_parser::{TypeDefinition, TypeExpr},
    value_parser::{ValFmtStringContents, ValHtmlStringContents, ValueExpr},
};
use crate::semantics::ident_mangler::{MANGLE_SEP, mangle};

pub fn retain_reachable_std(user_source: &SourceFile, mut std_source: SourceFile) -> SourceFile {
    let mut pending = PendingReferences::default();

    collect_source_file_references(user_source, &mut pending);
    collect_always_kept_std_references(&std_source, &mut pending);

    let mut function_definitions_by_name = index_by_name(
        std::mem::take(&mut std_source.function_definitions),
        |function_definition| function_definition.name.clone(),
    );

    let mut struct_definitions_by_name = index_by_name(
        std::mem::take(&mut std_source.struct_definitions),
        |struct_definition| struct_definition.name.clone(),
    );

    let mut type_definitions_by_name = index_by_name(
        std::mem::take(&mut std_source.type_definitions),
        |type_definition| type_definition.name.clone(),
    );

    let mut global_variables_by_name = index_by_name(
        std::mem::take(&mut std_source.global_var_decls),
        |global_variable| global_variable.name.clone(),
    );

    let mut reachable_function_definitions = Vec::new();
    let mut reachable_struct_definitions = Vec::new();
    let mut reachable_type_definitions = Vec::new();
    let mut reachable_global_variables = Vec::new();

    while let Some(referenced_name) = pending.worklist.pop() {
        if let Some(function_definition) = function_definitions_by_name.remove(&referenced_name) {
            collect_function_definition_references(&function_definition, &mut pending);
            reachable_function_definitions.push(function_definition);
        }

        if let Some(struct_definition) = struct_definitions_by_name.remove(&referenced_name) {
            collect_struct_definition_references(&struct_definition, &mut pending);
            reachable_struct_definitions.push(struct_definition);
        }

        if let Some(type_definition) = type_definitions_by_name.remove(&referenced_name) {
            collect_type_definition_references(&type_definition, &mut pending);
            reachable_type_definitions.push(type_definition);
        }

        if let Some(global_variable) = global_variables_by_name.remove(&referenced_name) {
            collect_global_variable_references(&global_variable, &mut pending);
            reachable_global_variables.push(global_variable);
        }
    }

    std_source.function_definitions = reachable_function_definitions;
    std_source.struct_definitions = reachable_struct_definitions;
    std_source.type_definitions = reachable_type_definitions;
    std_source.global_var_decls = reachable_global_variables;
    std_source
}

#[derive(Default)]
struct PendingReferences {
    seen: HashSet<String>,
    worklist: Vec<String>,
}

impl PendingReferences {
    fn add(&mut self, name: &str) {
        if self.seen.insert(name.to_owned()) {
            self.worklist.push(name.to_owned());
        }
    }
}

fn index_by_name<T>(definitions: Vec<T>, name_of: impl Fn(&T) -> String) -> HashMap<String, T> {
    let mut definitions_by_name = HashMap::with_capacity(definitions.len());

    for definition in definitions {
        definitions_by_name.insert(name_of(&definition), definition);
    }

    definitions_by_name
}

fn collect_always_kept_std_references(std_source: &SourceFile, pending: &mut PendingReferences) {
    for extensions_definition in &std_source.extensions_defs {
        collect_extensions_def_references(extensions_definition, pending);
    }

    for jsx_component in &std_source.jsx_components {
        collect_type_expr_references(&jsx_component.props_type.0, pending);
    }

    for duckx_component in &std_source.duckx_components {
        collect_type_expr_references(&duckx_component.props_type.0, pending);
        collect_value_expr_references(&duckx_component.value_expr.0, pending);
    }

    for test_case in &std_source.test_cases {
        collect_value_expr_references(&test_case.body.0, pending);
    }
}

fn collect_source_file_references(source_file: &SourceFile, pending: &mut PendingReferences) {
    for function_definition in &source_file.function_definitions {
        collect_function_definition_references(function_definition, pending);
    }

    for struct_definition in &source_file.struct_definitions {
        collect_struct_definition_references(struct_definition, pending);
    }

    for type_definition in &source_file.type_definitions {
        collect_type_definition_references(type_definition, pending);
    }

    for schema_definition in &source_file.schema_defs {
        collect_schema_definition_references(schema_definition, pending);
    }

    for global_variable in &source_file.global_var_decls {
        collect_global_variable_references(global_variable, pending);
    }

    collect_always_kept_std_references(source_file, pending);
}

fn collect_function_definition_references(
    function_definition: &FunctionDefintion,
    pending: &mut PendingReferences,
) {
    for (_, parameter_type) in &function_definition.params {
        collect_type_expr_references(&parameter_type.0, pending);
    }

    collect_type_expr_references(&function_definition.return_type.0, pending);
    collect_value_expr_references(&function_definition.value_expr.0, pending);
    collect_generic_constraint_references(&function_definition.generics, pending);
}

fn collect_struct_definition_references(
    struct_definition: &StructDefinition,
    pending: &mut PendingReferences,
) {
    for field in &struct_definition.fields {
        collect_type_expr_references(&field.type_expr.0, pending);
    }

    for method in &struct_definition.methods {
        collect_function_definition_references(method, pending);
    }

    collect_generic_constraint_references(&struct_definition.generics, pending);
}

fn collect_type_definition_references(
    type_definition: &TypeDefinition,
    pending: &mut PendingReferences,
) {
    collect_type_expr_references(&type_definition.type_expression.0, pending);
    collect_generic_constraint_references(&type_definition.generics, pending);
}

fn collect_schema_definition_references(
    schema_definition: &SchemaDefinition,
    pending: &mut PendingReferences,
) {
    for field in &schema_definition.fields {
        collect_type_expr_references(&field.type_expr.0, pending);

        if let Some(if_branch) = &field.if_branch {
            collect_value_expr_references(&if_branch.0.condition.0, pending);

            if let Some(branch_value) = &if_branch.0.value_expr {
                collect_value_expr_references(&branch_value.0, pending);
            }
        }

        if let Some(else_branch_value) = &field.else_branch_value_expr {
            collect_value_expr_references(&else_branch_value.0, pending);
        }
    }

    if let Some(out_type) = &schema_definition.out_type {
        collect_type_expr_references(&out_type.0, pending);
    }

    if let Some(schema_function_type) = &schema_definition.schema_fn_type {
        collect_type_expr_references(&schema_function_type.0, pending);
    }
}

fn collect_global_variable_references(
    global_variable: &GlobalVariableDeclaration,
    pending: &mut PendingReferences,
) {
    collect_type_expr_references(&global_variable.type_expr.0, pending);
    collect_value_expr_references(&global_variable.initializer.0, pending);
}

fn collect_extensions_def_references(
    extensions_definition: &ExtensionsDef,
    pending: &mut PendingReferences,
) {
    collect_type_expr_references(&extensions_definition.target_type_expr.0, pending);
    for function_definition in &extensions_definition.function_definitions {
        collect_function_definition_references(&function_definition.0, pending);
    }
}

fn collect_generic_constraint_references(
    generics: &[Spanned<Generic>],
    pending: &mut PendingReferences,
) {
    for generic in generics {
        if let Some(constraint) = &generic.0.constraint {
            collect_type_expr_references(&constraint.0, pending);
        }
    }
}

fn collect_type_expr_references(type_expression: &TypeExpr, pending: &mut PendingReferences) {
    match type_expression {
        TypeExpr::Struct { name, type_params } | TypeExpr::NamedDuck { name, type_params } => {
            pending.add(name);
            for type_parameter in type_params {
                collect_type_expr_references(&type_parameter.0, pending);
            }
        }
        TypeExpr::TypeName(_, name, type_params) => {
            pending.add(name);
            for type_parameter in type_params {
                collect_type_expr_references(&type_parameter.0, pending);
            }
        }
        TypeExpr::RawTypeName(_, path, type_params) => {
            pending.add(&mangle(path));
            for type_parameter in type_params {
                collect_type_expr_references(&type_parameter.0, pending);
            }
        }
        TypeExpr::TypeOf(identifier) => pending.add(identifier),
        TypeExpr::Duck(duck) => {
            for field in &duck.fields {
                collect_type_expr_references(&field.type_expr.0, pending);
            }
        }
        TypeExpr::Tuple(elements) | TypeExpr::Or(elements) | TypeExpr::And(elements) => {
            for element in elements {
                collect_type_expr_references(&element.0, pending);
            }
        }
        TypeExpr::Fun(params, return_type, _) => {
            for (_, parameter_type) in params {
                collect_type_expr_references(&parameter_type.0, pending);
            }

            collect_type_expr_references(&return_type.0, pending);
        }
        TypeExpr::Array(inner)
        | TypeExpr::KeyOf(inner)
        | TypeExpr::Ref(inner)
        | TypeExpr::RefMut(inner) => collect_type_expr_references(&inner.0, pending),
        TypeExpr::Indexed(target, index) => {
            collect_type_expr_references(&target.0, pending);
            collect_type_expr_references(&index.0, pending);
        }
        TypeExpr::Statement
        | TypeExpr::Never
        | TypeExpr::Html
        | TypeExpr::TemplParam(_)
        | TypeExpr::Any
        | TypeExpr::Byte
        | TypeExpr::Go(_)
        | TypeExpr::Tag(_)
        | TypeExpr::String(_)
        | TypeExpr::Int
        | TypeExpr::UInt
        | TypeExpr::Bool(_)
        | TypeExpr::Char
        | TypeExpr::Float => {}
    }
}

fn collect_value_expr_references(value_expression: &ValueExpr, pending: &mut PendingReferences) {
    match value_expression {
        ValueExpr::Variable(_, name, _, _, _) => pending.add(name),
        ValueExpr::RawVariable(_, path) => pending.add(&mangle(path)),
        ValueExpr::Struct {
            name,
            fields,
            type_params,
        } => {
            pending.add(name);

            for (_, field_value) in fields {
                collect_value_expr_references(&field_value.0, pending);
            }

            for type_parameter in type_params {
                collect_type_expr_references(&type_parameter.0, pending);
            }
        }
        ValueExpr::RawStruct {
            is_global: _,
            name,
            fields,
            type_params,
        } => {
            pending.add(&mangle(name));

            for (_, field_value) in fields {
                collect_value_expr_references(&field_value.0, pending);
            }

            for type_parameter in type_params {
                collect_type_expr_references(&type_parameter.0, pending);
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
            ..
        } => {
            collect_value_expr_references(&target.0, pending);

            for parameter in params {
                collect_value_expr_references(&parameter.0, pending);
            }

            for type_parameter in type_params {
                collect_type_expr_references(&type_parameter.0, pending);
            }
        }
        ValueExpr::FieldAccess { target_obj, .. } => {
            collect_value_expr_references(&target_obj.0, pending);
        }
        ValueExpr::As(inner, target_type) => {
            collect_value_expr_references(&inner.0, pending);
            collect_type_expr_references(&target_type.0, pending);
        }
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
            span: _,
        } => {
            collect_value_expr_references(&value_expr.0, pending);

            for arm in arms {
                collect_match_arm_references(
                    &arm.type_case,
                    arm.base.as_ref(),
                    arm.condition.as_ref(),
                    &arm.value_expr,
                    pending,
                );
            }

            if let Some(arm) = else_arm {
                collect_match_arm_references(
                    &arm.type_case,
                    arm.base.as_ref(),
                    arm.condition.as_ref(),
                    &arm.value_expr,
                    pending,
                );
            }
        }
        ValueExpr::Lambda(lambda) => {
            let LambdaFunctionExpr {
                is_mut: _,
                params,
                return_type,
                value_expr,
            } = lambda.as_ref();

            for (_, parameter_type) in params {
                if let Some(parameter_type) = parameter_type {
                    collect_type_expr_references(&parameter_type.0, pending);
                }
            }

            if let Some(return_type) = return_type {
                collect_type_expr_references(&return_type.0, pending);
            }

            collect_value_expr_references(&value_expr.0, pending);
        }
        ValueExpr::VarDecl(declaration) => {
            let declaration = &declaration.0;
            if let Some(declared_type) = &declaration.type_expr {
                collect_type_expr_references(&declared_type.0, pending);
            }

            if let Some(initializer) = &declaration.initializer {
                collect_value_expr_references(&initializer.0, pending);
            }
        }
        ValueExpr::VarAssign(assignment) => {
            collect_value_expr_references(&assignment.0.target.0, pending);
            collect_value_expr_references(&assignment.0.value_expr.0, pending);
        }
        ValueExpr::FormattedString(contents) => {
            for content in contents {
                if let ValFmtStringContents::Expr(expression) = content {
                    collect_value_expr_references(&expression.0, pending);
                }
            }
        }
        ValueExpr::HtmlString(contents) => {
            for content in contents {
                if let ValHtmlStringContents::Expr(expression) = content {
                    collect_value_expr_references(&expression.0, pending);
                }
            }
        }
        ValueExpr::InlineGo(go_source, return_type) => {
            collect_inline_go_references(go_source, pending);
            if let Some(return_type) = return_type {
                collect_type_expr_references(&return_type.0, pending);
            }
        }
        ValueExpr::Array(elements, element_type) => {
            for element in elements {
                collect_value_expr_references(&element.0, pending);
            }
            if let Some(element_type) = element_type {
                collect_type_expr_references(&element_type.0, pending);
            }
        }
        ValueExpr::Tuple(elements) | ValueExpr::Block(elements) => {
            for element in elements {
                collect_value_expr_references(&element.0, pending);
            }
        }
        ValueExpr::Duck(fields) => {
            for (_, field_value) in fields {
                collect_value_expr_references(&field_value.0, pending);
            }
        }
        ValueExpr::For {
            ident: _,
            target,
            block,
        } => {
            collect_value_expr_references(&target.0, pending);
            collect_value_expr_references(&block.0, pending);
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            collect_value_expr_references(&condition.0, pending);
            collect_value_expr_references(&then.0, pending);
            if let Some(else_branch) = r#else {
                collect_value_expr_references(&else_branch.0, pending);
            }
        }
        ValueExpr::While { condition, body } => {
            collect_value_expr_references(&condition.0, pending);
            collect_value_expr_references(&body.0, pending);
        }
        ValueExpr::ArrayAccess(target, index) => {
            collect_value_expr_references(&target.0, pending);
            collect_value_expr_references(&index.0, pending);
        }
        ValueExpr::BitAnd { lhs, rhs }
        | ValueExpr::BitOr { lhs, rhs }
        | ValueExpr::BitXor { lhs, rhs } => {
            collect_value_expr_references(&lhs.0, pending);
            collect_value_expr_references(&rhs.0, pending);
        }
        ValueExpr::ShiftLeft { target, amount } | ValueExpr::ShiftRight { target, amount } => {
            collect_value_expr_references(&target.0, pending);
            collect_value_expr_references(&amount.0, pending);
        }
        ValueExpr::Add(lhs, rhs)
        | ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Mul(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs)
        | ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Or(lhs, rhs) => {
            collect_value_expr_references(&lhs.0, pending);
            collect_value_expr_references(&rhs.0, pending);
        }
        ValueExpr::BitNot(inner)
        | ValueExpr::BoolNegate(inner)
        | ValueExpr::Negate(inner)
        | ValueExpr::Async(inner)
        | ValueExpr::Defer(inner)
        | ValueExpr::Deref(inner)
        | ValueExpr::Ref(inner)
        | ValueExpr::RefMut(inner) => collect_value_expr_references(&inner.0, pending),
        ValueExpr::Return(inner) => {
            if let Some(inner) = inner {
                collect_value_expr_references(&inner.0, pending);
            }
        }
        ValueExpr::Int(..)
        | ValueExpr::String(..)
        | ValueExpr::Bool(..)
        | ValueExpr::Float(..)
        | ValueExpr::Char(..)
        | ValueExpr::Tag(..)
        | ValueExpr::Break
        | ValueExpr::Continue => {}
    }
}

fn collect_match_arm_references(
    type_case: &Spanned<TypeExpr>,
    base_type: Option<&Spanned<TypeExpr>>,
    condition: Option<&Spanned<ValueExpr>>,
    arm_value: &Spanned<ValueExpr>,
    pending: &mut PendingReferences,
) {
    collect_type_expr_references(&type_case.0, pending);

    if let Some(base_type) = base_type {
        collect_type_expr_references(&base_type.0, pending);
    }

    if let Some(condition) = condition {
        collect_value_expr_references(&condition.0, pending);
    }

    collect_value_expr_references(&arm_value.0, pending);
}

fn collect_inline_go_references(go_source: &str, pending: &mut PendingReferences) {
    for token in go_source.split(|character: char| !character.is_alphanumeric() && character != '_')
    {
        if token.contains(MANGLE_SEP) {
            pending.add(token);
        }
    }
}
