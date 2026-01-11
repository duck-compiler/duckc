use std::collections::HashMap;
use std::process;

use chumsky::container::Seq;
use colored::Colorize;

use crate::parse::struct_parser::{NamedDuckDefinition, StructDefinition};
use crate::parse::type_parser::{Duck, TypeExpr};
use crate::parse::value_parser::{empty_range, type_expr_into_empty_range};
use crate::parse::{Field, SS, failure_with_occurence};
use crate::parse::{
    Spanned, failure,
    value_parser::{ValFmtStringContents, ValueExpr},
};
use crate::semantics::ident_mangler::{MANGLE_SEP, mangle};
use crate::semantics::type_resolve::{TypeEnv, is_const_var, merge_all_or_type_expr};
use crate::semantics::type_resolve2::ValueExprWithType;

impl TypeExpr {
    pub fn as_clean_user_faced_type_name(&self) -> String {
        return format!("{self}");
    }

    pub fn is_component_compatible(&self) -> bool {
        match self {
            TypeExpr::Duck(Duck { fields }) => fields
                .iter()
                .all(|x| x.type_expr.0.is_component_compatible()),
            TypeExpr::Array(ty) => ty.0.is_component_compatible(),
            TypeExpr::String(..)
            | TypeExpr::Bool(..)
            | TypeExpr::Float
            | TypeExpr::Int
            | TypeExpr::UInt => true,
            _ => false,
        }
    }

    pub fn from_value_expr_dereferenced(
        value_expr: &Spanned<ValueExpr>,
        type_env: &mut TypeEnv,
    ) -> Spanned<TypeExpr> {
        let (res, _, _) =
            TypeExpr::from_value_expr_dereferenced_with_count_and_mut(value_expr, type_env);
        res
    }

    pub fn from_value_expr_dereferenced_with_count(
        value_expr: &Spanned<ValueExpr>,
        type_env: &mut TypeEnv,
    ) -> (Spanned<TypeExpr>, usize) {
        let (res, counter, _) =
            TypeExpr::from_value_expr_dereferenced_with_count_and_mut(value_expr, type_env);
        (res, counter)
    }

    pub fn from_value_expr_dereferenced_with_count_and_mut(
        value_expr: &Spanned<ValueExpr>,
        type_env: &mut TypeEnv,
    ) -> (Spanned<TypeExpr>, usize, bool) {
        let mut res = TypeExpr::from_value_expr(value_expr, type_env);
        let mut counter = 0;
        let mut is_mut = true;

        loop {
            if let TypeExpr::Ref(v) = res.0 {
                res.0 = v.0;
                counter += 1;
                is_mut = false;
            } else if let TypeExpr::RefMut(v) = res.0 {
                res.0 = v.0;
                counter += 1;
                is_mut = true;
            } else {
                break;
            }
        }

        (res, counter, is_mut)
    }

    #[track_caller]
    pub fn from_value_expr(
        value_expr: &Spanned<ValueExpr>,
        type_env: &mut TypeEnv,
    ) -> Spanned<TypeExpr> {
        let complete_span = &value_expr.1;
        let value_expr = &value_expr.0;

        return (
            match value_expr {
                ValueExpr::BitAnd { lhs: inner, rhs: _ }
                | ValueExpr::BitOr { lhs: inner, rhs: _ }
                | ValueExpr::BitXor { lhs: inner, rhs: _ }
                | ValueExpr::ShiftLeft {
                    target: inner,
                    amount: _,
                }
                | ValueExpr::ShiftRight {
                    target: inner,
                    amount: _,
                }
                | ValueExpr::BitNot(inner) => {
                    let inner_type = TypeExpr::from_value_expr(&inner.expr, type_env);
                    inner_type.0
                }
                ValueExpr::RawStruct { .. } => panic!("raw struct should not be here"),
                ValueExpr::Negate(v) => TypeExpr::from_value_expr(&v.expr, type_env).0,
                ValueExpr::Async(e) => {
                    let inner = TypeExpr::from_value_expr(&e.expr, type_env);

                    let ValueExpr::FunctionCall {
                        target,
                        params,
                        type_params: _,
                    } = &e.expr.0
                    else {
                        panic!("can only async func call")
                    };

                    if [target.as_ref()]
                        .into_iter()
                        .chain(params.iter())
                        .any(|v| TypeExpr::from_value_expr(&v.expr, type_env).0.is_never())
                    {
                        TypeExpr::Never
                    } else {
                        TypeExpr::Struct {
                            name: mangle(&["std", "sync", "Channel"]),
                            type_params: vec![(inner.0, *complete_span)],
                        }
                    }
                }
                ValueExpr::Defer(call) => {
                    let ValueExpr::FunctionCall {
                        target,
                        params,
                        type_params: _,
                    } = &call.expr.0
                    else {
                        panic!("can only defer func call")
                    };
                    if [target.as_ref()]
                        .into_iter()
                        .chain(params.iter())
                        .any(|v| TypeExpr::from_value_expr(&v.expr, type_env).0.is_never())
                    {
                        TypeExpr::Never
                    } else {
                        TypeExpr::Statement
                    }
                }
                ValueExpr::As(v, t) => {
                    let v_type = TypeExpr::from_value_expr(&v.expr, type_env);

                    if v_type.0.is_never() {
                        TypeExpr::Never
                    } else {
                        check_type_compatability(t, &(v_type.0, v.expr.1), type_env);
                        t.0.clone()
                    }
                }
                ValueExpr::For { .. } => TypeExpr::Statement,
                ValueExpr::Ref(v) => {
                    let v_type = TypeExpr::from_value_expr(&v.expr, type_env);
                    if v_type.0.is_never() {
                        TypeExpr::Never
                    } else {
                        TypeExpr::Ref((v_type.0, v.expr.1).into())
                    }
                }
                ValueExpr::RefMut(v) => {
                    let v_type = TypeExpr::from_value_expr(&v.expr, type_env);
                    if v_type.0.is_never() {
                        TypeExpr::Never
                    } else {
                        TypeExpr::RefMut((v_type.0, v.expr.1).into())
                    }
                }
                ValueExpr::Deref(v) => {
                    let ty_expr = TypeExpr::from_value_expr(&v.expr, type_env);

                    if ty_expr.0.is_never() {
                        TypeExpr::Never
                    } else if !matches!(ty_expr.0, TypeExpr::Ref(..) | TypeExpr::RefMut(..)) {
                        failure_with_occurence(
                            "Can only dereference a reference",
                            *complete_span,
                            [("This is not a reference".to_string(), v.expr.1)],
                        );
                    } else {
                        let (TypeExpr::Ref(t) | TypeExpr::RefMut(t)) = ty_expr.0 else {
                            unreachable!()
                        };
                        t.0
                    }
                }
                ValueExpr::HtmlString(..) => TypeExpr::Html, // TODO: CHECK FOR NEVER
                ValueExpr::Tag(identifier) => TypeExpr::Tag(identifier.clone()),
                ValueExpr::RawVariable(_x, p) => panic!("{}", p.join(" ").leak()),
                ValueExpr::FormattedString(contents) => {
                    for c in contents {
                        if let ValFmtStringContents::Expr(e) = c {
                            let type_expr = TypeExpr::from_value_expr(&e.expr, type_env);
                            if type_expr.0.is_never() {
                                return (TypeExpr::Never, type_expr.1);
                            }

                            if !type_expr.0.is_string() {
                                let hints = [
                                    (
                                        "interpolated values inside a f-string must evaluate to a string".to_string(),
                                        e.expr.1,
                                    ),
                                    (
                                        format!(
                                            "this is of type {}{}",
                                            type_expr.0.as_clean_user_faced_type_name().yellow(),
                                            if type_expr.0.implements_to_string(type_env) {
                                                format!(
                                                    ", which implements {}. Add the method-call after the value",
                                                    "to_string".yellow(),
                                                )
                                            } else {
                                                String::new()
                                            }
                                        ),
                                        e.expr.1,
                                    ),
                                ];

                                failure_with_occurence("Incompatible Types", e.expr.1, hints);
                            }
                            require(
                                type_expr.0.is_string(),
                                format!("Needs to be string, is {type_expr:?}"),
                            );
                        }
                    }
                    TypeExpr::String(None)
                }
                ValueExpr::ArrayAccess(target, idx) => {
                    let target_type =
                        TypeExpr::from_value_expr_dereferenced(&target.expr, type_env);
                    let idx_type = TypeExpr::from_value_expr(&idx.expr, type_env);

                    if target_type.0.is_never() || idx_type.0.is_never() {
                        TypeExpr::Never
                    } else {
                        require(
                            target_type.0.is_array() || target_type.0.ref_is_array(),
                            "Needs to be array".into(),
                        );
                        require(idx_type.0.is_int(), "Needs to be int".into());

                        let TypeExpr::Array(array_type) = target_type.0 else {
                            panic!("{target_type:?}")
                        };

                        array_type.0.clone()
                    }
                }
                ValueExpr::Array(_, ty) => TypeExpr::Array(ty.as_ref().cloned().unwrap().into()),
                ValueExpr::Lambda(lambda_expr) => TypeExpr::Fun(
                    lambda_expr
                        .params
                        .iter()
                        .map(|(name, type_expr)| {
                            (Some(name.clone()), type_expr.as_ref().cloned().unwrap())
                        })
                        .collect(),
                    Box::new(
                        lambda_expr
                            .return_type
                            .clone()
                            .unwrap_or(TypeExpr::unit_with_span(*complete_span)),
                    ),
                    lambda_expr.is_mut,
                ),
                ValueExpr::InlineGo(_, ty) => ty.as_ref().cloned().unwrap_or(TypeExpr::unit()).0,
                ValueExpr::Int(_, t) => {
                    t.as_ref().cloned().map(|(x, _)| x).unwrap_or(TypeExpr::Int)
                }
                ValueExpr::Bool(..) => TypeExpr::Bool(None),
                ValueExpr::Char(..) => TypeExpr::Char,
                ValueExpr::Float(..) => TypeExpr::Float,
                ValueExpr::String(..) => TypeExpr::String(None),
                ValueExpr::Break => TypeExpr::Never,
                ValueExpr::Continue => TypeExpr::Never,
                ValueExpr::Return(..) => TypeExpr::Never,
                ValueExpr::VarAssign(assignment) => {
                    let target_type =
                        TypeExpr::from_value_expr(&assignment.0.target.expr, type_env);
                    let value_type =
                        TypeExpr::from_value_expr(&assignment.0.value_expr.expr, type_env);

                    if target_type.0.is_never() || value_type.0.is_never() {
                        TypeExpr::Never
                    } else {
                        TypeExpr::Statement
                    }
                }
                ValueExpr::VarDecl(decl) => {
                    let decl = decl.as_ref();
                    if let Some(initializer) = decl.0.initializer.as_ref() {
                        let init_type = TypeExpr::from_value_expr(&initializer.expr, type_env);
                        if init_type.0.is_never() {
                            return (TypeExpr::Never, *complete_span);
                        }

                        check_type_compatability(
                            decl.0.type_expr.as_ref().expect(
                                "compiler error: i expect the implicit types to be resolved by now",
                            ),
                            &(init_type.0, initializer.expr.1),
                            type_env,
                        );
                    }

                    TypeExpr::Statement
                }
                ValueExpr::Struct {
                    name,
                    fields,
                    type_params,
                } => {
                    let _struct_def = type_env.get_struct_def_with_type_params_mut(
                        name.as_str(),
                        type_params,
                        *complete_span,
                    );

                    for field in fields {
                        let ty = TypeExpr::from_value_expr(&field.1.expr, type_env);
                        if ty.0.is_never() {
                            return (TypeExpr::Never, field.1.expr.1);
                        }
                    }

                    TypeExpr::Struct {
                        name: name.to_string(),
                        type_params: type_params.clone(),
                    }
                }
                ValueExpr::Tuple(fields) => {
                    let mut is_never = false;
                    let types = fields
                        .iter()
                        .map(|value_expr| {
                            // todo: check if we really want to unconst tuple values
                            // maybe we need a way to tell that it should be consted here. e.g.
                            //  `(5,"hallo")`
                            (
                                match TypeExpr::from_value_expr(&value_expr.expr, type_env).0 {
                                    TypeExpr::Never => {
                                        is_never = true;
                                        TypeExpr::Never
                                    }
                                    x => x,
                                },
                                value_expr.expr.1,
                            )
                        })
                        .collect::<Vec<Spanned<TypeExpr>>>();

                    if is_never {
                        TypeExpr::Never
                    } else {
                        TypeExpr::Tuple(types)
                    }
                }
                ValueExpr::Duck(fields) => {
                    let mut f = Vec::new();

                    for (name, value_expr) in fields {
                        let (value_expr, span) = (&value_expr.expr, value_expr.expr.1);
                        let spanned_type = TypeExpr::from_value_expr(&value_expr, type_env);

                        if spanned_type.0.is_never() {
                            return (TypeExpr::Never, spanned_type.1);
                        }

                        f.push(Field {
                            name: name.clone(),
                            type_expr: spanned_type,
                        });
                    }

                    TypeExpr::Duck(Duck { fields: f })
                }
                ValueExpr::Add(left, right) => {
                    let left_type_expr: Spanned<TypeExpr> =
                        TypeExpr::from_value_expr(&left.expr, type_env);
                    if left_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    let right_type_expr: Spanned<TypeExpr> =
                        TypeExpr::from_value_expr(&right.expr, type_env);

                    if right_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    require(
                        left_type_expr.0.is_number(),
                        format!(
                            "Addition '+' is only allowed for numbers. You've used {} + {}.",
                            left_type_expr.0.as_go_type_annotation(type_env),
                            right_type_expr.0.as_go_type_annotation(type_env)
                        ),
                    );

                    check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                    left_type_expr.0.unconst()
                }
                ValueExpr::Sub(left, right) => {
                    let left_type_expr: Spanned<TypeExpr> =
                        TypeExpr::from_value_expr(&left.expr, type_env);

                    if left_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    let right_type_expr: Spanned<TypeExpr> =
                        TypeExpr::from_value_expr(&right.expr, type_env);

                    if right_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    require(
                        left_type_expr.0.is_number(),
                        format!(
                            "Subtraction '-' is only allowed for numbers. You've used {} - {}.",
                            left_type_expr.0.as_go_type_annotation(type_env),
                            right_type_expr.0.as_go_type_annotation(type_env)
                        ),
                    );

                    check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                    left_type_expr.0.unconst()
                }
                ValueExpr::Mod(left, right) => {
                    let left_type_expr = TypeExpr::from_value_expr(&left.expr, type_env);
                    if left_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    let right_type_expr = TypeExpr::from_value_expr(&right.expr, type_env);
                    if right_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    require(
                        left_type_expr.0.is_number(),
                        format!(
                            "Modulo '%' is only allowed for numbers. You've used {} % {}.",
                            left_type_expr.0.as_go_type_annotation(type_env),
                            right_type_expr.0.as_go_type_annotation(type_env)
                        ),
                    );

                    check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                    left_type_expr.0.unconst()
                }
                ValueExpr::Div(left, right) => {
                    let left_type_expr = TypeExpr::from_value_expr(&left.expr, type_env);
                    if left_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    let right_type_expr = TypeExpr::from_value_expr(&right.expr, type_env);
                    if right_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    require(
                        left_type_expr.0.is_number(),
                        format!(
                            "Division '/' is only allowed for numbers. You've used {} / {}.",
                            left_type_expr.0.as_go_type_annotation(type_env),
                            right_type_expr.0.as_go_type_annotation(type_env)
                        ),
                    );

                    check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                    left_type_expr.0.unconst()
                }
                ValueExpr::Equals(lhs, rhs)
                | ValueExpr::NotEquals(lhs, rhs)
                | ValueExpr::LessThan(lhs, rhs)
                | ValueExpr::LessThanOrEquals(lhs, rhs)
                | ValueExpr::GreaterThan(lhs, rhs)
                | ValueExpr::GreaterThanOrEquals(lhs, rhs)
                | ValueExpr::And(lhs, rhs)
                | ValueExpr::Or(lhs, rhs) => {
                    let left_type_expr = TypeExpr::from_value_expr(&lhs.expr, type_env);
                    if left_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    let right_type_expr = TypeExpr::from_value_expr(&rhs.expr, type_env);
                    if right_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                    TypeExpr::Bool(None)
                }
                ValueExpr::Mul(left, right) => {
                    let left_type_expr = TypeExpr::from_value_expr(&left.expr, type_env);
                    if left_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    let right_type_expr = TypeExpr::from_value_expr(&right.expr, type_env);

                    if right_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    require(
                        left_type_expr.0.is_number(),
                        format!(
                            "Multiplication '*' is only allowed for numbers. You've used {} + {}.",
                            left_type_expr.0.as_go_type_annotation(type_env),
                            right_type_expr.0.as_go_type_annotation(type_env)
                        ),
                    );

                    check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                    left_type_expr.0.unconst()
                }
                ValueExpr::FunctionCall {
                    target,
                    params,
                    type_params,
                } => {
                    let target_type = TypeExpr::from_value_expr(&target.expr, type_env);
                    if target_type.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    let mut in_param_types = Vec::new();

                    for param in params {
                        let param_type = (
                            TypeExpr::from_value_expr(&param.expr, type_env),
                            param.expr.1,
                        );
                        if param_type.0.0.is_never() {
                            return (TypeExpr::Never, *complete_span);
                        }
                        in_param_types.push(param_type);
                    }

                    // todo: type_params

                    if !type_params.is_empty() {
                        match &target.expr.0 {
                            ValueExpr::Variable(_, var_name, ..) => {
                                let new_fn_name = [var_name.to_string()]
                                    .into_iter()
                                    .chain(
                                        type_params
                                            .iter()
                                            .map(|(t, _)| t.as_clean_go_type_name(type_env)),
                                    )
                                    .collect::<Vec<_>>()
                                    .join(MANGLE_SEP);

                                let fn_def = type_env
                                    .generic_fns_generated
                                    .iter()
                                    .find(|fn_def| fn_def.name.as_str() == new_fn_name.clone());

                                let fn_def = fn_def.expect("this should exist");

                                let params = fn_def.params.clone();
                                for (index, param) in params.iter().enumerate() {
                                    let given_type =
                                        in_param_types.get(index).unwrap_or_else(|| {
                                            failure_with_occurence(
                                                "Missing Function Parameter",
                                                *complete_span,
                                                [
                                                    (format!("missing this param"), param.1.1),
                                                    (
                                                        format!("in this function function call"),
                                                        *complete_span,
                                                    ),
                                                ],
                                            )
                                        });
                                    // TODO: check if we should clone the typeenv
                                    check_type_compatability_full(
                                        &param.1.clone(),
                                        &given_type.0,
                                        &mut type_env.clone(),
                                        false,
                                    );
                                }

                                return fn_def.return_type.clone();
                            }
                            ValueExpr::FieldAccess {
                                target_obj,
                                field_name,
                            } => {
                                let t = TypeExpr::from_value_expr_dereferenced(
                                    &target_obj.expr,
                                    type_env,
                                );
                                let TypeExpr::Struct {
                                    name: struct_name,
                                    type_params: struct_type_params,
                                } = t.0
                                else {
                                    panic!("{t:?}")
                                };

                                let replaced_generics = type_env
                                    .get_struct_def_with_type_params_mut(
                                        &struct_name,
                                        &struct_type_params,
                                        *complete_span,
                                    )
                                    .name
                                    .clone();

                                let (_new_fn_name, global_generic_generation_id) = {
                                    let new_method_name = [field_name.clone()]
                                        .into_iter()
                                        .chain(
                                            type_params
                                                .iter()
                                                .map(|(t, _)| t.as_clean_go_type_name(type_env)),
                                        )
                                        .collect::<Vec<_>>();

                                    let mut gen_id = new_method_name.clone();
                                    gen_id.insert(0, replaced_generics.clone());
                                    (new_method_name.join(MANGLE_SEP), gen_id.join(MANGLE_SEP))
                                };

                                let header =
                                    type_env.get_method_header(&global_generic_generation_id);

                                let params = header.params;
                                for (index, param) in params.iter().enumerate() {
                                    let given_type =
                                        in_param_types.get(index).expect("todo: len doesnt match");
                                    check_type_compatability_full(
                                        param,
                                        &given_type.0,
                                        type_env,
                                        false,
                                    );
                                }

                                return header.return_type.clone();
                            }
                            _ => {}
                        };
                    }

                    let mut target_type = TypeExpr::from_value_expr(&target.expr, type_env);
                    if let TypeExpr::Fun(param_types, return_type, _) = &mut target_type.0 {
                        param_types
                            .iter_mut()
                            .enumerate()
                            .for_each(|(index, param_type)| {
                                if matches!(param_type.1.0, TypeExpr::Any) {
                                    return;
                                }

                                let Some(in_param_type) = in_param_types.get(index) else {
                                    failure_with_occurence(
                                        "Missing Parameter in Function Call",
                                        target.expr.1,
                                        [
                                            (
                                                format!(
                                                    "This function requires a {}",
                                                    param_type.1.0.as_clean_user_faced_type_name(),
                                                ),
                                                param_type.1.1,
                                            ),
                                            (
                                                format!(
                                                    "You need to pass a {} to this function",
                                                    param_type.1.0.as_clean_user_faced_type_name(),
                                                ),
                                                target.expr.1,
                                            ),
                                        ],
                                    )
                                };

                                if let Some(param_name) = &param_type.0
                                    && param_name == "self"
                                {
                                    check_type_compatability_full(
                                        &param_type.1,
                                        &in_param_type.0,
                                        type_env,
                                        is_const_var(&params[index].expr.0),
                                    );
                                } else {
                                    check_type_compatability_full(
                                        &param_type.1,
                                        &in_param_type.0,
                                        type_env,
                                        is_const_var(&params[index].expr.0),
                                    );
                                }

                                // variant any replace
                                if let TypeExpr::Array(boxed) =
                                    &in_param_types.get(index).unwrap().0.0
                                    && let TypeExpr::Or(_) = boxed.as_ref().0
                                {
                                    param_type.1.0 =
                                        TypeExpr::Array(Box::new((TypeExpr::Any, empty_range())))
                                }
                            });

                        return return_type.as_ref().clone();
                    }

                    failure(
                        target.expr.1.context.file_name,
                        "Tried to invoke a non-function value".to_string(),
                        (
                            "This is the value you tried to invoke as a function.".to_string(),
                            target.expr.1,
                        ),
                        vec![
                            (
                                format!(
                                    "the thing you tried to invoke is of type {}",
                                    TypeExpr::from_value_expr(&target.expr, type_env)
                                        .0
                                        .as_clean_go_type_name(type_env)
                                ),
                                target.expr.1,
                            ),
                            (
                                format!(
                                    "{} cannot be called as it's not of type function!",
                                    TypeExpr::from_value_expr(&target.expr, type_env)
                                        .0
                                        .as_clean_go_type_name(type_env)
                                ),
                                target.expr.1,
                            ),
                        ],
                        target.expr.1.context.file_contents,
                    )
                }
                ValueExpr::Block(value_exprs) => {
                    let mut ty = TypeExpr::Tuple(vec![]);
                    value_exprs.iter().for_each(|value_expr| {
                        if !ty.is_never() {
                            ty = TypeExpr::from_value_expr(&value_expr.expr, type_env).0;
                        }
                    });

                    ty
                }
                ValueExpr::Variable(_, ident, type_expr, _, _) => type_expr
                    .as_ref()
                    .cloned()
                    .or(type_env.get_identifier_type(ident))
                    .unwrap_or_else(|| {
                        failure_with_occurence(
                            "Unknown Identifier",
                            *complete_span,
                            [(
                                format!("couldn't resolve type for identifier {ident}"),
                                *complete_span,
                            )],
                        );
                    })
                    .clone(),
                ValueExpr::BoolNegate(bool_expr) => {
                    let t = TypeExpr::from_value_expr(&bool_expr.expr, type_env);
                    if t.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    check_type_compatability(&t, &TypeExpr::Bool(None).into_empty_span(), type_env);

                    TypeExpr::Bool(None)
                }
                ValueExpr::If {
                    condition,
                    then,
                    r#else,
                } => {
                    let condition_type_expr = TypeExpr::from_value_expr(&condition.expr, type_env);

                    if condition_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    check_type_compatability(
                        &condition_type_expr,
                        &TypeExpr::Bool(None).into_empty_span(),
                        type_env,
                    );

                    let then_type_expr = TypeExpr::from_value_expr(&then.expr, type_env);
                    if let Some(r#else) = r#else {
                        let else_type_expr = TypeExpr::from_value_expr(&r#else.expr, type_env);

                        if !matches!(else_type_expr.0, TypeExpr::Statement) {
                            if then_type_expr.0.is_never() && else_type_expr.0.is_never() {
                                return (TypeExpr::Never, *complete_span);
                            }

                            let mut both = (
                                TypeExpr::Or(vec![then_type_expr, else_type_expr]),
                                *complete_span,
                            );

                            merge_all_or_type_expr(&mut both, type_env);

                            return both;
                        }
                    }

                    TypeExpr::Statement
                }
                ValueExpr::FieldAccess {
                    target_obj,
                    field_name,
                } => {
                    let span = target_obj.expr.1;
                    let target_obj_type_expr =
                        TypeExpr::from_value_expr_dereferenced(&target_obj.expr, type_env);

                    if target_obj_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    if !(target_obj_type_expr
                        .0
                        .has_field_by_name(field_name.clone(), type_env)
                        || target_obj_type_expr
                            .0
                            .has_method_by_name(field_name.clone(), type_env)
                        || target_obj_type_expr
                            .0
                            .ref_has_field_by_name(field_name.clone(), type_env)
                        || target_obj_type_expr
                            .0
                            .ref_has_method_by_name(field_name.clone(), type_env)
                        || target_obj_type_expr
                            .0
                            .has_extension_by_name(field_name.clone(), type_env))
                    {
                        // dbg!(&field_name, &target_obj_type_expr);
                        failure_with_occurence(
                            "Invalid Field Access",
                            {
                                let mut span = span;
                                span.end += 2;
                                span
                            },
                            vec![(
                                format!(
                                    "this is of type {} and it has no field '{}'",
                                    target_obj_type_expr
                                        .0
                                        .as_clean_user_faced_type_name()
                                        .bright_yellow(),
                                    field_name.bright_blue()
                                ),
                                span,
                            )],
                        )
                    }

                    let target_obj_type_expr = target_obj_type_expr.0;
                    target_obj_type_expr
                        .typeof_field(field_name.to_string(), type_env)
                        .or_else(|| {
                            target_obj_type_expr.ref_typeof_field(field_name.to_string(), type_env)
                        })
                        .unwrap_or_else(|| panic!("Invalid field access {target_obj_type_expr}"))
                }
                ValueExpr::While { condition, body } => {
                    let condition_type_expr = TypeExpr::from_value_expr(&condition.expr, type_env);

                    if condition_type_expr.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    check_type_compatability(
                        &condition_type_expr,
                        &TypeExpr::Bool(None).into_empty_span(),
                        type_env,
                    );

                    let _body_type_expr = TypeExpr::from_value_expr(&body.expr, type_env);

                    TypeExpr::Tuple(vec![])
                }
                // TODO: Match Expressions need to be type resolved just as the function defs
                ValueExpr::Match {
                    value_expr,
                    arms,
                    else_arm,
                    span: _,
                } => {
                    let v_expr_type =
                        TypeExpr::from_value_expr_dereferenced(&value_expr.expr, type_env);
                    if v_expr_type.0.is_never() {
                        return (TypeExpr::Never, *complete_span);
                    }

                    let mut arms = arms.clone();
                    if let Some(arm) = else_arm {
                        arms.push(arm.as_ref().clone());
                    }

                    let mut arm_types = Vec::new();
                    for arm in &arms {
                        let arm_type = TypeExpr::from_value_expr(&arm.value_expr.expr, type_env);

                        let mut cloned_arm_type = arm_type.clone().0.into_empty_span();
                        merge_all_or_type_expr(&mut cloned_arm_type, type_env);
                        type_expr_into_empty_range(&mut cloned_arm_type);

                        if !arm_types.iter().any(|t: &Spanned<TypeExpr>| {
                            let mut cl1 = t.clone();
                            type_expr_into_empty_range(&mut cl1);
                            cl1.0.unconst() == cloned_arm_type.0.unconst()
                        }) {
                            arm_types.push((arm_type.0.unconst(), arm.value_expr.expr.1));
                        }
                    }

                    // arm_types.retain(|f| !f.0.is_unit());

                    if else_arm.is_none() {
                        let possible_types: Vec<Spanned<TypeExpr>> =
                            match TypeExpr::from_value_expr_dereferenced(&value_expr.expr, type_env)
                                .0
                            {
                                TypeExpr::Or(types) => types,
                                other => vec![(other.clone(), value_expr.expr.1)],
                            };

                        let mut covered_types = Vec::new();
                        for arm in &arms {
                            let case_type = &arm.type_case.0;

                            let mut cloned_arm_type = case_type.clone().into_empty_span();
                            type_expr_into_empty_range(&mut cloned_arm_type);
                            if !covered_types.iter().any(|t: &Spanned<TypeExpr>| {
                                let mut cl1 = t.clone();
                                type_expr_into_empty_range(&mut cl1);
                                cl1.0 == cloned_arm_type.0
                            }) {
                                covered_types.push((case_type.clone(), arm.type_case.1));
                            }
                        }

                        possible_types.iter().for_each(|possible_type| {
                            let mut b = possible_type.clone();
                            type_expr_into_empty_range(&mut b);
                            let is_covered = &covered_types.iter().any(|x| {
                                let mut a = x.clone();
                                type_expr_into_empty_range(&mut a);
                                a.0.unconst() == b.0.unconst()
                            });
                            if !is_covered {
                                let missing_type = possible_type;
                                failure_with_occurence(
                                    "Unexhaustive Match",
                                    *complete_span,
                                    vec![(
                                        format!(
                                            "possible type {} not covered",
                                            format!("{}", missing_type.0).bright_yellow()
                                        ),
                                        *complete_span,
                                    )],
                                );
                            }
                        });
                    }

                    let was_empty_before = arm_types.is_empty();
                    arm_types.retain(|t| !t.0.is_never());

                    if arm_types.is_empty() && !was_empty_before {
                        return (TypeExpr::Never, *complete_span);
                    }

                    if arm_types.is_empty() {
                        TypeExpr::Tuple(vec![])
                    } else {
                        let mut a = (TypeExpr::Or(arm_types), *complete_span);
                        merge_all_or_type_expr(&mut a, type_env);
                        a.0
                    }
                }
            },
            *complete_span,
        );
    }

    pub fn is_object_like(&self) -> bool {
        match self {
            Self::Tuple(..) | Self::Duck(..) | Self::Struct { .. } | Self::NamedDuck { .. } => true,
            _ => false,
        }
    }

    pub fn ref_is_object_like(&self) -> bool {
        match self {
            Self::Ref(t) | Self::RefMut(t) => t.0.is_object_like() || t.0.ref_is_object_like(),
            _ => false,
        }
    }

    pub fn is_duck(&self) -> bool {
        match self {
            Self::Duck(..) | Self::NamedDuck { .. } => true,
            _ => false,
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            Self::Tuple(v) => v.is_empty(),
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            Self::Struct { .. } => true,
            _ => false,
        }
    }

    pub fn unconst(&self) -> TypeExpr {
        match self {
            Self::String(..) => Self::String(None),
            Self::Int => Self::Int,
            Self::Bool(..) => Self::Bool(None),
            s => s.clone(),
        }
    }

    pub fn has_subtypes(&self) -> bool {
        match self {
            Self::Or(..) | Self::Tuple(..) | Self::Duck(..) | Self::Struct { .. } => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn has_field(&self, field: Field) -> bool {
        match self {
            Self::Tuple(fields) => fields.len() > field.name.parse::<usize>().unwrap(),
            Self::Struct { .. } => todo!(),
            Self::Duck(duck) => duck.fields.contains(&field),
            _ => false,
        }
    }

    pub fn has_method_by_name(&self, name: String, type_env: &mut TypeEnv) -> bool {
        if name.as_str() == "to_string" && self.implements_to_string(type_env) {
            return true;
        }
        if name.as_str() == "clone" && self.implements_clone(type_env) {
            return true;
        }
        if name.as_str() == "to_json" && self.implements_to_json(type_env) {
            return true;
        }
        if name.as_str() == "hash" && self.implements_hash(type_env) {
            return true;
        }
        if name.as_str() == "ord" && self.implements_ord(type_env) {
            return true;
        }
        if name.as_str() == "len"
            && let TypeExpr::Array(..) = self
        {
            return true;
        }
        if name.as_str() == "iter" && self.implements_into_iter(type_env) {
            return true;
        }
        if name.as_str() == "iter_mut" && self.implements_into_iter_mut(type_env) {
            return true;
        }
        match self {
            Self::Struct {
                name: r#struct,
                type_params,
            } => {
                let StructDefinition {
                    name: struct_name,
                    fields: _,
                    methods,
                    mut_methods: _,
                    generics: _,
                    doc_comments: _,
                    derived: _,
                } = type_env
                    .get_struct_def_with_type_params_mut(
                        r#struct.as_str(),
                        type_params,
                        empty_range(),
                    )
                    .clone();

                let has_method_with_name = methods.iter().any(|f| f.name.as_str() == name.as_str());

                let has_generic_method_with_name = type_env
                    .get_generic_methods(struct_name.clone())
                    .iter()
                    .any(|x| x.name.as_str() == name.as_str());

                return has_method_with_name || has_generic_method_with_name;
            }
            _ => false,
        }
    }

    pub fn has_field_by_name(&self, name: String, type_env: &mut TypeEnv) -> bool {
        match self {
            Self::Tuple(fields) => {
                if let Ok(tuple_access_idx) = name.parse::<usize>() {
                    fields.len() > tuple_access_idx
                } else {
                    if fields.is_empty() {
                        // i disabled this because it messed up auto_traits.duck test
                        // println!(
                        //     "{} it could be that the function you're trying to call is missing it's return, maybe that's why it's an empty tuple",
                        //     Tag::Note
                        // )
                    }
                    false
                }
            }
            Self::Struct {
                name: struct_name,
                type_params,
            } => {
                let StructDefinition {
                    name: _,
                    fields,
                    methods: _,
                    mut_methods: _,
                    generics: _,
                    doc_comments: _,
                    derived: _,
                } = type_env.get_struct_def_with_type_params_mut(
                    struct_name,
                    type_params,
                    empty_range(),
                );

                fields.iter().any(|f| f.name.as_str() == name.as_str())
            }
            Self::NamedDuck {
                name: duck_name,
                type_params,
            } => {
                let NamedDuckDefinition {
                    name: _,
                    fields,
                    generics: _,
                } = type_env.get_duck_def_with_type_params_mut(
                    duck_name,
                    type_params,
                    empty_range(),
                );

                fields.iter().any(|f| f.name.as_str() == name.as_str())
            }
            Self::Duck(duck) => duck
                .fields
                .iter()
                .any(|struct_field| *struct_field.name == name),
            _ => false,
        }
    }

    pub fn has_extension_by_name(&self, name: String, type_env: &mut TypeEnv) -> bool {
        let extension_fn_name = &self.build_extension_access_function_name(&name, type_env);
        return type_env
            .extension_functions
            .contains_key(&extension_fn_name.clone());
    }

    pub fn ref_has_field_by_name(&self, name: String, type_env: &mut TypeEnv) -> bool {
        match self {
            Self::Ref(t) | Self::RefMut(t) => {
                t.0.has_field_by_name(name.clone(), type_env)
                    || t.0.ref_has_field_by_name(name.clone(), type_env)
            }
            _ => false,
        }
    }

    pub fn ref_has_method_by_name(&self, name: String, type_env: &mut TypeEnv) -> bool {
        match self {
            Self::Ref(t) | Self::RefMut(t) => {
                t.0.has_method_by_name(name.clone(), type_env)
                    || t.0.ref_has_method_by_name(name.clone(), type_env)
            }
            _ => false,
        }
    }

    pub fn typeof_field(&self, field_name: String, type_env: &mut TypeEnv) -> Option<TypeExpr> {
        if self.implements_into_iter(type_env)
            && field_name.as_str() == "iter"
            && let TypeExpr::Array(inner) = self
        {
            return Some(TypeExpr::Fun(
                vec![],
                Box::new((
                    TypeExpr::Struct {
                        name: mangle(&["std", "col", "Iter"]),
                        type_params: vec![(TypeExpr::Ref(inner.as_ref().clone().into()), inner.1)],
                    },
                    empty_range(),
                )),
                false,
            ));
        }

        if field_name.as_str() == "len"
            && let TypeExpr::Array(..) = self
        {
            return Some(TypeExpr::Fun(
                vec![],
                Box::new((TypeExpr::Int, empty_range())),
                false,
            ));
        }

        if self.implements_into_iter_mut(type_env)
            && field_name.as_str() == "iter_mut"
            && let TypeExpr::Array(inner) = self
        {
            return Some(TypeExpr::Fun(
                vec![],
                Box::new((
                    TypeExpr::Struct {
                        name: mangle(&["std", "col", "Iter"]),
                        type_params: vec![(
                            TypeExpr::RefMut(inner.as_ref().clone().into()),
                            inner.1,
                        )],
                    },
                    empty_range(),
                )),
                false,
            ));
        }

        if self.implements_to_string(type_env) && field_name.as_str() == "to_string" {
            return Some(TypeExpr::Fun(
                vec![],
                Box::new((TypeExpr::String(None), empty_range())),
                false,
            ));
        }

        if self.implements_to_json(type_env) && field_name.as_str() == "to_json" {
            return Some(TypeExpr::Fun(
                vec![],
                Box::new((TypeExpr::String(None), empty_range())),
                false,
            ));
        }

        if self.implements_clone(type_env) && field_name.as_str() == "clone" {
            return Some(TypeExpr::Fun(
                vec![],
                Box::new((self.clone(), empty_range())),
                false,
            ));
        }

        if self.implements_hash(type_env) && field_name.as_str() == "hash" {
            return Some(TypeExpr::Fun(
                vec![],
                Box::new((TypeExpr::Int, empty_range())),
                false,
            ));
        }

        if self.implements_ord(type_env) && field_name.as_str() == "ord" {
            return Some(TypeExpr::Fun(
                vec![(
                    "other".to_string().into(),
                    TypeExpr::Ref(self.clone().into_empty_span().into()).into_empty_span(),
                )],
                Box::new(TypeExpr::ord_result().into_empty_span()),
                false,
            ));
        }

        Some(match self {
            Self::Tuple(fields) => fields[field_name.parse::<usize>().unwrap()].0.clone(),
            Self::Struct {
                name: r#struct,
                type_params,
            } => {
                let StructDefinition {
                    name,
                    fields,
                    methods,
                    mut_methods: _,
                    generics: _,
                    doc_comments: _,
                    derived: _,
                } = type_env
                    .get_struct_def_with_type_params_mut(
                        r#struct.as_str(),
                        type_params,
                        empty_range(),
                    )
                    .clone();

                fields
                    .iter()
                    .map(|x| (x.name.clone(), x.type_expr.0.clone()))
                    .chain(methods.iter().map(|x| {
                        (
                            x.name.clone(),
                            TypeExpr::Fun(
                                x.params
                                    .iter()
                                    .map(|x| (Some(x.0.clone()), x.1.clone()))
                                    .collect(),
                                Box::new(x.return_type.clone()),
                                true,
                            ),
                        )
                    }))
                    .chain(
                        type_env
                            .generic_methods_generated
                            .get(name.as_str())
                            .unwrap_or(&vec![])
                            .iter()
                            .map(|x| {
                                (
                                    x.name.clone(),
                                    TypeExpr::Fun(
                                        x.params
                                            .iter()
                                            .map(|x| (Some(x.0.clone()), x.1.clone()))
                                            .collect(),
                                        Box::new(x.return_type.clone()),
                                        true,
                                    ),
                                )
                            }),
                    )
                    .find(|struct_field| struct_field.0 == field_name)
                    .expect("Tried to access field that doesn't exist")
                    .1
                    .clone()
            }
            Self::NamedDuck { name, type_params } => {
                let NamedDuckDefinition {
                    name: _,
                    fields,
                    generics: _,
                } = type_env
                    .get_duck_def_with_type_params_mut(name, type_params, empty_range())
                    .clone();

                fields
                    .iter()
                    .map(|x| (x.name.clone(), x.type_expr.0.clone()))
                    .find(|duck_field| duck_field.0 == field_name)
                    .expect("Tried to access field that doesn't exist")
                    .1
                    .clone()
            }
            Self::Duck(duck) => duck
                .fields
                .iter()
                .find(|struct_field| *struct_field.name == field_name)
                .expect("Tried to access field that doesn't exist")
                .type_expr
                .0
                .clone(),
            _ => {
                let extension_fn_name =
                    self.build_extension_access_function_name(&field_name, type_env);
                let extension = type_env.extension_functions.get(&extension_fn_name);
                match extension {
                    Some(extension_def) => return Some(extension_def.0.0.clone()),
                    None => return None,
                }
            }
        })
    }

    pub fn ref_typeof_field(&self, field_name: String, type_env: &mut TypeEnv) -> Option<TypeExpr> {
        match self {
            Self::Ref(t) | Self::RefMut(t) => {
                t.0.typeof_field(field_name.clone(), type_env)
                    .or_else(|| t.0.ref_typeof_field(field_name.clone(), type_env))
            }
            _ => None,
        }
    }

    pub fn is_number(&self) -> bool {
        return *self == TypeExpr::Float
            || matches!(*self, TypeExpr::Int)
            || matches!(*self, TypeExpr::UInt)
            || matches!(*self, TypeExpr::Byte);
    }

    pub fn is_tuple(&self) -> bool {
        return matches!(*self, TypeExpr::Tuple(..));
    }

    pub fn is_bool(&self) -> bool {
        return matches!(*self, TypeExpr::Bool(..));
    }

    pub fn is_fun(&self) -> bool {
        return matches!(*self, TypeExpr::Fun(..));
    }

    pub fn is_char(&self) -> bool {
        return *self == TypeExpr::Char;
    }

    pub fn is_float(&self) -> bool {
        return *self == TypeExpr::Float;
    }

    pub fn is_string(&self) -> bool {
        return matches!(*self, TypeExpr::String(..)) || {
            let TypeExpr::Or(variants) = self else {
                return false;
            };
            return !variants.iter().any(|variant| !variant.0.is_string());
        };
    }

    pub fn is_array(&self) -> bool {
        return matches!(*self, TypeExpr::Array(..));
    }

    pub fn ref_is_array(&self) -> bool {
        match self {
            TypeExpr::Ref(v) | TypeExpr::RefMut(v) => v.0.is_array() || v.0.ref_is_array(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        return matches!(self, TypeExpr::Int);
    }

    pub fn holds_const_value(&self) -> bool {
        // todo(@Mvmo) Implement other literal types
        // floats, chars.... missing
        return matches!(*self, TypeExpr::Int)
            || matches!(*self, TypeExpr::String(..))
            || matches!(*self, TypeExpr::Bool(..));
    }

    pub fn is_variant(&self) -> bool {
        return matches!(&self, TypeExpr::Or(..));
    }

    pub fn is_primitive(&self) -> bool {
        return match *self {
            TypeExpr::Float
            | TypeExpr::Char
            | TypeExpr::Int
            | TypeExpr::UInt
            | TypeExpr::String(..)
            | TypeExpr::Bool(..) => true,
            _ => false,
        };
    }

    pub fn is_tag(&self) -> bool {
        return match *self {
            TypeExpr::Tag(..) => true,
            _ => false,
        };
    }
}

fn require(condition: bool, fail_message: String) {
    if !condition {
        println!("TypeError: {fail_message}");
        process::exit(2);
    }
}

pub fn assert_type_is_exactly_the_same(
    required_type: &Spanned<TypeExpr>,
    given_type: &Spanned<TypeExpr>,
    type_env: &mut TypeEnv,
) {
    let msg = "These types need to be the same";
    if required_type.0.type_id(type_env) != given_type.0.type_id(type_env) {
        failure_with_occurence(
            msg,
            given_type.1,
            [(msg, required_type.1), (msg, given_type.1)],
        );
    }
}

pub fn check_type_compatability(
    required_type: &Spanned<TypeExpr>,
    given_type: &Spanned<TypeExpr>,
    type_env: &mut TypeEnv,
) {
    check_type_compatability_full(required_type, given_type, type_env, false);
}

pub fn check_type_compatability_full(
    required_type: &Spanned<TypeExpr>,
    given_type: &Spanned<TypeExpr>,
    type_env: &mut TypeEnv,
    given_const_var: bool,
) {
    let given_type = given_type.clone();

    if matches!(given_type.0, TypeExpr::Uninit) {
        panic!("un init type");
    }

    if matches!(given_type.0, TypeExpr::TemplParam(..) | TypeExpr::Never) {
        return;
    }

    let fail_requirement = |explain_required: String, explain_given: String| {
        let (smaller, larger) = if required_type.1.start <= given_type.1.start {
            (required_type.1, given_type.1)
        } else {
            (given_type.1, required_type.1)
        };

        // this is unused at the moment but come in handy later
        let _combined_span = SS {
            start: smaller.start,
            end: larger.end,
            context: required_type.1.context,
        };

        failure_with_occurence(
            "Incompatible Types",
            given_type.1,
            vec![
                (explain_required.to_string(), required_type.1),
                (explain_given.to_string(), given_type.1),
            ],
        )
    };

    let is_empty_tuple = if let TypeExpr::Tuple(t) = &required_type.0 {
        t.is_empty()
    } else {
        false
    };

    if !is_empty_tuple && matches!(given_type.0, TypeExpr::Statement) {
        let msg = "Statement is not an expression";
        failure_with_occurence(
            msg,
            given_type.1,
            [(
                "This needs an expression, you provided a statement",
                required_type.1,
            )],
        );
    }

    match &required_type.0 {
        TypeExpr::Uninit => unreachable!("uninit types should have been replaced by now"),
        TypeExpr::Indexed(..) => unreachable!("indexed types should have been replaced by now"),
        TypeExpr::Byte => {
            if !matches!(given_type.0, TypeExpr::Byte) {
                fail_requirement(
                    "this expects a Byte.".to_string(),
                    format!(
                        "this is not a Byte. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::UInt => {
            if !matches!(given_type.0, TypeExpr::UInt) {
                fail_requirement(
                    "this expects a UInt.".to_string(),
                    format!(
                        "this is not a UInt. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::Statement => panic!("Compiler Bug: statement should never be required"),
        TypeExpr::TemplParam(..) | TypeExpr::Never => return,
        TypeExpr::NamedDuck { name, type_params } => {
            let def = type_env
                .get_duck_def_with_type_params_mut(name, type_params, required_type.1)
                .clone();
            let is_matched = match &given_type.0 {
                TypeExpr::Duck(Duck { fields }) => {
                    fields.len() >= def.fields.len()
                        && def
                            .fields
                            .iter()
                            .zip(fields.iter())
                            .all(|(def_field, given_field)| {
                                check_type_compatability(
                                    &def_field.type_expr,
                                    &given_field.type_expr,
                                    type_env,
                                );
                                def_field.name == given_field.name
                            })
                }
                TypeExpr::Struct { name, type_params } => {
                    let s_def = type_env
                        .get_struct_def_with_type_params_mut(name, type_params, given_type.1)
                        .clone();
                    let struct_fields = s_def
                        .fields
                        .iter()
                        .map(|f| (f.name.clone(), f.type_expr.clone()))
                        .chain(s_def.methods.iter().map(|m| {
                            let mut method_type = m.type_expr();
                            let TypeExpr::Fun(_, _, ref mut is_mut) = method_type.0 else {
                                panic!("how is this not a method {method_type:?}")
                            };
                            *is_mut = s_def.mut_methods.contains(&m.name);
                            (m.name.clone(), method_type)
                        }))
                        .fold(HashMap::new(), |mut acc, (name, ty)| {
                            acc.insert(name, ty);
                            acc
                        });
                    for def_field in &def.fields {
                        if let Some(field_type) = struct_fields.get(&def_field.name) {
                            check_type_compatability(&def_field.type_expr, field_type, type_env);
                        } else {
                            panic!("field {} not found", def_field.name);
                        }
                    }
                    true
                }
                TypeExpr::NamedDuck {
                    name: given_name,
                    type_params,
                } => {
                    if !type_params.is_empty() {
                        todo!("type params not impl yet");
                    }

                    given_name == &def.name
                }
                _ => false,
            };

            if !is_matched {
                panic!("mismatch {name} {:?}", given_type.0);
            }
        }
        TypeExpr::Ref(req_t) => {
            if let TypeExpr::Ref(given_t) | TypeExpr::RefMut(given_t) = &given_type.0 {
                let mut req_x = req_t.clone();
                let mut given_x = given_t.clone();

                loop {
                    match req_x.0 {
                        TypeExpr::Ref(inner_ty) => {
                            req_x = inner_ty;
                            if let TypeExpr::Ref(inner_given_ty)
                            | TypeExpr::RefMut(inner_given_ty) = given_x.0
                            {
                                given_x = inner_given_ty;
                            } else if req_x.0.type_id(type_env) != given_x.0.type_id(type_env) {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                        TypeExpr::RefMut(inner_ty) => {
                            req_x = inner_ty;
                            if let TypeExpr::RefMut(inner_given_ty) = given_x.0 {
                                given_x = inner_given_ty;
                            } else if req_x.0.type_id(type_env) != given_x.0.type_id(type_env) {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                        _ => {
                            if req_x.0.type_id(type_env) == given_x.0.type_id(type_env) {
                                break;
                            } else {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                    }
                }
            } else {
                fail_requirement(
                    format!(
                        "This is an immutable reference to {}",
                        required_type.0.as_clean_user_faced_type_name().yellow()
                    ),
                    format!(
                        "So this needs to be an immutable reference or a mutable reference but it's of type {}",
                        given_type.0.as_clean_user_faced_type_name().yellow()
                    ),
                );
            }
        }
        TypeExpr::RefMut(req_t) => {
            if let TypeExpr::RefMut(given_t) = &given_type.0 {
                let mut req_x = req_t.clone();
                let mut given_x = given_t.clone();

                loop {
                    match req_x.0 {
                        TypeExpr::Ref(inner_ty) => {
                            req_x = inner_ty;
                            if let TypeExpr::Ref(inner_given_ty)
                            | TypeExpr::RefMut(inner_given_ty) = given_x.0
                            {
                                given_x = inner_given_ty;
                            } else if req_x.0.type_id(type_env) != given_x.0.type_id(type_env) {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                        TypeExpr::RefMut(inner_ty) => {
                            req_x = inner_ty;
                            if let TypeExpr::RefMut(inner_given_ty) = given_x.0 {
                                given_x = inner_given_ty;
                            } else if req_x.0.type_id(type_env) != given_x.0.type_id(type_env) {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                        _ => {
                            if req_x.0.type_id(type_env) == given_x.0.type_id(type_env) {
                                break;
                            } else {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                    }
                }
            } else {
                fail_requirement(
                    "This is a mutable reference".to_string(),
                    "So this needs to be a mutable reference as well".to_string(),
                );
            }
        }
        TypeExpr::Html => {
            if let TypeExpr::Html = &given_type.0 {
                return;
            }
            fail_requirement(
                format!(
                    "the required type is {}",
                    format!("{}", required_type.0).bright_yellow(),
                ),
                format!(
                    "because of the fact, that the required type is {}. The value you need to pass must be a tag aswell, but it is a {}",
                    format!("{}", required_type.0).bright_yellow(),
                    format!("{}", given_type.0).bright_yellow(),
                ),
            )
        }
        TypeExpr::TypeOf(..) => panic!("typeof should have been replaced"),
        TypeExpr::KeyOf(..) => panic!("keyof should have been replaced"),
        TypeExpr::Any => return,
        TypeExpr::Go(_) => return,
        TypeExpr::Tag(required_identifier) => {
            if let TypeExpr::Tag(given_identifier) = &given_type.0 {
                if given_identifier != required_identifier {
                    fail_requirement(
                        format!(
                            "the required tag is {}",
                            format!(".{required_identifier}").bright_yellow(),
                        ),
                        format!(
                            "but you've provided the tag {}",
                            format!(".{given_identifier}").bright_yellow(),
                        ),
                    )
                }

                // everything's okay
                return;
            }

            // todo: produce snapshot for the given error
            fail_requirement(
                format!(
                    "the required type is {}",
                    format!("{}", required_type.0).bright_yellow(),
                ),
                format!(
                    "because of the fact, that the required type is {}. The value you need to pass must be a tag aswell, but it is a {}",
                    format!("{}", required_type.0).bright_yellow(),
                    format!("{}", given_type.0).bright_yellow(),
                ),
            )
        }
        TypeExpr::Struct {
            name: req_name,
            type_params: req_type_params,
        } => {
            if let TypeExpr::Struct {
                name: given_name,
                type_params: given_type_params,
            } = &given_type.0
            {
                // TODO: STRUCT DISPLAY NAME
                if req_name.as_str() != given_name.as_str() {
                    fail_requirement(
                        format!("the required struct is {}", req_name.bright_yellow(),),
                        format!("you have given a {}", given_name.bright_yellow(),),
                    );
                }

                for (idx, (req_param, given_param)) in req_type_params
                    .iter()
                    .zip(given_type_params.iter())
                    .enumerate()
                {
                    if req_param.0.type_id(type_env) != given_param.0.type_id(type_env) {
                        failure_with_occurence(
                            "Type parameters do not match",
                            given_param.1,
                            [
                                (
                                    format!(
                                        "type parameter no. {} is required to be a {}",
                                        idx + 1,
                                        req_param.0.as_clean_user_faced_type_name()
                                    ),
                                    req_param.1,
                                ),
                                (
                                    format!(
                                        "you have given a {}",
                                        given_param.0.as_clean_user_faced_type_name()
                                    ),
                                    given_param.1,
                                ),
                            ],
                        );
                    }
                }
            } else {
                fail_requirement(
                    format!(
                        "the required type {} is a struct",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                    format!(
                        "because of the fact, that the required type {} is a struct. The value you need to pass must be a aswell, but it isn't. {:?}",
                        format!("{}", required_type.0).bright_yellow(),
                        given_type.0,
                    ),
                )
            }
        }
        TypeExpr::Duck(duck) => {
            if duck.fields.is_empty() {
                return;
            }

            match &given_type.0 {
                TypeExpr::Duck(given_duck) => {
                    let required_duck = duck;

                    for required_field in required_duck.fields.iter() {
                        let companion_field = given_duck
                            .fields
                            .iter()
                            .find(|field| field.name == required_field.name);

                        if companion_field.is_none() {
                            fail_requirement(
                                format!(
                                    "this type states that it has requires a field {} of type {}",
                                    required_field.name.bright_purple(),
                                    format!("{}", required_field.type_expr.0).bright_yellow(),
                                ),
                                format!(
                                    "the given type doesn't have a field {}",
                                    required_field.name.bright_purple(),
                                ),
                            )
                        }

                        let companion_field = companion_field.unwrap();

                        check_type_compatability(
                            &required_field.type_expr,
                            &companion_field.type_expr,
                            type_env,
                        );
                    }
                }
                TypeExpr::NamedDuck {
                    name: _,
                    type_params: _,
                } => {
                    assert_eq!(
                        required_type.0.type_id(type_env),
                        given_type.0.type_id(type_env)
                    );
                    return;
                }
                TypeExpr::Struct {
                    name: struct_name,
                    type_params,
                } => {
                    let struct_def = type_env
                        .get_struct_def_with_type_params_mut(
                            struct_name,
                            type_params,
                            required_type.1,
                        )
                        .clone();

                    for required_field in duck.fields.iter() {
                        if let TypeExpr::Fun(_, _, is_mut) = required_field.type_expr.0 {
                            let companion_method = struct_def
                                .methods
                                .iter()
                                .find(|method| method.name == required_field.name);

                            if companion_method.is_none() {
                                fail_requirement(
                                    format!(
                                        "this type states that it requires a field {} of type {}",
                                        required_field.name.bright_purple(),
                                        format!("{}", required_field.type_expr.0).bright_yellow(),
                                    ),
                                    format!(
                                        "the given type doesn't have a field or method with name {}",
                                        required_field.name.bright_purple(),
                                    ),
                                );
                            }

                            if is_mut {
                                if !struct_def.mut_methods.contains(&required_field.name) {
                                    fail_requirement(
                                        format!(
                                            "this type states that it requires a mutable method named {}",
                                            required_field.name.bright_purple(),
                                        ),
                                        format!(
                                            "the given type doesn't have a mutable method named {}",
                                            required_field.name.bright_purple(),
                                        ),
                                    );
                                }
                                if given_const_var
                                    && !{
                                        let mut is_mut_ref = false;

                                        let mut current = given_type.0.clone();
                                        while let TypeExpr::RefMut(next) = current {
                                            is_mut_ref = true;

                                            if let TypeExpr::Ref(..) = next.0 {
                                                is_mut_ref = false;
                                                break;
                                            }

                                            current = next.0;
                                        }

                                        is_mut_ref
                                    }
                                {
                                    fail_requirement(
                                        "this needs mutable access".to_string(),
                                        "this is a const var".to_string(),
                                    );
                                }
                            }

                            let companion_method = companion_method.unwrap();
                            check_type_compatability(
                                &required_field.type_expr,
                                &companion_method.type_expr(),
                                type_env,
                            );
                            return;
                        }

                        let companion_field = struct_def
                            .fields
                            .iter()
                            .find(|field| field.name == required_field.name);

                        if companion_field.is_none() {
                            fail_requirement(
                                format!(
                                    "this type states that it has requires a field {} of type {}",
                                    required_field.name.bright_purple(),
                                    format!("{}", required_field.type_expr.0).bright_yellow(),
                                ),
                                format!(
                                    "the given type doesn't have a field {}",
                                    required_field.name.bright_purple(),
                                ),
                            )
                        }

                        let companion_field = companion_field.unwrap();

                        check_type_compatability(
                            &required_field.type_expr,
                            &companion_field.type_expr,
                            type_env,
                        );
                    }
                }
                _ => fail_requirement(
                    format!(
                        "the required type {} is a duck",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                    format!(
                        "this must be a duck as well, but it's {}",
                        format!("{}", given_type.0).bright_yellow(),
                    ),
                ),
            }
        }
        TypeExpr::Tuple(item_types) => {
            if item_types.is_empty() && matches!(given_type.0, TypeExpr::Statement) {
                return;
            }
            if !given_type.0.is_tuple() {
                fail_requirement(
                    format!(
                        "{} is a tuple",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                    String::new(),
                )
            }

            let required_item_types = item_types;
            let TypeExpr::Tuple(given_item_types) = &given_type.0 else {
                unreachable!()
            };

            if given_item_types.len() < required_item_types.len() {
                fail_requirement(
                    format!(
                        "requires {} item(s)",
                        format!("{}", required_item_types.len()).bright_green(),
                    ),
                    format!(
                        "only has {} item(s)",
                        format!("{}", given_item_types.len()).bright_green(),
                    ),
                )
            }

            for (idx, (req_param, given_param)) in required_item_types
                .iter()
                .zip(given_item_types.iter())
                .enumerate()
            {
                if let TypeExpr::Or(req_variants) = &req_param.0
                    && req_variants.iter().any(|variant| {
                        variant.0.type_id(type_env) == given_param.0.type_id(type_env)
                    })
                {
                    return;
                }

                if req_param.0.type_id(type_env) != given_param.0.type_id(type_env) {
                    failure_with_occurence(
                        "Incompatible Types",
                        given_param.1,
                        [
                            (
                                format!(
                                    "item no. {} is required to be a {}",
                                    idx + 1,
                                    req_param.0.as_clean_user_faced_type_name()
                                ),
                                req_param.1,
                            ),
                            (
                                format!(
                                    "you have given a {}",
                                    given_param.0.as_clean_user_faced_type_name()
                                ),
                                given_param.1,
                            ),
                        ],
                    );
                }
            }
        }
        TypeExpr::String(..) => {
            if !given_type.0.is_string() {
                fail_requirement(
                    format!("this expects a {}", "String".yellow()),
                    format!(
                        "this is not a {}. It's of type {}",
                        "String".yellow(),
                        given_type.0.as_clean_user_faced_type_name().yellow()
                    ),
                );
            }
        }
        TypeExpr::Int => {
            if !matches!(given_type.0, TypeExpr::Int) {
                fail_requirement(
                    "this expects an Int.".to_string(),
                    format!(
                        "this is not an Int. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::Bool(..) => {
            if !given_type.0.is_bool() {
                fail_requirement(
                    format!("a {} value is required here", "Bool".bright_yellow(),),
                    format!(
                        "this is not a Bool. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::Char => {
            if !given_type.0.is_char() {
                fail_requirement(
                    "this expects a Char.".to_string(),
                    "this is not a Char.".to_string(),
                );
            }
        }
        TypeExpr::Float => {
            if !matches!(given_type.0, TypeExpr::Float) {
                fail_requirement(
                    "this expects a Float.".to_string(),
                    format!(
                        "this is not a Float. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::Or(contents) => {
            let other_contents = if let TypeExpr::Or(other_contents) = &given_type.0 {
                if other_contents.len() > contents.len() {
                    fail_requirement(
                        "This union is smaller than".to_string(),
                        "this one".to_string(),
                    );
                }
                other_contents
            } else {
                &vec![given_type.clone()]
            };

            for giv in other_contents.iter() {
                let found = contents
                    .iter()
                    .any(|c| c.0.type_id(type_env) == giv.0.type_id(type_env));
                if !found {
                    let msg = format!(
                        "This expression is of type `{}`",
                        giv.0.as_clean_user_faced_type_name().blue(),
                    );
                    let msg = msg.as_str();

                    failure_with_occurence(
                        "Incompatible Types",
                        giv.1,
                        [
                            (msg, giv.1),
                            (
                                format!(
                                    "Must be one of these: {}",
                                    contents
                                        .iter()
                                        .map(|c| c.0.as_clean_user_faced_type_name())
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                        .yellow()
                                )
                                .as_str(),
                                required_type.1,
                            ),
                        ],
                    );
                }
            }
        }
        TypeExpr::Fun(required_params, required_return_type, is_mut_required) => {
            if !given_type.0.is_fun() {
                fail_requirement(
                    "this requires a function".to_string(),
                    "this value isn't even a function.".to_string(),
                )
            }

            let TypeExpr::Fun(given_params, given_return_type, is_mut_given) = &given_type.0 else {
                unreachable!("we've already checked that it's a function")
            };

            if !*is_mut_required && *is_mut_given {
                fail_requirement(
                    "this requires a function that does not modify its environment".to_string(),
                    "thus, this must not be a mut fn".to_string(),
                );
            }

            if given_params.len() != required_params.len() {
                fail_requirement(
                    format!(
                        "this requires a function with {} argument(s)",
                        format!("{}", required_params.len()).bright_green(),
                    ),
                    format!(
                        "this is a function, but it takes {} arguments(s)",
                        format_args!("{}", given_params.len())
                    ),
                )
            }

            for (idx, ((_, req_param), (_, given_param))) in
                required_params.iter().zip(given_params.iter()).enumerate()
            {
                if req_param.0.type_id(type_env) != given_param.0.type_id(type_env) {
                    failure_with_occurence(
                        "Parameter types do not match",
                        given_param.1,
                        [
                            (
                                format!(
                                    "parameter no. {} is required to be a {}",
                                    idx + 1,
                                    req_param.0.as_clean_user_faced_type_name()
                                ),
                                req_param.1,
                            ),
                            (
                                format!(
                                    "you have given a {}",
                                    given_param.0.as_clean_user_faced_type_name()
                                ),
                                given_param.1,
                            ),
                        ],
                    );
                }
            }
            if required_return_type.0.type_id(type_env) != given_return_type.0.type_id(type_env) {
                failure_with_occurence(
                    "Return types do not match",
                    given_return_type.1,
                    [
                        (
                            format!(
                                "Return type needs to be a {}",
                                required_return_type.0.as_clean_user_faced_type_name()
                            ),
                            required_return_type.1,
                        ),
                        (
                            format!(
                                "You have given a {}",
                                given_return_type.0.as_clean_user_faced_type_name(),
                            ),
                            given_return_type.1,
                        ),
                    ],
                );
            }
        }

        TypeExpr::Array(content_type) => {
            if !given_type.0.is_array() {
                fail_requirement(
                    format!("this requires an array of {}", &content_type.0),
                    "this is not an array".to_string(),
                );
            }

            let TypeExpr::Array(given_content_type) = given_type.clone().0 else {
                unreachable!("we've checked that given_type is an array")
            };

            if content_type.0.type_id(type_env) != given_content_type.0.type_id(type_env) {
                failure_with_occurence(
                    "Array content types do not match",
                    given_content_type.1,
                    [
                        (
                            format!(
                                "This requires an array of {}",
                                content_type.0.as_clean_user_faced_type_name().yellow()
                            ),
                            content_type.1,
                        ),
                        (
                            format!(
                                "You have given an array of {}",
                                given_content_type
                                    .0
                                    .as_clean_user_faced_type_name()
                                    .yellow()
                            ),
                            given_content_type.1,
                        ),
                    ],
                )
            }
        }
        TypeExpr::RawTypeName(..) | TypeExpr::TypeName(..) | TypeExpr::And(..) => {
            panic!("{required_type:?} should not be here")
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        parse::{
            SS,
            function_parser::FunctionDefintion,
            lexer::lex_parser,
            make_input,
            source_file_parser::SourceFile,
            value_parser::{empty_range, type_expr_into_empty_range, value_expr_parser},
        },
        semantics::type_resolve::typeresolve_source_file,
    };
    use chumsky::prelude::*;

    use super::*;

    #[test]
    fn test_typeresolve() {
        let src_and_expected_type_vec = vec![
            // todo: ("4 + 4", TypeExpr::Int(Some("4"))),
            ("\"Hallo\"", TypeExpr::String(None)),
            (
                "{ x: \"hallo\", }",
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new(
                        "x".to_string(),
                        TypeExpr::String(None).into_empty_span(),
                    )],
                }),
            ),
            ("0.5", TypeExpr::Float),
            ("0.1 + 0.4", TypeExpr::Float),
            ("0.0 + 0.4", TypeExpr::Float),
            ("0.4 + 0.0", TypeExpr::Float),
            (
                "(0, 2)",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::Int.into_empty_span(),
                ]),
            ),
            (
                "(0, 2 + 2)",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::Int.into_empty_span(),
                ]),
            ),
            (
                "(0, (2 + 2, 5))",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::Tuple(vec![
                        TypeExpr::Int.into_empty_span(),
                        TypeExpr::Int.into_empty_span(),
                    ])
                    .into_empty_span(),
                ]),
            ),
            (
                "(0, (\"Hallo, Welt\", 5))",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::Tuple(vec![
                        TypeExpr::String(None).into_empty_span(),
                        TypeExpr::Int.into_empty_span(),
                    ])
                    .into_empty_span(),
                ]),
            ),
            ("{ let x: Int = 5; 5 }", TypeExpr::Int),
            ("{ let x: Int = 5; x }", TypeExpr::Int),
            ("{ let x: Int = 5; x * x }", TypeExpr::Int),
        ];

        for (src, expected_type_expr) in src_and_expected_type_vec {
            println!("lexing {src}");
            let lexer_parse_result = lex_parser("test", "").parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {src}");
            let value_expr_parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(value_expr_parse_result.has_errors(), false);
            assert_eq!(value_expr_parse_result.has_output(), true);

            let value_expr = value_expr_parse_result.into_output().unwrap();
            let mut source_file = SourceFile {
                function_definitions: vec![FunctionDefintion {
                    name: "main".to_string(),
                    params: vec![],
                    return_type: TypeExpr::unit(),
                    value_expr: value_expr,
                    generics: vec![],
                    span: empty_range(),
                    comments: Vec::new(),
                }],
                ..Default::default()
            };

            let mut type_env = TypeEnv::default();
            typeresolve_source_file(&mut source_file, &mut type_env);

            let mut type_expr = (
                TypeExpr::from_value_expr(
                    &source_file
                        .function_definitions
                        .get(0)
                        .unwrap()
                        .value_expr
                        .expr,
                    &mut type_env,
                ),
                empty_range(),
            );
            type_expr_into_empty_range(&mut type_expr.0);

            assert_eq!(type_expr.0.0, expected_type_expr);
        }
    }

    #[test]
    fn test_type_compatibility_success() {
        let mut type_env = TypeEnv::default();

        let success_cases = vec![
            (TypeExpr::Int, TypeExpr::Int),
            (TypeExpr::String(None), TypeExpr::String(None)),
            (TypeExpr::UInt, TypeExpr::UInt),
            (TypeExpr::Float, TypeExpr::Float),
            (
                TypeExpr::Tuple(vec![
                    empty_spanned(TypeExpr::Int),
                    empty_spanned(TypeExpr::String(None)),
                ]),
                TypeExpr::Tuple(vec![
                    empty_spanned(TypeExpr::Int),
                    empty_spanned(TypeExpr::String(None)),
                ]),
            ),
            (
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::Int))],
                }),
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::Int))],
                }),
            ),
            (
                TypeExpr::Or(vec![
                    empty_spanned(TypeExpr::Int),
                    empty_spanned(TypeExpr::String(None)),
                ]),
                TypeExpr::Int,
            ),
            (
                TypeExpr::Or(vec![
                    empty_spanned(TypeExpr::Int),
                    empty_spanned(TypeExpr::String(None)),
                    empty_spanned(TypeExpr::Bool(None)),
                ]),
                TypeExpr::Or(vec![
                    empty_spanned(TypeExpr::String(None)),
                    empty_spanned(TypeExpr::Int),
                ]),
            ),
        ];

        for (one, two) in success_cases {
            check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
        }
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_primitives() {
        let mut type_env = TypeEnv::default();
        check_type_compatability(
            &empty_spanned(TypeExpr::Int),
            &empty_spanned(TypeExpr::String(None)),
            &mut type_env,
        );
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_str() {
        let mut type_env = TypeEnv::default();
        check_type_compatability(
            &empty_spanned(TypeExpr::String(None)),
            &empty_spanned(TypeExpr::Int),
            &mut type_env,
        );
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_number_and_string() {
        let mut type_env = TypeEnv::default();
        check_type_compatability(
            &empty_spanned(TypeExpr::Float),
            &empty_spanned(TypeExpr::String(None)),
            &mut type_env,
        );
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_tuples_different_types() {
        let mut type_env = TypeEnv::default();

        let one = TypeExpr::Tuple(vec![empty_spanned(TypeExpr::Int)]);
        let two = TypeExpr::Tuple(vec![empty_spanned(TypeExpr::String(None))]);

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_tuples_different_length() {
        let mut type_env = TypeEnv::default();

        let one = TypeExpr::Tuple(vec![
            empty_spanned(TypeExpr::Int),
            empty_spanned(TypeExpr::Int),
            empty_spanned(TypeExpr::Int),
        ]);
        let two = TypeExpr::Tuple(vec![
            empty_spanned(TypeExpr::Int),
            empty_spanned(TypeExpr::Int),
        ]);

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_ducks_different_field_names() {
        let mut type_env = TypeEnv::default();

        let one = TypeExpr::Duck(Duck {
            fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::Int))],
        });

        let two = TypeExpr::Duck(Duck {
            fields: vec![Field::new("y".to_string(), empty_spanned(TypeExpr::Int))],
        });

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_ducks_different_field_types() {
        let mut type_env = TypeEnv::default();

        let one = TypeExpr::Duck(Duck {
            fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::Int))],
        });

        let two = TypeExpr::Duck(Duck {
            fields: vec![Field::new(
                "x".to_string(),
                empty_spanned(TypeExpr::String(None)),
            )],
        });

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_type_not_in_variant() {
        let mut type_env = TypeEnv::default();

        let variant = TypeExpr::Or(vec![
            empty_spanned(TypeExpr::Int),
            empty_spanned(TypeExpr::String(None)),
        ]);
        let a_bool = TypeExpr::Bool(None);

        check_type_compatability(
            &empty_spanned(variant),
            &empty_spanned(a_bool),
            &mut type_env,
        );
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_variant_not_subset_of_variant() {
        let mut type_env = TypeEnv::default();

        let super_variant = TypeExpr::Or(vec![
            empty_spanned(TypeExpr::Int),
            empty_spanned(TypeExpr::String(None)),
        ]);
        let sub_variant = TypeExpr::Or(vec![
            empty_spanned(TypeExpr::Int),
            empty_spanned(TypeExpr::Bool(None)),
        ]);

        check_type_compatability(
            &empty_spanned(super_variant),
            &empty_spanned(sub_variant),
            &mut type_env,
        );
    }

    fn empty_spanned<T>(item: T) -> Spanned<T> {
        use crate::parse::Context as SourceFileContext;

        let context = SourceFileContext {
            file_name: "test",
            file_contents: "",
        };

        return (
            item,
            SS {
                start: 0,
                end: 0,
                context,
            },
        );
    }
}
