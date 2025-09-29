use std::panic::Location;
use std::process;

use chumsky::container::Seq;
use colored::Colorize;

use crate::parse::struct_parser::StructDefinition;
use crate::parse::type_parser::{Duck, TypeExpr};
use crate::parse::value_parser::empty_range;
use crate::parse::{Field, SS, failure_with_occurence};
use crate::parse::{
    Spanned, failure,
    value_parser::{ValFmtStringContents, ValueExpr},
};
use crate::semantics::type_resolve::TypeEnv;

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
            TypeExpr::String(..) | TypeExpr::Bool(..) | TypeExpr::Float | TypeExpr::Int(..) => true,
            _ => false,
        }
    }

    pub fn from_value_expr_resolved_type_name(
        value_expr: &Spanned<ValueExpr>,
        type_env: &mut TypeEnv,
    ) -> TypeExpr {
        let mut res = TypeExpr::from_value_expr(value_expr, type_env);
        loop {
            if let TypeExpr::TypeName(_, name, _) = &res {
                res = type_env.resolve_type_alias(name);
            } else if let TypeExpr::Alias(def) = &res {
                res = type_env.resolve_type_alias(&def.name);
            } else {
                break res;
            }
        }
    }

    pub fn from_value_expr_resolved_type_name_dereferenced(
        value_expr: &Spanned<ValueExpr>,
        type_env: &mut TypeEnv,
    ) -> TypeExpr {
        let mut res = TypeExpr::from_value_expr(value_expr, type_env);

        while let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = res {
            res = v.0;
        }

        loop {
            if let TypeExpr::TypeName(_, name, _) = &res {
                res = type_env.resolve_type_alias(name);
            } else if let TypeExpr::Alias(def) = &res {
                res = type_env.resolve_type_alias(&def.name);
            } else {
                break;
            }
        }
        res
    }

    #[track_caller]
    pub fn from_value_expr(value_expr: &Spanned<ValueExpr>, type_env: &mut TypeEnv) -> TypeExpr {
        let complete_span = &value_expr.1;
        let value_expr = &value_expr.0;

        return match value_expr {
            ValueExpr::Ref(v) => {
                TypeExpr::Ref((TypeExpr::from_value_expr(v, type_env), v.1.clone()).into())
            }
            ValueExpr::RefMut(v) => {
                TypeExpr::RefMut((TypeExpr::from_value_expr(v, type_env), v.1.clone()).into())
            }
            ValueExpr::Deref(v) => {
                let ty_expr = TypeExpr::from_value_expr(&*v, type_env);
                if !matches!(ty_expr, TypeExpr::Ref(..) | TypeExpr::RefMut(..)) {
                    failure_with_occurence(
                        complete_span.context.file_name,
                        "Can only dereference a reference".to_string(),
                        *complete_span,
                        [("This is not a reference".to_string(), v.1)],
                        complete_span.context.file_contents,
                    );
                }
                let (TypeExpr::Ref(t) | TypeExpr::RefMut(t)) = ty_expr else {
                    unreachable!()
                };
                t.0
            }
            ValueExpr::HtmlString(..) => TypeExpr::Html,
            ValueExpr::Tag(identifier) => TypeExpr::Tag(identifier.clone()),
            ValueExpr::RawVariable(_x, p) => panic!("{}", p.join(" ").leak()),
            ValueExpr::FormattedString(contents) => {
                for c in contents {
                    if let ValFmtStringContents::Expr(e) = c {
                        require(
                            // matches!(TypeExpr::from_value_expr(&e.0, type_env), TypeExpr::String) ||
                            // matches!(TypeExpr::from_value_expr(&e.0, type_env), TypeExpr::ConstString(..)),
                            TypeExpr::from_value_expr(e, type_env).is_string(),
                            "Needs to be string".into(),
                        );
                    }
                }
                TypeExpr::String(None)
            }
            ValueExpr::ArrayAccess(target, idx) => {
                let target_type = TypeExpr::from_value_expr_resolved_type_name_dereferenced(target, type_env);
                let idx_type = TypeExpr::from_value_expr(idx, type_env);

                require(
                    target_type.is_array() || target_type.ref_is_array(),
                    "Needs to be array".into(),
                );
                require(idx_type.is_int(), "Needs to be int".into());

                let TypeExpr::Array(array_type) = target_type else {
                    panic!("{target_type:?}")
                };

                array_type.0.clone()
            }
            ValueExpr::Array(optional_type_support, value_exprs) => {
                if let Some(type_support) = optional_type_support {
                    for value_expr in value_exprs {
                        let type_expr = &(
                            TypeExpr::from_value_expr(value_expr, type_env),
                            value_expr.1,
                        );
                        check_type_compatability(type_support, type_expr, type_env);
                    }

                    return TypeExpr::Array(Box::new(type_support.clone()));
                }

                let mut variants = value_exprs
                    .iter()
                    .map(|value_expr| {
                        (
                            TypeExpr::from_value_expr(value_expr, type_env),
                            value_expr.1,
                        )
                    })
                    .collect::<Vec<Spanned<TypeExpr>>>();

                variants.sort_by_key(|value_expr| value_expr.0.as_clean_go_type_name(type_env));
                variants.dedup_by_key(|value_expr| value_expr.0.as_clean_go_type_name(type_env));

                if variants.len() > 1 {
                    let start = variants
                        .first()
                        .expect("we've just checked that variants is at least 2 items long");
                    let end = variants
                        .last()
                        .expect("we've just checked that variants is at least 2 items long");

                    let combined_span = SS {
                        context: start.1.context,
                        start: start.1.start,
                        end: end.1.end,
                    };

                    return TypeExpr::Array(Box::new((TypeExpr::Or(variants), combined_span)));
                }

                if variants.is_empty() {
                    panic!(
                        "Internal Compiler Error: variants shoulnd't ever be empty, as this is a syntax error."
                    );
                }

                let first_type = variants
                    .first()
                    .expect("we've checked that variants is exactly of len 1");
                return TypeExpr::Array(Box::new(first_type.clone()));
            }
            ValueExpr::Lambda(lambda_expr) => TypeExpr::Fun(
                lambda_expr
                    .params
                    .iter()
                    .map(|(name, type_expr)| (Some(name.clone()), type_expr.clone()))
                    .collect(),
                lambda_expr.return_type.clone().map(Box::new),
            ),
            ValueExpr::InlineGo(..) => TypeExpr::InlineGo,
            ValueExpr::Int(value) => TypeExpr::Int(Some(*value)),
            ValueExpr::Bool(value) => TypeExpr::Bool(Some(*value)),
            ValueExpr::Char(..) => TypeExpr::Char,
            ValueExpr::Float(..) => TypeExpr::Float,
            ValueExpr::String(str_value, is_const) => {
                if *is_const {
                    TypeExpr::String(Some(str_value.clone()))
                } else {
                    TypeExpr::String(None)
                }
            }
            ValueExpr::Break => TypeExpr::Tuple(vec![]),
            ValueExpr::Continue => TypeExpr::Tuple(vec![]),
            ValueExpr::Return(Some(value_expr)) => TypeExpr::from_value_expr(value_expr, type_env),
            ValueExpr::Return(None) => TypeExpr::Any, // TODO return never !
            ValueExpr::VarAssign(_assignment) => TypeExpr::Tuple(vec![]),
            ValueExpr::VarDecl(decl) => {
                let decl = decl.as_ref();
                check_type_compatability(
                    decl.0.type_expr.as_ref().expect(
                        "compiler error: i expect the implicit types to be resolved by now",
                    ),
                    &(
                        TypeExpr::from_value_expr(&decl.0.initializer, type_env),
                        decl.0.initializer.1,
                    ),
                    type_env,
                );

                TypeExpr::Tuple(vec![])
            }
            ValueExpr::Struct {
                name,
                fields: value_expr_fields,
                type_params,
            } => {
                if type_params.is_some() {
                    panic!(
                        "compiler error: type params should be omitted by now {name} {type_params:?}"
                    )
                }

                let type_expr = type_env.try_resolve_type_expr(&type_env.resolve_type_alias(name));
                let TypeExpr::Struct(struct_def) = type_expr else {
                    panic!("is not a struct");
                };

                let struct_def = type_env.get_struct_def(struct_def.as_str()).clone();

                let value_expr_fields = value_expr_fields
                    .iter()
                    .map(|(name, (value_expr, span))| {
                        Field::new(
                            name.to_string(),
                            (
                                TypeExpr::from_value_expr(&(value_expr.clone(), *span), type_env),
                                *span,
                            ),
                        )
                    })
                    .collect::<Vec<Field>>();

                let is_missing_field = !struct_def.fields.iter().all(|field| {
                    let field_from_value_expr = value_expr_fields
                        .iter()
                        .find(|value_expr_field| value_expr_field.name == field.name);

                    let Some(field_from_value_expr) = field_from_value_expr else {
                        return false;
                    };

                    check_type_compatability(
                        &field.type_expr,
                        &field_from_value_expr.type_expr,
                        type_env,
                    );
                    return true;
                });

                if is_missing_field || struct_def.fields.len() != value_expr_fields.len() {
                    panic!("invalid type from value expr {is_missing_field}")
                }

                // TypeExpr::Struct(Struct { fields: types })
                // TODO: require name and implement typing for structs
                TypeExpr::Struct(struct_def.name.clone())
            }
            ValueExpr::Tuple(fields) => {
                let types = fields
                    .iter()
                    .map(|value_expr| {
                        // todo: check if we really want to unconst tuple values
                        // maybe we need a way to tell that it should be consted here. e.g.
                        //  `(5,"hallo")`
                        (
                            TypeExpr::from_value_expr(value_expr, type_env),
                            value_expr.1,
                        )
                    })
                    .collect::<Vec<Spanned<TypeExpr>>>();

                TypeExpr::Tuple(types)
            }
            ValueExpr::Duck(fields) => {
                let types = fields
                    .iter()
                    .map(|(name, (value_expr, span))| {
                        Field::new(
                            name.to_string(),
                            (
                                TypeExpr::from_value_expr(&(value_expr.clone(), *span), type_env),
                                *span,
                            ),
                        )
                    })
                    .collect::<Vec<Field>>();

                TypeExpr::Duck(Duck { fields: types })
            }
            ValueExpr::Add(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right, type_env);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Addition '+' is only allowed for numbers. You've used {} + {}.",
                        left_type_expr.as_go_type_annotation(type_env),
                        right_type_expr.as_go_type_annotation(type_env)
                    ),
                );

                check_type_compatability(
                    &(left_type_expr.clone(), left.as_ref().1),
                    &(right_type_expr, right.as_ref().1),
                    type_env,
                );

                left_type_expr.unconst()
            }
            ValueExpr::Sub(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right, type_env);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Subtraction '-' is only allowed for numbers. You've used {} - {}.",
                        left_type_expr.as_go_type_annotation(type_env),
                        right_type_expr.as_go_type_annotation(type_env)
                    ),
                );

                check_type_compatability(
                    &(left_type_expr.clone(), left.as_ref().1),
                    &(right_type_expr, right.as_ref().1),
                    type_env,
                );

                left_type_expr.unconst()
            }
            ValueExpr::Mod(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right, type_env);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Modulo '-' is only allowed for numbers. You've used {} % {}.",
                        left_type_expr.as_go_type_annotation(type_env),
                        right_type_expr.as_go_type_annotation(type_env)
                    ),
                );

                check_type_compatability(
                    &(left_type_expr.clone(), left.as_ref().1),
                    &(right_type_expr, right.as_ref().1),
                    type_env,
                );

                left_type_expr.unconst()
            }
            ValueExpr::Div(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right, type_env);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Division '/' is only allowed for numbers. You've used {} / {}.",
                        left_type_expr.as_go_type_annotation(type_env),
                        right_type_expr.as_go_type_annotation(type_env)
                    ),
                );

                check_type_compatability(
                    &(left_type_expr.clone(), left.as_ref().1),
                    &(right_type_expr, right.as_ref().1),
                    type_env,
                );

                left_type_expr.unconst()
            }
            ValueExpr::Equals(lhs, rhs)
            | ValueExpr::NotEquals(lhs, rhs)
            | ValueExpr::LessThan(lhs, rhs)
            | ValueExpr::LessThanOrEquals(lhs, rhs)
            | ValueExpr::GreaterThan(lhs, rhs)
            | ValueExpr::GreaterThanOrEquals(lhs, rhs)
            | ValueExpr::And(lhs, rhs)
            | ValueExpr::Or(lhs, rhs) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(lhs, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(rhs, type_env);

                check_type_compatability(
                    &(left_type_expr.clone(), lhs.1),
                    &(right_type_expr, rhs.1),
                    type_env,
                );

                TypeExpr::Bool(None)
            }
            ValueExpr::Mul(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right, type_env);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Multiplication '*' is only allowed for numbers. You've used {} + {}.",
                        left_type_expr.as_go_type_annotation(type_env),
                        right_type_expr.as_go_type_annotation(type_env)
                    ),
                );

                check_type_compatability(
                    &(left_type_expr.clone(), left.1),
                    &(right_type_expr, right.1),
                    type_env,
                );

                left_type_expr.unconst()
            }
            ValueExpr::FunctionCall {
                target,
                params,
                type_params: _,
            } => {
                // todo: type_params
                let in_param_types = params
                    .iter()
                    .map(|param| (TypeExpr::from_value_expr(param, type_env), param.1))
                    .collect::<Vec<_>>();

                let mut target_type = TypeExpr::from_value_expr(target.as_ref(), type_env);
                if let TypeExpr::Fun(param_types, return_type) = &mut target_type {
                    param_types
                        .iter_mut()
                        .enumerate()
                        .for_each(|(index, param_type)| {
                            if matches!(param_type.1.0, TypeExpr::Any) {
                                return;
                            }

                            check_type_compatability(
                                &param_type.1,
                                in_param_types.get(index).unwrap(),
                                type_env,
                            );

                            // variant any replace
                            if let TypeExpr::Array(boxed) = &in_param_types.get(index).unwrap().0 {
                                if let TypeExpr::Or(_) = boxed.as_ref().0 {
                                    param_type.1.0 =
                                        TypeExpr::Array(Box::new((TypeExpr::Any, empty_range())))
                                }
                            }
                        });

                    return return_type
                        .clone()
                        .map_or(TypeExpr::Tuple(vec![]), |x| x.as_ref().0.clone());
                }

                failure(
                    target.as_ref().1.context.file_name,
                    "Tried to invoke a non-function value".to_string(),
                    (
                        "This is the value you tried to invoke as a function.".to_string(),
                        target.as_ref().1,
                    ),
                    vec![
                        (
                            format!(
                                "the thing you tried to invoke is of type {}",
                                TypeExpr::from_value_expr(target.as_ref(), type_env)
                                    .as_clean_go_type_name(type_env)
                            ),
                            target.as_ref().1,
                        ),
                        (
                            format!(
                                "{} cannot be called as it's not of type function!",
                                TypeExpr::from_value_expr(target.as_ref(), type_env)
                                    .as_clean_go_type_name(type_env)
                            ),
                            target.as_ref().1,
                        ),
                    ],
                    target.as_ref().1.context.file_contents,
                )
            }
            ValueExpr::Block(value_exprs) => {
                let mut ty = TypeExpr::Tuple(vec![]);
                value_exprs.iter().for_each(|value_expr| {
                    ty = TypeExpr::from_value_expr(value_expr, type_env);
                });

                // TODO: add correct return type of block
                // 26.06.2025: Return type of last expression as type of block?

                return ty;
            }
            ValueExpr::Variable(_, ident, type_expr) => {
                let s = Location::caller();

                type_expr
                    .as_ref()
                    .cloned()
                    .or(type_env.get_identifier_type(ident.clone()))
                    .unwrap_or_else(|| {
                        panic!(
                            "{} - {s}",
                            format!("Expected type but didn't get one {ident} {type_expr:?}")
                                .leak(),
                        )
                    })
                    .clone()
            }
            ValueExpr::BoolNegate(bool_expr) => {
                check_type_compatability(
                    &(TypeExpr::from_value_expr(bool_expr, type_env), bool_expr.1),
                    &TypeExpr::Bool(None).into_empty_span(),
                    type_env,
                );
                TypeExpr::Bool(None)
            }
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                let condition_type_expr = TypeExpr::from_value_expr(condition, type_env);
                check_type_compatability(
                    &(condition_type_expr, condition.1),
                    &TypeExpr::Bool(None).into_empty_span(),
                    type_env,
                );

                let _then_type_expr = TypeExpr::from_value_expr(then, type_env);
                if let Some(r#else) = r#else {
                    let _else_type_expr = TypeExpr::from_value_expr(r#else, type_env);
                }

                // let x: TypeExpression = combine_types(vec![else_type_expr, then]);

                // todo!("combine then and else, then return combined type");

                _then_type_expr
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let span = target_obj.as_ref().1;
                let target_obj_type_expr =
                    TypeExpr::from_value_expr_resolved_type_name_dereferenced(target_obj, type_env);

                if !target_obj_type_expr.is_object_like()
                    && !target_obj_type_expr.ref_is_object_like()
                {
                    failure_with_occurence(
                        span.context.file_name,
                        "Invalid Field Access".to_string(),
                        {
                            let mut span = span.clone();
                            span.end += 2;
                            span
                        },
                        vec![(
                            format!("this value is not object like and has no fields to access"),
                            span.clone(),
                        )],
                        span.context.file_contents,
                    )
                }

                if !(target_obj_type_expr.has_field_by_name(field_name.clone(), type_env)
                    || target_obj_type_expr.has_method_by_name(field_name.clone(), type_env)
                    || target_obj_type_expr.ref_has_field_by_name(field_name.clone(), type_env)
                    || target_obj_type_expr.ref_has_method_by_name(field_name.clone(), type_env))
                {
                    failure_with_occurence(
                        span.context.file_name,
                        "Invalid Field Access".to_string(),
                        {
                            let mut span = span.clone();
                            span.end += 2;
                            span
                        },
                        vec![(
                            format!(
                                "this is of type {} and it has no field '{}'",
                                target_obj_type_expr
                                    .as_clean_user_faced_type_name()
                                    .bright_yellow(),
                                field_name.bright_blue()
                            ),
                            span.clone(),
                        )],
                        span.context.file_contents,
                    )
                }

                target_obj_type_expr
                    .typeof_field(field_name.to_string(), type_env)
                    .or_else(|| {
                        target_obj_type_expr.ref_typeof_field(field_name.to_string(), type_env)
                    })
                    .expect("Invalid Field Access")
            }
            ValueExpr::While { condition, body } => {
                let condition_type_expr = TypeExpr::from_value_expr(condition, type_env);
                check_type_compatability(
                    &(condition_type_expr, condition.1),
                    &TypeExpr::Bool(None).into_empty_span(),
                    type_env,
                );

                let _body_type_expr = TypeExpr::from_value_expr(body, type_env);

                return TypeExpr::Tuple(vec![]);
            }
            // TODO: Match Expressions need to be type resolved just as the function defs
            ValueExpr::Match {
                value_expr,
                arms,
                else_arm,
                span: _,
            } => {
                let mut arms = arms.clone();
                if let Some(arm) = else_arm {
                    arms.push(arm.as_ref().clone());
                }

                let mut arm_types = Vec::new();
                for arm in &arms {
                    let arm_type = TypeExpr::from_value_expr(&arm.value_expr, type_env);
                    if !arm_types.iter().any(|(x, _)| x == &arm_type.unconst()) {
                        arm_types.push((arm_type.unconst(), arm.value_expr.1));
                    }
                }

                if else_arm.is_none() {
                    let possible_types: Vec<Spanned<TypeExpr>> =
                        match &TypeExpr::from_value_expr(value_expr, type_env) {
                            TypeExpr::Or(types) => types.clone(),
                            other => vec![(other.clone(), value_expr.1)],
                        };

                    let mut covered_types = Vec::new();
                    for arm in &arms {
                        let case_type = &arm.type_case.0;

                        if !covered_types.iter().any(|(x, _)| x == case_type) {
                            covered_types.push((case_type.clone(), arm.type_case.1));
                        }
                    }

                    possible_types.iter().for_each(|possible_type| {
                        let is_covered = &covered_types.iter().any(|(x, _)| *x == possible_type.0.unconst());
                        if !is_covered {
                            let missing_type = possible_type;
                            failure_with_occurence(
                                complete_span.context.file_name,
                                "Unexhaustive Match".to_string(),
                                *complete_span,
                                vec![(
                                    format!(
                                        "possible type {} not covered",
                                        format!("{}", missing_type.0).bright_yellow()
                                    ),
                                    *complete_span,
                                )],
                                complete_span.context.file_contents,
                            );
                        }
                    });
                }

                if arm_types.is_empty() {
                    TypeExpr::Tuple(vec![])
                } else if arm_types.len() == 1 {
                    arm_types.first().cloned().unwrap().0
                } else {
                    TypeExpr::Or(arm_types)
                }
            }
        };
    }

    pub fn is_object_like(&self) -> bool {
        match self {
            Self::Tuple(..) | Self::Duck(..) | Self::Struct(..) => true,
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
            Self::Duck(..) => true,
            _ => false,
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            Self::Tuple(v) => v.is_empty(),
            Self::InlineGo => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            Self::Struct(..) => true,
            _ => false,
        }
    }

    pub fn unconst(&self) -> TypeExpr {
        match self {
            Self::String(..) => Self::String(None),
            Self::Int(..) => Self::Int(None),
            Self::Bool(..) => Self::Bool(None),
            s => s.clone(),
        }
    }

    pub fn has_subtypes(&self) -> bool {
        match self {
            Self::Or(..) | Self::Tuple(..) | Self::Duck(..) | Self::Struct(..) => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn has_field(&self, field: Field) -> bool {
        match self {
            Self::Tuple(fields) => fields.len() > field.name.parse::<usize>().unwrap(),
            Self::Struct(_struct) => todo!(),
            Self::Duck(duck) => duck.fields.contains(&field),
            _ => false,
        }
    }

    fn has_method_by_name(&self, name: String, type_env: &mut TypeEnv) -> bool {
        match self {
            Self::Struct(r#struct) => {
                let StructDefinition {
                    name: struct_name,
                    fields: _,
                    methods,
                    mut_methods: _,
                    generics: _,
                } = type_env.get_struct_def(r#struct.as_str());

                methods.iter().any(|f| f.name.as_str() == name.as_str())
                    || type_env
                        .get_generic_methods(struct_name.clone())
                        .iter()
                        .any(|x| x.name.as_str() == name.as_str())
            }
            _ => false,
        }
    }

    fn has_field_by_name(&self, name: String, type_env: &TypeEnv) -> bool {
        match self {
            Self::Tuple(fields) => fields.len() > name.parse::<usize>().unwrap(),
            Self::Struct(r#struct) => {
                let StructDefinition {
                    name: _,
                    fields,
                    methods: _,
                    mut_methods: _,
                    generics: _,
                } = type_env.get_struct_def(r#struct.as_str());

                fields.iter().any(|f| f.name.as_str() == name.as_str())
            }
            Self::Duck(duck) => duck
                .fields
                .iter()
                .any(|struct_field| *struct_field.name == name),
            _ => false,
        }
    }

    fn ref_has_field_by_name(&self, name: String, type_env: &TypeEnv) -> bool {
        match self {
            Self::Ref(t) | Self::RefMut(t) => {
                t.0.has_field_by_name(name.clone(), type_env)
                    || t.0.ref_has_field_by_name(name.clone(), type_env)
            }
            _ => false,
        }
    }

    fn ref_has_method_by_name(&self, name: String, type_env: &mut TypeEnv) -> bool {
        match self {
            Self::Ref(t) | Self::RefMut(t) => {
                t.0.has_method_by_name(name.clone(), type_env)
                    || t.0.ref_has_method_by_name(name.clone(), type_env)
            }
            _ => false,
        }
    }

    fn typeof_field(&self, field_name: String, type_env: &TypeEnv) -> Option<TypeExpr> {
        Some(match self {
            Self::Tuple(fields) => fields[field_name.parse::<usize>().unwrap()].0.clone(),
            Self::Struct(r#struct) => {
                let StructDefinition {
                    name,
                    fields,
                    methods,
                    mut_methods: _,
                    generics: _,
                } = type_env.get_struct_def(r#struct.as_str());

                fields
                    .iter()
                    .map(|x| (x.name.clone(), x.type_expr.0.clone()))
                    .chain(methods.iter().map(|x| {
                        (
                            x.name.clone(),
                            TypeExpr::Fun(
                                x.params
                                    .clone()
                                    .unwrap_or_default()
                                    .iter()
                                    .map(|x| (Some(x.0.clone()), x.1.clone()))
                                    .collect(),
                                x.return_type.clone().map(Box::new),
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
                                            .clone()
                                            .unwrap_or_default()
                                            .iter()
                                            .map(|x| (Some(x.0.clone()), x.1.clone()))
                                            .collect(),
                                        x.return_type.clone().map(Box::new),
                                    ),
                                )
                            }),
                    )
                    .find(|struct_field| struct_field.0 == field_name)
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
            _ => return None,
        })
    }

    fn ref_typeof_field(&self, field_name: String, type_env: &TypeEnv) -> Option<TypeExpr> {
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
        || matches!(*self, TypeExpr::Int(..));
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
        return matches!(self, TypeExpr::Int(..));
    }

    pub fn holds_const_value(&self) -> bool {
        // todo(@Mvmo) Implement other literal types
        // floats, chars.... missing
        return matches!(*self, TypeExpr::Int(..))
            || matches!(*self, TypeExpr::String(..))
            || matches!(*self, TypeExpr::Bool(..));
    }

    pub fn is_variant(&self) -> bool {
        return matches!(&self, TypeExpr::Or(..));
    }

    pub fn is_primitive(&self) -> bool {
        return match *self {
            | TypeExpr::Float
            | TypeExpr::Char
            | TypeExpr::Int(..)
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

fn types_are_compatible(one: &TypeExpr, two: &TypeExpr, type_env: &mut TypeEnv) -> bool {
    if one.is_string() && two.is_string() {
        return true;
    }

    if one.is_bool() && two.is_bool() {
        return true;
    }

    if one.is_number() && two.is_number() {
        return true;
    }

    if one.is_number() || two.is_number() {
        return false;
    }

    if one.is_tuple() && two.is_tuple() {
        let TypeExpr::Tuple(types_one) = one.clone() else {
            panic!("not a tuple?")
        };
        let TypeExpr::Tuple(types_two) = two.clone() else {
            panic!("not a tuple?")
        };

        if types_one.len() == types_two.len()
            && types_one
                .iter()
                .zip(types_two.iter())
                .all(|(a, b)| types_are_compatible(&a.0, &b.0, type_env))
        {
            return true;
        }
    }

    one == two
}

fn is_non_variant_type_in_variant(
    non_variant_type: &Spanned<TypeExpr>,
    variant: &[Spanned<TypeExpr>],
    type_env: &mut TypeEnv,
) -> bool {
    variant.iter().any(|(haystack_member, _)| {
        types_are_compatible(haystack_member, &non_variant_type.0, type_env)
    })
}

fn require_subset_of_variant_type(
    variant_type: &Spanned<TypeExpr>,
    other: &Spanned<TypeExpr>,
    type_env: &mut TypeEnv,
) {
    let variant_members = match &variant_type.0 {
        TypeExpr::Or(members) => members,
        _ => {
            panic!("is_subset_of_variant_type called with a non-variant type");
        }
    };

    match &other.0 {
        TypeExpr::Or(other_members) => {
            for other_member in other_members {
                if !is_non_variant_type_in_variant(other_member, variant_members, type_env) {
                    failure(
                        variant_type.1.context.file_name,
                        "Incompatible Variant Types".to_string(),
                        (
                            format!(
                                "The type `{}` is not compatible with the target variant.",
                                other_member.0.as_clean_user_faced_type_name()
                            ),
                            other_member.1,
                        ),
                        vec![(
                            format!(
                                "The target variant only allows the following types: `{}`.",
                                variant_type.0.as_clean_user_faced_type_name()
                            ),
                            variant_type.1,
                        )],
                        variant_type.1.context.file_contents,
                    );
                }
            }
        }
        _ => {
            if !is_non_variant_type_in_variant(other, variant_members, type_env) {
                failure(
                    other.1.context.file_name,
                    "Incompatible Types".to_string(),
                    (
                        format!(
                            "This expression is of type `{}`.",
                            other.0.as_clean_user_faced_type_name()
                        ),
                        other.1,
                    ),
                    vec![(
                        format!(
                            "But it needs to be compatible with one of the types in the variant: `{}`.",
                            variant_type.0.as_clean_user_faced_type_name()
                        ),
                        variant_type.1,
                    )],
                    other.1.context.file_contents,
                );
            }
        }
    }
}

pub fn check_type_compatability(
    required_type: &Spanned<TypeExpr>,
    given_type: &Spanned<TypeExpr>,
    type_env: &mut TypeEnv,
) {
    let mut given_type = given_type.clone();
    given_type.0 = type_env.try_resolve_type_expr(&given_type.0);
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
            given_type.1.context.file_name,
            "Incompatible Types".to_string(),
            given_type.1,
            vec![
                (explain_required.to_string(), required_type.1),
                (explain_given.to_string(), given_type.1),
            ],
            given_type.1.context.file_contents,
        )
    };

    match &required_type.0 {
        TypeExpr::Ref(req_t) => {
            if let TypeExpr::Ref(given_t) | TypeExpr::RefMut(given_t) = &given_type.0 {
                check_type_compatability(req_t, given_t, type_env);
            } else {
                fail_requirement(
                    "This is an immutable reference".to_string(),
                    "So this needs to be an immutable reference or a mutable reference".to_string(),
                );
            }
        }
        TypeExpr::RefMut(req_t) => {
            if let TypeExpr::RefMut(given_t) = &given_type.0 {
                check_type_compatability(req_t, given_t, type_env);
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
        TypeExpr::Alias(..) => panic!("alias should have been replaced"),
        TypeExpr::Any => return,
        TypeExpr::InlineGo => todo!("should inline go be typechecked?"),
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
        TypeExpr::Struct(_strct) => {
            if !given_type.0.is_struct() {
                fail_requirement(
                    format!(
                        "the required type {} is a struct",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                    format!(
                        "because of the fact, that the required type {} is a struct. The value you need to pass must be a  aswell, but it isn't.",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                )
            }

            // todo: check against identity of struct in typechecking
        }
        TypeExpr::Duck(duck) => match &given_type.0 {
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
            TypeExpr::Struct(struct_name) => {
                let struct_def = type_env.get_struct_def(struct_name).clone();

                for required_field in duck.fields.iter() {
                    if required_field.type_expr.0.is_fun() {
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
                            )
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
                    "because of the fact, that the required type {} is a duck. The value you need to pass must be a duck aswell, but it isn't.",
                    format!("{}", required_type.0).bright_yellow(),
                ),
            ),
        },
        TypeExpr::Tuple(item_types) => {
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

            for (index, required_item_type) in required_item_types.iter().enumerate() {
                let given_item_type = given_item_types.get(index)
                    .expect("we've just checked that given_item_types is at least the sizeof required_item_types");

                check_type_compatability(required_item_type, given_item_type, type_env);
            }
        }
        // this is a const time string check, which has been removed at 29.09.2025 by me mvmo m omo oo mo m o m
        // yoooo
        // TypeExpr::String(literal) => {
        //     if !given_type.0.is_string() {
        //         fail_requirement(
        //             format!("this requires an at compile time known {}", required_type.0),
        //             format!("this value isn't even a string, it's a {}.", given_type.0),
        //         )
        //     }

        //     if !given_type.0.holds_const_value() {
        //         fail_requirement(
        //             format!("this requires a compile time known {}", required_type.0),
        //             "this is a string, but it's not known at compile time".to_string(),
        //         )
        //     }

        //     let required_string = literal;
        //     let TypeExpr::String(given_string) = &given_type.0 else {
        //         unreachable!("we've just checked that the given type is a string and is const")
        //     };

        //     // if given_string != required_string {
        //     //     fail_requirement(
        //     //         format!("this requires the compile time known string '{required_string}'"),
        //     //         format!("this is a compile time known string, but it's '{given_string}'"),
        //     //     )
        //     // }
        // }
        // const int validation
        // TypeExpr::Int(const_int) => {
        //     if !given_type.0.is_int() {
        //         fail_requirement(
        //             "this requires an at compile time known Int".to_string(),
        //             "this value isn't even an Int.".to_string(),
        //         )
        //     }

        //     if !given_type.0.holds_const_value() {
        //         fail_requirement(
        //             "this requires a compile time known Int".to_string(),
        //             "this is an Int, but it's not known at compile time".to_string(),
        //         )
        //     }

        //     let required_int = const_int;
        //     let TypeExpr::Int(given_int) = &given_type.0 else {
        //         unreachable!("we've just checked that the given type is a int and is const")
        //     };

        //     if required_int != given_int {
        //         fail_requirement(
        //             format!("this requires the compile time known Int '{required_int}'"),
        //             format!("this is a compile time known Int, but it's '{given_int}'"),
        //         )
        //     }
        // }
        // const bool validation
        // TypeExpr::Bool(const_bool) => {
        //     if !given_type.0.is_bool() {
        //         fail_requirement(
        //             "this requires an at compile time known Bool".to_string(),
        //             "this value isn't even a Bool.".to_string(),
        //         )
        //     }

        //     if !given_type.0.holds_const_value() {
        //         fail_requirement(
        //             "this requires a compile time known Bool".to_string(),
        //             "this is an Bool, but it's not known at compile time".to_string(),
        //         )
        //     }

        //     let required_bool = const_bool;
        //     let TypeExpr::Bool(given_bool) = &given_type.0 else {
        //         unreachable!("we've just checked that the given type is a bool and is const")
        //     };

        //     if required_bool != given_bool {
        //         fail_requirement(
        //             format!("this requires the compile time known Bool '{required_bool}'"),
        //             format!("this is a compile time known Bool, but it's '{required_bool}'"),
        //         )
        //     }
        // }
        TypeExpr::String(..) => {
            if !given_type.0.is_string() {
                fail_requirement(
                    "this expects a string.".to_string(),
                    "this is not a string.".to_string(),
                );
            }
        }
        TypeExpr::Int(..) => {
            if !given_type.0.is_number() {
                fail_requirement(
                    "this expects an int.".to_string(),
                    format!(
                        "this is not an int. it's a {}",
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
                        "this is not a bool. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::Char => {
            if !given_type.0.is_char() {
                fail_requirement(
                    "this expects an int.".to_string(),
                    "this is not an int.".to_string(),
                );
            }
        }
        TypeExpr::Float => {
            // todo: discuss if we just allow passing ints as floats
            if !given_type.0.is_number() {
                fail_requirement(
                    "this expects a number.".to_string(),
                    "this is not a number.".to_string(),
                );
            }
        }
        TypeExpr::Or(..) => {
            require_subset_of_variant_type(required_type, &given_type, type_env);
        }
        TypeExpr::Fun(required_params, required_return_type) => {
            if !given_type.0.is_fun() {
                fail_requirement(
                    "this requires a function".to_string(),
                    "this value isn't even a string.".to_string(),
                )
            }

            let TypeExpr::Fun(given_params, given_return_type) = &given_type.0 else {
                unreachable!("we've already checked that it's a function")
            };
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

            for (index, param) in required_params.iter().enumerate() {
                let given_param = given_params
                    .get(index)
                    .expect("we've just checked that required and given params are equal size");

                check_type_compatability(&param.1, &given_param.1, type_env);
            }

            if let Some(required_return_type) = required_return_type {
                if given_return_type.clone().is_none() {
                    fail_requirement(
                        format!(
                            "this requires a function that returns {}",
                            required_return_type.clone().0
                        ),
                        "this function doesn't return anything".to_string(),
                    )
                }

                let given_return_type = given_return_type
                    .as_ref()
                    .expect("we've just handled is_none");

                check_type_compatability(required_return_type, given_return_type, type_env);
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

            check_type_compatability(content_type, &given_content_type, type_env);
        }
        TypeExpr::RawTypeName(..) | TypeExpr::TypeName(..) | TypeExpr::TypeNameInternal(..) => {}
        TypeExpr::And(required_variants) => {
            for required_variant in required_variants {
                check_type_compatability(&required_variant, &given_type, type_env);
            }
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
        semantics::type_resolve::{TypesSummary, typeresolve_source_file},
    };
    use chumsky::prelude::*;

    use super::*;

    #[test]
    fn test_typeresolve() {
        let src_and_expected_type_vec = vec![
            // todo: ("4 + 4", TypeExpr::Int(Some("4"))),
            ("\"Hallo\"", TypeExpr::String(Some("Hallo".to_string()))),
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
            ("0 + 0.4", TypeExpr::Int(None)),
            ("0.4 + 0", TypeExpr::Float),
            (
                "(0, 2)",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int(Some(0)).into_empty_span(),
                    TypeExpr::Int(Some(2)).into_empty_span(),
                ]),
            ),
            (
                "(0, 2 + 2)",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int(Some(0)).into_empty_span(),
                    TypeExpr::Int(None).into_empty_span(),
                ]),
            ),
            (
                "(0, (2 + 2, 5))",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int(Some(0)).into_empty_span(),
                    TypeExpr::Tuple(vec![
                        TypeExpr::Int(None).into_empty_span(),
                        TypeExpr::Int(Some(5)).into_empty_span(),
                    ])
                    .into_empty_span(),
                ]),
            ),
            (
                "(0, (\"Hallo, Welt\", 5))",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int(Some(0)).into_empty_span(),
                    TypeExpr::Tuple(vec![
                        TypeExpr::String(Some("Hallo, Welt".to_string())).into_empty_span(),
                        TypeExpr::Int(Some(5)).into_empty_span(),
                    ])
                    .into_empty_span(),
                ]),
            ),
            ("{ let x: Int = 5; 5 }", TypeExpr::Int(Some(5))),
            ("{ let x: Int = 5; x }", TypeExpr::Int(None)),
            ("{ let x: Int = 5; x * x }", TypeExpr::Int(None)),
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
                    params: None,
                    return_type: None,
                    value_expr: value_expr,
                    generics: None,
                    span: empty_range(),
                }],
                ..Default::default()
            };

            let mut type_env = TypeEnv::default();
            typeresolve_source_file(&mut source_file, &mut type_env);

            let mut type_expr = (
                TypeExpr::from_value_expr(
                    &source_file.function_definitions.get(0).unwrap().value_expr,
                    &mut type_env,
                ),
                empty_range(),
            );
            type_expr_into_empty_range(&mut type_expr);

            assert_eq!(type_expr.0, expected_type_expr);
        }
    }

    #[test]
    fn test_type_summary() {
        // the summary always holds the type of main and it's return type and all the primitives
        let primitive_and_main_len = TypeExpr::primitives().len() + 1 /* main fn type */ + 1 /* main fn return type -> () */;
        let src_and_summary_check_funs: Vec<(&str, Box<dyn FnOnce(&TypesSummary)>)> = vec![
            (
                "{ let y: { x: String, y: Int } = { x: \"\", y: 0 }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: String, y: Int, a: { b: { c: { d: { e: String }}}} } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // +1 because of the empty duck
                    assert_eq!(summary.types_used.len(), 5 + 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let x: Int = 5; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 0 + primitive_and_main_len);
                }),
            ),
            (
                "{ let x: { a: Char, b: Char } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // +1 because of empty duck
                    assert_eq!(summary.types_used.len(), 1 + 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: { y: Int } } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // + 1 because of empty duck
                    assert_eq!(summary.types_used.len(), 1 + 2 + primitive_and_main_len);
                }),
            ),
            (
                // + 1 because of empty duck
                "{ let y: { x: Int } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 1 + 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: Int, y: String, z: { x: Int } } = { }; }",
                Box::new(|summary: &TypesSummary| {
                    // + 1 because of empty duck
                    assert_eq!(summary.types_used.len(), 2 + 1 + primitive_and_main_len);
                }),
            ),
            (
                // + 1 because of empty duck
                "{ let y: { x: Int, y: String, z: { x: Int }, w: () } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 2 + 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let a: { b: { c: { d: { e: { f: { a: Int }}}}}} = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // + 1 because of empty duck
                    assert_eq!(summary.types_used.len() - primitive_and_main_len, 7);
                }),
            ),
            (
                "{ let y: { x: String } | { y: String } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // + 1 because of empty duck
                    assert_eq!(summary.types_used.len(), 3 + 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: String } | { y: String } | { z: String } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // +1 because of empty duck
                    assert_eq!(summary.types_used.len(), 4 + 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: { x: String } | { y: String } | { z: String } }  = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // +1 because of empty duck
                    assert_eq!(summary.types_used.len() - primitive_and_main_len, 5 + 1);
                }),
            ),
            (
                "{ let y: { x: String } | { y: String } | { z: String } | { u: String } | { v: String } | { w: String } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // +1 because of empty duck
                    assert_eq!(summary.types_used.len(), 7 + 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: String } | { x: String } | { x: String } | { x: String } | { x: String } | { x: String } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // +1 because of empty duck
                    assert_eq!(summary.types_used.len(), 2 + 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { abc: { x: String, y: String }, abc2: { x: String, y: String } } = {}; }",
                Box::new(|summary: &TypesSummary| {
                    // +1 because of empty duck
                    assert_eq!(summary.types_used.len(), 2 + 1 + primitive_and_main_len);
                }),
            ),
        ];

        for (src, summary_check_fun) in src_and_summary_check_funs {
            let lexer_parse_result = lex_parser("test", "").parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false, "Couldn't lex {src}");
            assert_eq!(lexer_parse_result.has_output(), true, "Couldn't lex {src}");

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            let value_expr_parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));

            assert_eq!(
                value_expr_parse_result.has_errors(),
                false,
                "Couldn't parse value expr {src}"
            );

            assert_eq!(
                value_expr_parse_result.has_output(),
                true,
                "Couldn't parse value expr {src}"
            );

            let value_expr = value_expr_parse_result.into_output().unwrap();
            let mut source_file = SourceFile {
                function_definitions: vec![FunctionDefintion {
                    name: "main".to_string(),
                    params: None,
                    return_type: None,
                    value_expr: value_expr,
                    generics: None,
                    span: empty_range(),
                }],
                ..Default::default()
            };

            let mut type_env = TypeEnv::default();
            typeresolve_source_file(&mut source_file, &mut type_env);

            let summary = type_env.summarize();

            summary
                .types_used
                .iter()
                .map(|type_expr| type_expr.as_clean_go_type_name(&mut type_env))
                .for_each(|type_name| println!("\t{type_name}"));

            summary_check_fun(&summary);
        }
    }

    #[test]
    fn test_type_compatibility_success() {
        let mut type_env = TypeEnv::default();

        let success_cases = vec![
            (TypeExpr::Int(None), TypeExpr::Int(None)),
            (TypeExpr::String(None), TypeExpr::String(None)),
            (TypeExpr::Int(None), TypeExpr::Float),
            (TypeExpr::Float, TypeExpr::Int(None)),
            (
                TypeExpr::Tuple(vec![
                    empty_spanned(TypeExpr::Int(None)),
                    empty_spanned(TypeExpr::String(None)),
                ]),
                TypeExpr::Tuple(vec![
                    empty_spanned(TypeExpr::Int(None)),
                    empty_spanned(TypeExpr::String(None)),
                ]),
            ),
            (
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::Int(None)))],
                }),
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::Int(None)))],
                }),
            ),
            (
                TypeExpr::Or(vec![
                    empty_spanned(TypeExpr::Int(None)),
                    empty_spanned(TypeExpr::String(None)),
                ]),
                TypeExpr::Int(None),
            ),
            (
                TypeExpr::Or(vec![
                    empty_spanned(TypeExpr::Int(None)),
                    empty_spanned(TypeExpr::String(None)),
                    empty_spanned(TypeExpr::Bool(None)),
                ]),
                TypeExpr::Or(vec![
                    empty_spanned(TypeExpr::String(None)),
                    empty_spanned(TypeExpr::Int(None)),
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
            &empty_spanned(TypeExpr::Int(None)),
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
            &empty_spanned(TypeExpr::Int(None)),
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

        let one = TypeExpr::Tuple(vec![empty_spanned(TypeExpr::Int(None))]);
        let two = TypeExpr::Tuple(vec![empty_spanned(TypeExpr::String(None))]);

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_tuples_different_length() {
        let mut type_env = TypeEnv::default();

        let one = TypeExpr::Tuple(vec![
            empty_spanned(TypeExpr::Int(None)),
            empty_spanned(TypeExpr::Int(None)),
            empty_spanned(TypeExpr::Int(None)),
        ]);
        let two = TypeExpr::Tuple(vec![
            empty_spanned(TypeExpr::Int(None)),
            empty_spanned(TypeExpr::Int(None)),
        ]);

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_ducks_different_field_names() {
        let mut type_env = TypeEnv::default();

        let one = TypeExpr::Duck(Duck {
            fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::Int(None)))],
        });

        let two = TypeExpr::Duck(Duck {
            fields: vec![Field::new("y".to_string(), empty_spanned(TypeExpr::Int(None)))],
        });

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_ducks_different_field_types() {
        let mut type_env = TypeEnv::default();

        let one = TypeExpr::Duck(Duck {
            fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::Int(None)))],
        });

        let two = TypeExpr::Duck(Duck {
            fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::String(None)))],
        });

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_type_not_in_variant() {
        let mut type_env = TypeEnv::default();

        let variant = TypeExpr::Or(vec![
            empty_spanned(TypeExpr::Int(None)),
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
    #[should_panic(expected = "Incompatible Variant Types")]
    fn test_variant_not_subset_of_variant() {
        let mut type_env = TypeEnv::default();

        let super_variant = TypeExpr::Or(vec![
            empty_spanned(TypeExpr::Int(None)),
            empty_spanned(TypeExpr::String(None)),
        ]);
        let sub_variant = TypeExpr::Or(vec![
            empty_spanned(TypeExpr::Int(None)),
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
