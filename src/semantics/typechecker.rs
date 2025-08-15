use std::process;

use colored::Colorize;

use crate::parse::type_parser::{Duck, Field, Struct, TypeExpr};
use crate::parse::SS;
use crate::parse::{
    Spanned, failure,
    value_parser::{ValFmtStringContents, ValueExpr, empty_range},
};
use crate::semantics::type_resolve::TypeEnv;

impl TypeExpr {
    pub fn as_clean_user_faced_type_name(&self) -> String {
        return format!("{self}");
    }

    pub fn from_value_expr(value_expr: &ValueExpr, type_env: &mut TypeEnv) -> TypeExpr {
        return match value_expr {
            ValueExpr::RawVariable(..) => panic!(),
            ValueExpr::FormattedString(contents) => {
                for c in contents {
                    if let ValFmtStringContents::Expr(e) = c {
                        require(
                            matches!(TypeExpr::from_value_expr(&e.0, type_env), TypeExpr::String),
                            "Needs to be string".into(),
                        );
                    }
                }
                TypeExpr::String
            }
            ValueExpr::ArrayAccess(target, idx) => {
                let target_type = TypeExpr::from_value_expr(&target.0, type_env);
                let idx_type = TypeExpr::from_value_expr(&idx.0, type_env);

                require(
                    matches!(&target_type, TypeExpr::Array(_)),
                    "Needs to be array".into(),
                );
                require(matches!(&idx_type, TypeExpr::Int), "Needs to be int".into());

                let TypeExpr::Array(array_type) = target_type else {
                    panic!()
                };

                array_type.0.clone()
            }
            ValueExpr::Array(ty, exprs) => {
                let mut ty = ty.clone().map(|(ty, _)| ty);
                for expr in exprs {
                    let expr_type = TypeExpr::from_value_expr(&expr.0, type_env);
                    match ty.as_ref() {
                        Some(expected) => {
                            check_type_compatability(
                                &(expr_type, expr.1),
                                &(expected.clone(), empty_range()),
                                type_env,
                            );
                        }
                        None => ty = Some(expr_type),
                    }
                }
                TypeExpr::Array(ty.unwrap().into_empty_span().into())
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
            ValueExpr::Int(..) => TypeExpr::Int,
            ValueExpr::Bool(..) => TypeExpr::Bool,
            ValueExpr::Char(..) => TypeExpr::Char,
            ValueExpr::Float(..) => TypeExpr::Float,
            ValueExpr::String(..) => TypeExpr::String,
            ValueExpr::Break => TypeExpr::Tuple(vec![]),
            ValueExpr::Continue => TypeExpr::Tuple(vec![]),
            ValueExpr::Return(Some(value_expr)) => {
                TypeExpr::from_value_expr(&value_expr.0, type_env)
            }
            ValueExpr::Return(None) => TypeExpr::Any, // TODO return never !
            ValueExpr::VarAssign(_assignment) => TypeExpr::Tuple(vec![]),
            ValueExpr::VarDecl(decl) => {
                let decl = decl.as_ref();
                if let Some(init) = &decl.0.initializer {
                    check_type_compatability(
                        &decl.0.type_expr,
                        &(TypeExpr::from_value_expr(&init.0, type_env), init.1),
                        type_env,
                    );
                }

                TypeExpr::Tuple(vec![])
            }
            ValueExpr::Struct(fields) => {
                let types = fields
                    .iter()
                    .map(|(name, (value_expr, span))| {
                        Field::new(
                            name.to_string(),
                            (TypeExpr::from_value_expr(value_expr, type_env), dbg!(*span)),
                        )
                    })
                    .collect::<Vec<Field>>();

                TypeExpr::Struct(Struct { fields: types })
            }
            ValueExpr::Tuple(fields) => {
                let types = fields
                    .iter()
                    .map(|value_expr| {
                        TypeExpr::from_value_expr(&value_expr.0, type_env).into_empty_span()
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
                            (TypeExpr::from_value_expr(&value_expr, type_env), *span),
                        )
                    })
                    .collect::<Vec<Field>>();

                TypeExpr::Duck(Duck { fields: types })
            }
            ValueExpr::Add(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(&left.0, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(&right.0, type_env);

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

                left_type_expr
            }
            ValueExpr::Equals(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(&left.0, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(&right.0, type_env);

                check_type_compatability(
                    &(left_type_expr.clone(), left.1),
                    &(right_type_expr, right.1),
                    type_env,
                );

                TypeExpr::Bool
            }
            ValueExpr::Mul(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(&left.0, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(&right.0, type_env);

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

                left_type_expr
            }
            ValueExpr::FunctionCall {
                target,
                params,
                type_params: _,
            } => {
                // todo: type_params
                let in_param_types = params
                    .iter()
                    .map(|param| (TypeExpr::from_value_expr(&param.0, type_env), param.1))
                    .collect::<Vec<_>>();

                let target_type = TypeExpr::from_value_expr(&target.as_ref().0, type_env);
                if let TypeExpr::Fun(param_types, return_type) = target_type {
                    param_types
                        .iter()
                        .enumerate()
                        .for_each(|(index, param_type)| {
                            if matches!(param_type.1.0, TypeExpr::Any) {
                                return;
                            }

                            check_type_compatability(
                                &param_type.1,
                                in_param_types.get(index).unwrap(),
                                type_env,
                            )
                        });

                    return return_type.map_or(TypeExpr::Tuple(vec![]), |x| x.as_ref().0.clone());
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
                                TypeExpr::from_value_expr(&target.as_ref().0, type_env)
                                    .as_clean_go_type_name(type_env)
                            ),
                            target.as_ref().1,
                        ),
                        (
                            format!(
                                "{} cannot be called as it's not of type function!",
                                TypeExpr::from_value_expr(&target.as_ref().0, type_env)
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
                    ty = TypeExpr::from_value_expr(&value_expr.0, type_env);
                });

                // TODO: add correct return type of block
                // 26.06.2025: Return type of last expression as type of block?

                return ty;
            }
            ValueExpr::Variable(_, ident, type_expr) => {
                type_expr
                    .as_ref()
                    .cloned()
                    .or(type_env.get_identifier_type(ident.clone()))
                    .expect("Expected type but didn't get one")
                    .clone()
            }
            ValueExpr::BoolNegate(bool_expr) => {
                check_type_compatability(
                    &(
                        TypeExpr::from_value_expr(&bool_expr.0, type_env),
                        bool_expr.1,
                    ),
                    &TypeExpr::Bool.into_empty_span(),
                    type_env,
                );
                TypeExpr::Bool
            }
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                let condition_type_expr = TypeExpr::from_value_expr(&condition.0, type_env);
                check_type_compatability(
                    &(condition_type_expr, condition.1),
                    &TypeExpr::Bool.into_empty_span(),
                    type_env,
                );

                let _then_type_expr = TypeExpr::from_value_expr(&then.0, type_env);
                if let Some(r#else) = r#else {
                    let _else_type_expr = TypeExpr::from_value_expr(&r#else.0, type_env);
                }

                // let x: TypeExpression = combine_types(vec![else_type_expr, then]);

                // todo!("combine then and else, then return combined type");

                _then_type_expr
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let target_obj_type_expr = TypeExpr::from_value_expr(&target_obj.0, type_env);
                require(
                    target_obj_type_expr.is_object_like(),
                    format!(
                        "the target of a field access must be of type duck, struct or tuple. Got {}",
                        target_obj_type_expr.as_go_type_annotation(type_env)
                    ),
                );
                require(
                    target_obj_type_expr.has_field_by_name(field_name.clone()),
                    format!(
                        "{:?} {} doesn't have a field with name {}",
                        &target_obj_type_expr,
                        target_obj_type_expr.as_go_type_annotation(type_env),
                        field_name
                    ),
                );

                target_obj_type_expr.typeof_field(field_name.to_string())
            }
            ValueExpr::While { condition, body } => {
                let condition_type_expr = TypeExpr::from_value_expr(&condition.0, type_env);
                check_type_compatability(
                    &(condition_type_expr, condition.1),
                    &TypeExpr::Bool.into_empty_span(),
                    type_env,
                );

                let _body_type_expr = TypeExpr::from_value_expr(&body.0, type_env);

                return TypeExpr::Tuple(vec![]);
            }
            // TODO: Match Expressions need to be type resolved just as the function defs
            ValueExpr::Match {
                value_expr: _,
                arms: _,
            } => return TypeExpr::Tuple(vec![]),
        };
    }

    pub fn is_object_like(&self) -> bool {
        match self {
            Self::Tuple(..) | Self::Duck(..) | Self::Struct(..) => true,
            _ => false,
        }
    }

    pub fn is_duck(&self) -> bool {
        match self {
            Self::Duck(..) => true,
            _ => false,
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
            Self::Struct(r#struct) => r#struct.fields.contains(&field),
            Self::Duck(duck) => duck.fields.contains(&field),
            _ => false,
        }
    }

    fn has_field_by_name(&self, name: String) -> bool {
        match self {
            Self::Tuple(fields) => fields.len() > name.parse::<usize>().unwrap(),
            Self::Struct(r#struct) => r#struct
                .fields
                .iter()
                .any(|struct_field| *struct_field.name == name),
            Self::Duck(duck) => duck
                .fields
                .iter()
                .any(|struct_field| *struct_field.name == name),
            _ => false,
        }
    }

    fn typeof_field(&self, field_name: String) -> TypeExpr {
        match self {
            Self::Tuple(fields) => fields[field_name.parse::<usize>().unwrap()].0.clone(),
            Self::Struct(r#struct) => r#struct
                .fields
                .iter()
                .find(|struct_field| *struct_field.name == field_name)
                .expect("Tried to access field that doesn't exist")
                .type_expr
                .0
                .clone(),
            Self::Duck(duck) => duck
                .fields
                .iter()
                .find(|struct_field| *struct_field.name == field_name)
                .expect("Tried to access field that doesn't exist")
                .type_expr
                .0
                .clone(),
            _ => panic!("Tried to access field on non object-like type."),
        }
    }

    pub fn is_number(&self) -> bool {
        return *self == TypeExpr::Int
        || *self == TypeExpr::Float
        || matches!(*self, TypeExpr::IntLiteral(..));
    }

    pub fn is_tuple(&self) -> bool {
        return matches!(*self, TypeExpr::Tuple(..));
    }

    pub fn is_bool(&self) -> bool {
        return *self == TypeExpr::Bool || matches!(*self, TypeExpr::BoolLiteral(..));
    }

    pub fn is_string(&self) -> bool {
        return *self == TypeExpr::String || matches!(*self, TypeExpr::StringLiteral(..));
    }

    pub fn holds_const_value(&self) -> bool {
        // todo(@Mvmo) Implement other literal types
        // floats, chars.... missing
        return matches!(*self, TypeExpr::IntLiteral(..))
        || matches!(*self, TypeExpr::StringLiteral(..))
        || matches!(*self, TypeExpr::BoolLiteral(..));
    }

    pub fn is_variant(&self) -> bool {
        return matches!(&self, TypeExpr::Or(..));
    }

    pub fn is_literal(&self) -> bool {
        return match *self {
            TypeExpr::IntLiteral(..) | TypeExpr::BoolLiteral(..) | TypeExpr::StringLiteral(..) => {
                true
            }
            _ => false,
        };
    }

    pub fn is_primitive(&self) -> bool {
        return match *self {
            TypeExpr::Int
            | TypeExpr::Float
            | TypeExpr::String
            | TypeExpr::Char
            | TypeExpr::IntLiteral(..)
            | TypeExpr::BoolLiteral(..)
            | TypeExpr::StringLiteral(..)
            | TypeExpr::Bool => true,
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

fn types_are_compatible(one: &TypeExpr, two: &TypeExpr, _type_env: &mut TypeEnv) -> bool {
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

fn check_type_compatability(
    required_type: &Spanned<TypeExpr>,
    given_type: &Spanned<TypeExpr>,
    type_env: &mut TypeEnv,
) {
    let fail_requirement = |explain_required: String, explain_given: String| {
        let (smaller, larger) = if required_type.1.start <= given_type.1.start {
            (required_type.1, given_type.1)
        } else {
            (required_type.1, given_type.1)
        };


        let combined_span = SS {
            start: smaller.start,
            end: larger.end,
            context: required_type.1.context,
        };


        failure(
            given_type.1.context.file_name,
            format!(
                "Incompatible Types",
            ),
            (format!(
                "Expected value of type {}.",
                format!("{}", required_type.0).bright_yellow(),
            ), combined_span),
            vec![
                (format!(
                    "{explain_required}"
                ), required_type.1),
                (format!(
                    "{explain_given}"
                ), given_type.1),
                (format!(
                    "but got a value of type {}.",
                    format!("{}", given_type.0).bright_yellow()
                ), given_type.1),
            ],
            given_type.1.context.file_contents
        )
    };

    println!("check compatability for required type '{}' and given type '{}'", required_type.0, given_type.0);

    match &required_type.0 {
        TypeExpr::Any => return,
        TypeExpr::InlineGo => todo!("should inline go be typechecked?"),
        TypeExpr::Struct(_) => todo!("implement struct typechecking"),
        TypeExpr::Go(_) => unreachable!("typeexpr::go"),
        TypeExpr::Duck(duck) => {
            if !given_type.0.is_duck() {
                fail_requirement(
                    format!(
                        "the required type {} is a duck",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                    format!(
                        "because of the fact, that the required type {} is a duck. The value you need to pass must be a duck aswell, but it isn't.",
                        format!("{}", required_type.0).bright_yellow(),
                    )
                )
            }

            let required_duck = duck;
            let TypeExpr::Duck(given_duck) = &given_type.0 else { unreachable!() };

            for required_field in required_duck.fields.iter() {
                let companion_field = given_duck.fields.iter()
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
                        )
                    )
                }

                let companion_field = companion_field.unwrap();

                check_type_compatability(
                    &required_field.type_expr,
                    &companion_field.type_expr,
                    type_env
                );
            }
        },
        TypeExpr::Tuple(item_types) => {
            if !given_type.0.is_tuple() {
                fail_requirement(
                    format!(
                        "{} is a tuple",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                    String::new()
                )
            }

            let required_item_types = item_types;
            let TypeExpr::Tuple(given_item_types) = &given_type.0 else { unreachable!() };

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

            // for (index, required_item_type) in required_item_types.iter().enumerate() {
                // let given_item_type = requi
                // }
        },
        TypeExpr::RawTypeName(_, items, items1) => todo!(),
        TypeExpr::TypeName(_, _, items) => todo!(),
        TypeExpr::TypeNameInternal(_) => todo!(),
        TypeExpr::StringLiteral(_) => todo!(),
        TypeExpr::IntLiteral(_) => todo!(),
        TypeExpr::BoolLiteral(_) => todo!(),
        TypeExpr::String => todo!(),
        TypeExpr::Int => {
            if !given_type.0.is_number() {
                fail_requirement(
                    format!("a {} value is required here, so {}", "Number".bright_yellow(), "Float | Int".bright_yellow()),
                    format!("")
                )
            }
        },
        TypeExpr::Bool => todo!(),
        TypeExpr::Char => todo!(),
        TypeExpr::Float => todo!(),
        TypeExpr::Or(items) => todo!(),
        TypeExpr::Fun(items, _) => todo!(),
        TypeExpr::Array(_) => todo!(),
        TypeExpr::GenericToBeReplaced(_) => todo!(),
    }
}

fn check_type_compatability__(
    one: &Spanned<TypeExpr>,
    two: &Spanned<TypeExpr>,
    type_env: &mut TypeEnv,
) {
    if one.0.is_variant() {
        require_subset_of_variant_type(one, two, type_env);
        return;
    }

    if one.0.is_bool() && !two.0.is_bool() {
        failure(
            one.1.context.file_name,
            "Incompatible Types".to_string(),
            (
                format!(
                    "This expression is of type {}, which is a bool.",
                    one.0.as_clean_user_faced_type_name()
                ),
                one.1,
            ),
            vec![
                (
                    "because of this, the second operand also needs to be of type bool."
                        .to_string(),
                    two.1,
                ),
                (
                    format!(
                        "but it is of type {}.",
                        two.0.as_clean_user_faced_type_name()
                    ),
                    two.1,
                ),
            ],
            one.1.context.file_contents,
        )
    }

    if one.0.is_string() && !two.0.is_string() {
        failure(
            one.1.context.file_name,
            "Incompatible Types".to_string(),
            (
                format!(
                    "This expression is of type {}, which is a string.",
                    one.0.as_clean_user_faced_type_name()
                ),
                one.1,
            ),
            vec![
                (
                    "because of this, the second operand also needs to be of type string."
                        .to_string(),
                    two.1,
                ),
                (
                    format!(
                        "but it is of type {}.",
                        two.0.as_clean_user_faced_type_name()
                    ),
                    two.1,
                ),
            ],
            one.1.context.file_contents,
        )
    }

    if one.0.is_number() {
        if !two.0.is_number() {
            failure(
                one.1.context.file_name,
                "Incompatible Types".to_string(),
                (
                    format!(
                        "This expression is of type {}, which is a number.",
                        one.0.as_clean_user_faced_type_name()
                    ),
                    one.1,
                ),
                vec![
                    (
                        "because of this, the second operand also needs to be of type number."
                            .to_string(),
                        two.1,
                    ),
                    (
                        format!(
                            "but it is of type {}.",
                            two.0.as_clean_user_faced_type_name()
                        ),
                        two.1,
                    ),
                ],
                one.1.context.file_contents,
            )
        }

        return;
    }

    let one_fn = match &one.0 {
        TypeExpr::Fun(params, return_type) => Some((params, return_type)),
        _ => None,
    };

    let two_fn = match &two.0 {
        TypeExpr::Fun(params, return_type) => Some((params, return_type)),
        _ => None,
    };

    if let Some((fst, snd)) = one_fn.zip(two_fn) {
        let fst_params = fst
            .0
            .iter()
            .map(|x| x.1.0.as_clean_go_type_name(type_env))
            .collect::<Vec<_>>();
        let snd_params = snd
            .0
            .iter()
            .map(|x| x.1.0.as_clean_go_type_name(type_env))
            .collect::<Vec<_>>();

        let fst_return = fst
            .1
            .clone()
            .map(|x| x.0.as_clean_go_type_name(type_env))
            .unwrap_or(TypeExpr::Tuple(vec![]).as_clean_go_type_name(type_env));
        let snd_return = snd
            .1
            .clone()
            .map(|x| x.0.as_clean_go_type_name(type_env))
            .unwrap_or(TypeExpr::Tuple(vec![]).as_clean_go_type_name(type_env));

        if (fst_params == snd_params) && (fst_return == snd_return) {
            return;
        }
    }

    if one.0.as_clean_go_type_name(type_env) != two.0.as_clean_go_type_name(type_env) {
        let (smaller, larger) = if one.1.start <= two.1.start {
            (one.1, two.1)
        } else {
            (two.1, one.1)
        };

        let combined_span = SS {
            start: smaller.start,
            end: larger.end,
            context: one.1.context,
        };

        failure(
            one.1.context.file_name,
            "Incompatible Types".to_string(),
            (
                format!("this is of type {}", one.0.as_clean_user_faced_type_name()),
                one.1,
            ),
            vec![
                (
                    format!("this is of type {}", two.0.as_clean_user_faced_type_name()),
                    two.1,
                ),
                (
                    "These two types are not not compatible".to_string(),
                    combined_span,
                ),
            ],
            one.1.context.file_contents,
        );
    }
}

#[cfg(test)]
mod test {
    use crate::{
        parse::{
            function_parser::FunctionDefintion,
            lexer::lex_parser,
            make_input,
            source_file_parser::SourceFile,
            type_parser::Field,
            value_parser::{empty_range, value_expr_parser}, SS,
        },
        semantics::type_resolve::{typeresolve_source_file, TypesSummary},
    };
    use chumsky::prelude::*;

    use super::*;

    #[test]
    fn test_typeresolve() {
        let src_and_expected_type_vec = vec![
            ("4 + 4", TypeExpr::Int),
            ("\"Hallo\"", TypeExpr::String),
            (
                "{ x: \"hallo\", }",
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new(
                        "x".to_string(),
                        TypeExpr::String.into_empty_span(),
                    )],
                }),
            ),
            ("0.5", TypeExpr::Float),
            ("0.1 + 0.4", TypeExpr::Float),
            ("0 + 0.4", TypeExpr::Int),
            ("0.4 + 0", TypeExpr::Float),
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
                        TypeExpr::String.into_empty_span(),
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
                    params: None,
                    return_type: None,
                    value_expr: value_expr,
                    generics: None,
                }],
                ..Default::default()
            };

            let mut type_env = TypeEnv::default();
            typeresolve_source_file(&mut source_file, &mut type_env);

            let type_expr = TypeExpr::from_value_expr(
                &source_file
                    .function_definitions
                    .get(0)
                    .unwrap()
                    .value_expr
                    .0,
                &mut type_env,
            );
            assert_eq!(type_expr, expected_type_expr);
        }
    }

    #[test]
    fn test_type_summary() {
        // the summary always holds the type of main and it's return type and all the primitives
        let primitive_and_main_len = TypeExpr::primitives().len() + 1 /* main fn type */ + 1 /* main fn return type -> () */;
        let src_and_summary_check_funs: Vec<(&str, Box<dyn FnOnce(&TypesSummary)>)> = vec![
            (
                "{ let y: { x: String, y: Int }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: String, y: Int, a: { b: { c: { d: { e: String }}}} }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 5 + primitive_and_main_len);
                }),
            ),
            (
                "{ let x: Int = 5; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 0 + primitive_and_main_len);
                }),
            ),
            (
                "{ let x: { a: Char, b: Char }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: { y: Int } }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 2 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: Int }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 1 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: Int, y: String, z: { x: Int } }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 2 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: Int, y: String, z: { x: Int }, w: () }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 2 + primitive_and_main_len);
                }),
            ),
            (
                "{ let a: { b: { c: { d: { e: { f: { a: Int }}}}}}; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len() - primitive_and_main_len, 6);
                }),
            ),
            (
                "{ let y: { x: String } | { y: String }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 3 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: String } | { y: String } | { z: String }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 4 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: { x: String } | { y: String } | { z: String } } }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len() - primitive_and_main_len, 5);
                }),
            ),
            (
                "{ let y: { x: String } | { y: String } | { z: String } | { u: String } | { v: String } | { w: String }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 7 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { x: String } | { x: String } | { x: String } | { x: String } | { x: String } | { x: String }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 2 + primitive_and_main_len);
                }),
            ),
            (
                "{ let y: { abc: { x: String, y: String }, abc2: { x: String, y: String } }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 2 + primitive_and_main_len);
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
                }],
                ..Default::default()
            };

            let mut type_env = TypeEnv::default();
            typeresolve_source_file(&mut source_file, &mut type_env);

            let summary = type_env.summarize();

            println!("------------------------------------");
            println!("source: \n{src}");
            println!("types used:");
            summary
                .types_used
                .iter()
                .map(|type_expr| type_expr.as_clean_go_type_name(&mut type_env))
                .for_each(|type_name| println!("\t{type_name}"));

            println!("------------------------------------");

            summary_check_fun(&summary);
        }
    }

    #[test]
    fn test_type_compatibility_success() {
        let mut type_env = TypeEnv::default();

        let success_cases = vec![
            (TypeExpr::Int, TypeExpr::Int),
            (TypeExpr::String, TypeExpr::String),
            (TypeExpr::Int, TypeExpr::Float),
            (TypeExpr::Float, TypeExpr::Int),
            (
                TypeExpr::Tuple(vec![
                    empty_spanned(TypeExpr::Int),
                    empty_spanned(TypeExpr::String),
                ]),
                TypeExpr::Tuple(vec![
                    empty_spanned(TypeExpr::Int),
                    empty_spanned(TypeExpr::String),
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
                    empty_spanned(TypeExpr::String),
                ]),
                TypeExpr::Int,
            ),
            (
                TypeExpr::Or(vec![
                    empty_spanned(TypeExpr::Int),
                    empty_spanned(TypeExpr::String),
                    empty_spanned(TypeExpr::Bool),
                ]),
                TypeExpr::Or(vec![
                    empty_spanned(TypeExpr::String),
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
            &empty_spanned(TypeExpr::String),
            &mut type_env,
        );
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_str() {
        let mut type_env = TypeEnv::default();
        check_type_compatability(
            &empty_spanned(TypeExpr::String),
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
            &empty_spanned(TypeExpr::String),
            &mut type_env,
        );
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_tuples_different_types() {
        let mut type_env = TypeEnv::default();

        let one = TypeExpr::Tuple(vec![empty_spanned(TypeExpr::Int)]);
        let two = TypeExpr::Tuple(vec![empty_spanned(TypeExpr::String)]);

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_incompatible_tuples_different_length() {
        let mut type_env = TypeEnv::default();

        let one = TypeExpr::Tuple(vec![empty_spanned(TypeExpr::Int)]);
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
            fields: vec![Field::new("x".to_string(), empty_spanned(TypeExpr::String))],
        });

        check_type_compatability(&empty_spanned(one), &empty_spanned(two), &mut type_env);
    }

    #[test]
    #[should_panic(expected = "Incompatible Types")]
    fn test_type_not_in_variant() {
        let mut type_env = TypeEnv::default();

        let variant = TypeExpr::Or(vec![
            empty_spanned(TypeExpr::Int),
            empty_spanned(TypeExpr::String),
        ]);
        let a_bool = TypeExpr::Bool;

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
            empty_spanned(TypeExpr::Int),
            empty_spanned(TypeExpr::String),
        ]);
        let sub_variant = TypeExpr::Or(vec![
            empty_spanned(TypeExpr::Int),
            empty_spanned(TypeExpr::Bool),
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
