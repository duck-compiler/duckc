use std::{
    collections::{HashMap, HashSet, VecDeque},
    panic,
};

use crate::{
    emit::{fix_ident_for_go, function::function_epilogue_2, types::escape_string_for_go},
    parse::{
        SS, Spanned,
        duckx_component_parser::find_client_components,
        failure, failure_with_occurence,
        function_parser::LambdaFunctionExpr,
        struct_parser::NamedDuckDefinition,
        type_parser::{Duck, TypeExpr},
        value_parser::{
            Declaration, ValFmtStringContents, ValHtmlStringContents, ValueExpr, empty_range,
        },
    },
    semantics::{
        ident_mangler::{MANGLE_SEP, mangle},
        type_resolve::TypeEnv,
    },
};

#[derive(Debug, Clone, Default)]
pub struct ToIr {
    pub var_counter: usize,
    pub labels: Vec<String>,
}

/// Expression further down should use this
/// if they want the result
type IrRes = String;

type Identifier = String;
type Param = (String, String);
type ReturnType = Option<String>;

#[derive(Debug, Clone, PartialEq)]
pub enum IrInstruction {
    GlobalVarDecl {
        name: String,
        go_type: String,
        init_code: Vec<IrInstruction>,
    },
    Defer(Box<IrInstruction>),
    ForRangeElem {
        ident: String,
        range_target: IrValue,
        body: Vec<IrInstruction>,
        label: String,
    },

    // Code Statements
    VarDecl(String, String),
    VarAssignment(IrRes, IrValue),
    FunCall(Option<IrRes>, IrValue, Vec<IrValue>), //
    StringConcat(IrRes, Vec<IrValue>),
    Add(IrRes, IrValue, IrValue, TypeExpr),
    Mul(IrRes, IrValue, IrValue, TypeExpr),
    Sub(IrRes, IrValue, IrValue, TypeExpr),
    Mod(IrRes, IrValue, IrValue, TypeExpr),
    Div(IrRes, IrValue, IrValue, TypeExpr),
    Equals(IrRes, IrValue, IrValue, TypeExpr),
    NotEquals(IrRes, IrValue, IrValue, TypeExpr),
    LessThan(IrRes, IrValue, IrValue, TypeExpr),
    LessThanOrEquals(IrRes, IrValue, IrValue, TypeExpr),
    GreaterThan(IrRes, IrValue, IrValue, TypeExpr),
    GreaterThanOrEquals(IrRes, IrValue, IrValue, TypeExpr),
    And(IrRes, IrValue, IrValue, TypeExpr),
    Or(IrRes, IrValue, IrValue, TypeExpr),
    Break(Option<String>),
    Continue(Option<String>),
    Return(Option<IrValue>),
    InlineGo(String),
    If(
        IrValue,                    // bool_value
        Vec<IrInstruction>,         // body
        Option<Vec<IrInstruction>>, // else
    ),
    Loop(Vec<IrInstruction>, String),
    Block(Vec<IrInstruction>),

    // Top-Level Statements
    GoPackage(String),
    GoImports(Vec<(Option<String>, String)>),
    GenericFun(
        String,                // Name
        Vec<(String, String)>, // Generics
        Vec<(String, String)>, // Params
        Option<String>,        // Return Type
        Vec<IrInstruction>,    // Body
    ),
    FunDef(
        String,                   // Name
        Option<(String, String)>, // Receiver
        Vec<(String, String)>,    // Params
        Option<String>,           // Return Type
        Vec<IrInstruction>,       // Body
    ),
    StructDef(String, Vec<(String, String)>),
    InterfaceDef(
        Identifier,                                // Name
        Vec<(String, String)>,                     // Generics
        Vec<(Identifier, Vec<Param>, ReturnType)>, // Methods
    ),

    SwitchType(IrValue, Vec<Case>),
}

type IrCondition = (Vec<IrInstruction>, Option<IrValue>);

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub type_name: String,
    pub instrs: Vec<IrInstruction>,
    pub identifier_binding: Option<String>,
    pub condition: Option<Spanned<ValueExpr>>,
    pub conditional_branches: Option<Vec<(IrCondition, Case)>>,
    pub span: SS,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrValue {
    Int(u64),
    Float(f64),
    String(String, bool),
    Bool(bool),
    Char(char),
    Array(String, Vec<IrValue>),
    Lambda(
        Vec<(String, String)>, // params
        Option<String>,        // return type
        Vec<IrInstruction>,    // body
    ),
    Tuple(String, Vec<IrValue>),
    Duck(String, Vec<(String, IrValue)>),
    Struct(String, Vec<(String, IrValue)>),
    Tag(String),
    Var(String),
    BoolNegate(Box<IrValue>),
    FieldAccess(Box<IrValue>, String),
    ArrayAccess(Box<IrValue>, Box<IrValue>),
    Imm(String),
    Pointer(Box<IrValue>),
    Negate(Box<IrValue>),
    Deref(Box<IrValue>),
    Nil,
}

impl IrValue {
    pub fn empty_tuple() -> Self {
        Self::Tuple(
            TypeExpr::from_value_expr(
                &ValueExpr::Tuple(vec![]).into_empty_span(),
                &mut TypeEnv::default(),
            )
            .0
            .as_go_type_annotation(&mut TypeEnv::default()),
            vec![],
        )
    }
}

impl ToIr {
    pub fn top_label_cloned(&self) -> Option<String> {
        self.labels.last().cloned()
    }

    pub fn new_label(&mut self) -> &String {
        let label = format!("label_{}", self.var_counter);
        self.var_counter += 1;
        self.labels.push(label);
        &self.labels[self.labels.len() - 1]
    }

    pub fn new_var(&mut self) -> String {
        let var_name = format!("var_{}", self.var_counter);
        self.var_counter += 1;
        var_name
    }
}

pub fn as_rvar(s: impl Into<String>) -> Option<IrValue> {
    Some(IrValue::Var(s.into()))
}

pub fn as_var(s: impl Into<String>) -> IrValue {
    IrValue::Var(s.into())
}

pub fn needs_mut(v: &ValueExpr, type_env: &mut TypeEnv) -> bool {
    match v {
        ValueExpr::VarAssign(_) => true,
        ValueExpr::FunctionCall {
            target,
            params: _,
            type_params: _,
            ..
        } => {
            if let ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } = &target.0
            {
                let ty = TypeExpr::from_value_expr_dereferenced(target_obj, type_env);
                if let TypeExpr::TypeName(_, type_name, _) = ty.0
                    && let Some(struct_def) = type_env.get_struct_def_opt(&type_name)
                    && struct_def.mut_methods.contains(&field_name.to_string())
                {
                    return true;
                }
            }
            false
        }
        _ => false,
    }
}

#[allow(dead_code)]
fn contains_imm_ref(mut t: &TypeExpr) -> bool {
    while let TypeExpr::RefMut(to) = t {
        if matches!(&to.0, TypeExpr::Ref(..)) {
            return false;
        }
        t = &to.0;
    }
    true
}

pub fn can_do_mut_stuff_through2(
    v: &Spanned<ValueExpr>,
    type_env: &mut TypeEnv,
    mut var_needs_const: bool,
) -> bool {
    let ty = TypeExpr::from_value_expr(v, type_env);

    if matches!(ty.0, TypeExpr::Ref(..)) {
        return false;
    }

    if matches!(ty.0, TypeExpr::RefMut(..)) {
        var_needs_const = false;
    }

    if let ValueExpr::ArrayAccess(target_obj, _) = &v.0 {
        can_do_mut_stuff_through2(target_obj, type_env, var_needs_const)
    } else if let ValueExpr::FieldAccess {
        target_obj,
        field_name: _,
    } = &v.0
    {
        can_do_mut_stuff_through2(target_obj, type_env, var_needs_const)
    } else {
        !var_needs_const || !matches!(&v.0, ValueExpr::Variable(_, _, _, Some(true), _))
    }
}

pub fn can_do_mut_stuff_through(v: &Spanned<ValueExpr>, type_env: &mut TypeEnv) -> bool {
    can_do_mut_stuff_through2(v, type_env, true)
}

#[derive(Debug, Clone, PartialEq)]
enum FrontPart {
    Deref(usize),
    ExtCall(String, usize),
    ExtCall2(String, usize),
}

fn walk_access_raw(
    obj: &Spanned<ValueExpr>,
    type_env: &mut TypeEnv,
    env: &mut ToIr,
    span: SS,
    only_read: bool,
    deref_needs_to_be_mut: bool,
    last_needs_mut: bool,
) -> (Vec<IrInstruction>, Option<Vec<String>>) {
    let imports = type_env.all_go_imports;

    let mut res_instr = VecDeque::new();
    let mut current_obj = obj.clone();
    let mut s = VecDeque::new();

    let mut derefs = Vec::new();
    let mut stars = 0;

    let mut is_calling_fun = false;

    loop {
        let cloned = is_calling_fun;
        is_calling_fun = false;
        match current_obj.0.clone() {
            ValueExpr::Variable(_, name, _type_expr, is_const, needs_copy) => {
                if needs_copy {
                    let (emit_instr, Some(IrValue::Var(var_name))) =
                        current_obj.0.emit(type_env, env, current_obj.1)
                    else {
                        panic!("this should be a var")
                    };

                    emit_instr
                        .into_iter()
                        .rev()
                        .for_each(|i| res_instr.push_front(i));

                    s.push_front(var_name);
                } else {
                    s.push_front(name.clone());
                }

                s[0] = fix_ident_for_go(&s[0], imports);

                if is_const.is_some_and(|v| v) && last_needs_mut && stars == 0 && !cloned {
                    failure(
                        current_obj.1.context.file_name,
                        format!("NEED LET VAR {stars} {name}"),
                        ("need let var".to_string(), current_obj.1),
                        [],
                        current_obj.1.context.file_contents,
                    );
                }

                if stars > 0 {
                    derefs.push(FrontPart::Deref(stars));
                    s[0].push(')');
                }
                break;
            }
            ValueExpr::ArrayAccess(target_obj, index) => {
                let (instr, res) = index.0.emit(type_env, env, span);

                let mut type_expr = TypeExpr::from_value_expr(&target_obj, type_env);

                let mut stars_to_set = 0;
                if deref_needs_to_be_mut {
                    if !can_do_mut_stuff_through(&target_obj, type_env) {
                        failure_with_occurence(
                            "This needs to allow mutable access",
                            target_obj.1,
                            [(
                                "This needs to allow mutable access".to_string(),
                                target_obj.1,
                            )],
                        );
                    }

                    while let TypeExpr::RefMut(v) = type_expr.0 {
                        type_expr.0 = v.0;
                        stars_to_set += 1;
                    }

                    if let TypeExpr::Ref(_) = type_expr.0 {
                        panic!(
                            "need only mut refs for mut stuff {}..{}",
                            span.start, span.end
                        );
                    }
                } else {
                    while let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = type_expr.0 {
                        type_expr.0 = v.0;
                        stars_to_set += 1;
                    }
                }

                instr
                    .into_iter()
                    .rev()
                    .for_each(|x| res_instr.push_front(x));
                if let Some(res) = res {
                    let IrValue::Var(res) = res else {
                        panic!("need var");
                    };

                    s.push_front(format!("[{res}]"));
                } else {
                    return (res_instr.into(), None);
                }

                if stars > 0 {
                    derefs.push(FrontPart::Deref(stars));
                    s[0].push(')');
                }

                stars = stars_to_set;

                current_obj = *target_obj;
            }
            ValueExpr::FunctionCall {
                target,
                params,
                type_params,
                ..
            } => {
                is_calling_fun = true;
                let mut param_res = Vec::new();
                let mut flag = None;

                if let ValueExpr::FieldAccess {
                    target_obj,
                    field_name,
                } = &target.0
                {
                    let (target_field_type, stars_count) =
                        TypeExpr::from_value_expr_dereferenced_with_count(target_obj, type_env);

                    let clean_go_type_name = target_field_type.0.as_clean_go_type_name(type_env);
                    let mut skip = false;
                    if field_name.as_str() == "iter_mut"
                        && target_field_type.0.implements_into_iter_mut(type_env)
                    {
                        if let TypeExpr::Array(..) = target_field_type.clone().0 {
                            if !can_do_mut_stuff_through(target_obj, type_env) {
                                failure_with_occurence(
                                    "This needs to allow mutable access",
                                    target.1,
                                    [(
                                        "This needs to allow mutable access".to_string(),
                                        target_obj.1,
                                    )],
                                );
                            }

                            flag = Some((
                                format!("{clean_go_type_name}_IterMut("),
                                stars_count,
                                false,
                            ));
                            skip = true;
                        }
                    } else if field_name.as_str() == "iter"
                        && target_field_type.0.implements_into_iter(type_env)
                    {
                        if let TypeExpr::Array(..) = target_field_type.clone().0 {
                            flag =
                                Some((format!("{clean_go_type_name}_Iter("), stars_count, false));
                            skip = true;
                        }
                    } else if field_name.as_str() == "len" && target_field_type.0.is_array() {
                        if let TypeExpr::Array(..) = target_field_type.clone().0 {
                            flag = Some(("len(".to_string(), stars_count, false));
                            skip = true;
                        }
                    } else if field_name.as_str() == "to_string"
                        && target_field_type.0.implements_to_string(type_env)
                    {
                        match target_field_type.clone().0 {
                            TypeExpr::Array(..) => {
                                flag = Some((
                                    format!("{clean_go_type_name}_ToString("),
                                    stars_count,
                                    false,
                                ));
                                skip = true;
                            }
                            TypeExpr::String(..) => {
                                flag =
                                    Some((r#"fmt.Sprintf("%s", "#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Int => {
                                flag =
                                    Some((r#"fmt.Sprintf("%d", "#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::UInt => {
                                flag =
                                    Some((r#"fmt.Sprintf("%d", "#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Or(t) => {
                                let mut go_code = r#"
                                    var p1 any = param1

                                "#
                                .to_string();

                                for t in t {
                                    let conc_type = t.0.as_go_type_annotation(type_env);
                                    go_code.push('\n');
                                    go_code.push_str(&format!(
                                        r#"
                                        switch p1.(type) {{
                                        case {conc_type}:
                                            tmp := p1.({conc_type})
                                            _ = tmp
                                            return {}
                                        }}
                                    "#,
                                        t.0.call_to_string("tmp", type_env)
                                    ));
                                }

                                go_code.push_str("\nreturn \"\"");
                                let c = format!("func(param1 any) string {{ {go_code} }}(");

                                flag = Some((c, stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Bool(..) => {
                                flag =
                                    Some((r#"fmt.Sprintf("%t", "#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Char => {
                                flag =
                                    Some((r#"fmt.Sprintf("%c", "#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Tag(t) => {
                                flag = Some((
                                    format!(r#"(func(_ any) string {{ return ".{t}" }})("#),
                                    stars_count,
                                    false,
                                ));
                                skip = true;
                            }
                            TypeExpr::Float => {
                                flag =
                                    Some((r#"fmt.Sprintf("%f", "#.to_string(), stars_count, false));
                                skip = true;
                            }
                            _ => {}
                        }
                    } else if field_name.as_str() == "clone"
                        && target_field_type.0.implements_clone(type_env)
                    {
                        match target_field_type.0.clone() {
                            TypeExpr::Array(..) => {
                                flag = Some((
                                    format!("{clean_go_type_name}_Clone("),
                                    stars_count,
                                    false,
                                ));
                                skip = true;
                            }
                            TypeExpr::String(..) => {
                                flag = Some((r#"IDENTITY("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Int => {
                                flag = Some((r#"IDENTITY("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::UInt => {
                                flag = Some((r#"IDENTITY("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Bool(..) => {
                                flag = Some((r#"IDENTITY("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Char => {
                                flag = Some((r#"IDENTITY("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Tag(..) => {
                                flag = Some((r#"IDENTITY("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Float => {
                                flag = Some((r#"IDENTITY("%f", "#.to_string(), stars_count, false));
                                skip = true;
                            }
                            _ => {}
                        }
                    } else if field_name.as_str() == "hash"
                        && target_field_type.0.implements_hash(type_env)
                    {
                        match target_field_type.clone().0 {
                            TypeExpr::Array(..) => {
                                flag = Some((
                                    format!("{clean_go_type_name}_Hash("),
                                    stars_count,
                                    false,
                                ));
                                skip = true;
                            }
                            TypeExpr::String(..) => {
                                flag = Some((r#"String_Hash("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Int => {
                                flag = Some((r#"Int_Hash("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::UInt => {
                                flag = Some((r#"UInt_Hash("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Bool(..) => {
                                flag = Some((r#"Bool_Hash("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Char => {
                                flag = Some((r#"Char_Hash("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Tag(t) => {
                                flag = Some((format!(r#"String_Hash("{t}""#), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Float => {
                                flag = Some((r#"Float_Hash("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            _ => {}
                        }
                    } else if field_name.as_str() == "ord"
                        && target_field_type.0.implements_ord(type_env)
                    {
                        match target_field_type.clone().0 {
                            TypeExpr::Array(..) => {
                                flag = Some((
                                    format!("{clean_go_type_name}_Ord("),
                                    stars_count,
                                    false,
                                ));
                                skip = true;
                            }
                            TypeExpr::String(..) => {
                                flag = Some((r#"String_Ord("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Int => {
                                flag = Some((r#"Int_Ord("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::UInt => {
                                flag = Some((r#"UInt_Ord("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Bool(..) => {
                                flag = Some((r#"Bool_Ord("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Char => {
                                flag = Some((r#"Char_Ord("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            TypeExpr::Float => {
                                flag = Some((r#"Float_Ord("#.to_string(), stars_count, false));
                                skip = true;
                            }
                            _ => {}
                        }
                    }

                    if !skip {
                        let extension_fn_name = target_field_type
                            .0
                            .build_extension_access_function_name(field_name, type_env);
                        let extension_fn = type_env.extension_functions.get(&extension_fn_name);

                        if extension_fn.is_some() {
                            flag = Some((extension_fn_name, stars_count, true));
                        }

                        match target_field_type.0 {
                            TypeExpr::Struct {
                                name: struct_name,
                                type_params: struct_type_params,
                            } => {
                                let struct_def = type_env.get_struct_def_with_type_params_mut(
                                    struct_name.as_str(),
                                    &struct_type_params,
                                    empty_range(),
                                );
                                if struct_def.mut_methods.contains(field_name)
                                    && !can_do_mut_stuff_through(target_obj, type_env)
                                {
                                    failure_with_occurence(
                                        "This needs to allow mutable access",
                                        target.1,
                                        [(
                                            "This needs to allow mutable access".to_string(),
                                            target_obj.1,
                                        )],
                                    );
                                }
                            }
                            TypeExpr::Duck(duck) => {
                                if let Some(duck_field) = duck
                                    .fields
                                    .iter()
                                    .find(|field| field.name.as_str() == field_name.as_str())
                                    && let TypeExpr::Fun(_, _, true) = duck_field.type_expr.0
                                    && !can_do_mut_stuff_through(target_obj, type_env)
                                {
                                    failure_with_occurence(
                                        "This needs to allow mutable access",
                                        target.1,
                                        [(
                                            "This needs to allow mutable access".to_string(),
                                            target_obj.1,
                                        )],
                                    );
                                }
                            }
                            _ => {}
                        }
                    }
                }

                for param in params {
                    let (param_instr, var) = param.0.emit(type_env, env, span);
                    res_instr.extend(param_instr);

                    if let Some(res) = var {
                        let (IrValue::Var(res) | IrValue::Imm(res)) = res else {
                            panic!("need var {res:?}");
                        };

                        param_res.push(fix_ident_for_go(&res, imports));
                    } else {
                        return (res_instr.into(), None);
                    }
                }

                let mut type_expr = TypeExpr::from_value_expr(&target, type_env);

                let mut stars_to_set = 0;
                if deref_needs_to_be_mut {
                    while let TypeExpr::RefMut(v) = type_expr.0 {
                        type_expr.0 = v.0;
                        stars_to_set += 1;
                    }
                    if let TypeExpr::Ref(_) = type_expr.0 {
                        panic!(
                            "need only mut refs for mut stuff {}..{}",
                            span.start, span.end
                        );
                    }
                } else {
                    while let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = type_expr.0 {
                        type_expr.0 = v.0;
                        stars_to_set += 1;
                    }
                }

                if let Some((f, stars_count, is_wrapped)) = flag.as_ref() {
                    let mut param_res = param_res.join(", ");
                    if *is_wrapped {
                        derefs.push(FrontPart::ExtCall(f.clone(), *stars_count));
                        s.push_front(format!(")({param_res})"));
                    } else {
                        derefs.push(FrontPart::ExtCall2(f.clone(), *stars_count));
                        if !param_res.is_empty() {
                            param_res.insert_str(0, ", ");
                        }
                        s.push_front(format!("{param_res})"));
                    }
                } else {
                    s.push_front(format!("({})", param_res.join(", ")));
                }

                if stars > 0 {
                    derefs.push(FrontPart::Deref(stars));
                    s[0].push(')');
                }

                stars = stars_to_set;
                current_obj = if let ValueExpr::Variable(a, var_name, b, c, needs_copy) = &target.0
                    && !type_params.is_empty()
                {
                    let generic_name = [var_name.clone()]
                        .into_iter()
                        .chain(
                            type_params
                                .iter()
                                .map(|(t, _)| t.as_clean_go_type_name(type_env)),
                        )
                        .collect::<Vec<_>>()
                        .join(MANGLE_SEP);
                    (
                        ValueExpr::Variable(*a, generic_name, b.clone(), *c, *needs_copy),
                        target.1,
                    )
                } else if let ValueExpr::FieldAccess {
                    target_obj,
                    field_name,
                } = &target.0
                {
                    let generic_name = [field_name.clone()]
                        .into_iter()
                        .chain(
                            type_params
                                .iter()
                                .map(|(t, _)| t.as_clean_go_type_name(type_env)),
                        )
                        .collect::<Vec<_>>()
                        .join(MANGLE_SEP);
                    (
                        if flag.is_some() {
                            target_obj.0.clone()
                        } else {
                            ValueExpr::FieldAccess {
                                target_obj: target_obj.clone(),
                                field_name: generic_name.clone(),
                            }
                        },
                        target.1,
                    )
                } else {
                    *target
                };
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let mut type_expr = TypeExpr::from_value_expr(&target_obj, type_env);
                let fixed_field_name = fix_ident_for_go(&field_name, imports);

                let mut stars_to_set = 0;
                if deref_needs_to_be_mut {
                    if !can_do_mut_stuff_through(&target_obj, type_env) {
                        failure_with_occurence(
                            "This needs to allow mutable access",
                            target_obj.1,
                            [(
                                "This needs to allow mutable access".to_string(),
                                target_obj.1,
                            )],
                        );
                    }

                    while let TypeExpr::RefMut(v) = type_expr.0 {
                        type_expr.0 = v.0;
                        stars_to_set += 1;
                    }
                    if let TypeExpr::Ref(_) = type_expr.0 {
                        panic!(
                            "need only mut refs for mut stuff {}..{}",
                            span.start, span.end
                        );
                    }
                } else {
                    while let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = type_expr.0 {
                        type_expr.0 = v.0;
                        stars_to_set += 1;
                    }
                }

                let type_expr = TypeExpr::from_value_expr_dereferenced(&target_obj, type_env);

                match type_expr.0 {
                    TypeExpr::Tuple(t) => {
                        if field_name.as_str() == "to_string"
                            && t.iter().all(|t| t.0.implements_to_string(type_env))
                        {
                            s.push_front("to_string".to_string());
                        } else if field_name.as_str() == "clone"
                            && t.iter().all(|t| t.0.implements_clone(type_env))
                        {
                            s.push_front("clone".to_string());
                        } else if field_name.as_str() == "hash"
                            && t.iter().all(|t| t.0.implements_hash(type_env))
                        {
                            s.push_front("hash".to_string());
                        } else if field_name.as_str() == "ord"
                            && t.iter().all(|t| t.0.implements_ord(type_env))
                        {
                            s.push_front("ord".to_string());
                        } else {
                            s.push_front(format!("field_{field_name}"));
                        }
                    }
                    TypeExpr::Duck(Duck { fields }) => {
                        let found_field = fields
                            .iter()
                            .find(|x| x.name == field_name)
                            .expect("Field doesn't exist");
                        if found_field.type_expr.0.is_array()
                            || found_field.type_expr.0.ref_is_array()
                            || found_field.type_expr.0.is_duck()
                            || found_field.type_expr.0.is_struct()
                            || found_field.type_expr.0.is_fun()
                            || found_field.type_expr.0.is_primitive()
                            || matches!(found_field.type_expr.0, TypeExpr::Or(..))
                            || only_read
                        {
                            s.push_front(format!("Get{field_name}()"));
                        } else {
                            s.push_front(format!("GetPtr{field_name}()"));
                        }
                    }
                    TypeExpr::NamedDuck {
                        ref name,
                        ref type_params,
                    } => {
                        let NamedDuckDefinition {
                            name: _,
                            fields,
                            generics: _,
                        } = type_env.get_duck_def_with_type_params_mut(
                            name,
                            type_params,
                            empty_range(),
                        );
                        let found_field = fields
                            .iter()
                            .find(|x| x.name == field_name)
                            .expect("Field doesn't exist");
                        if found_field.type_expr.0.is_array()
                            || found_field.type_expr.0.ref_is_array()
                            || found_field.type_expr.0.is_duck()
                            || found_field.type_expr.0.is_struct()
                            || found_field.type_expr.0.is_fun()
                            || found_field.type_expr.0.is_primitive()
                            || matches!(found_field.type_expr.0, TypeExpr::Or(..))
                            || only_read
                        {
                            s.push_front(format!("Get{field_name}()"));
                        } else {
                            s.push_front(format!("GetPtr{field_name}()"));
                        }
                    }
                    TypeExpr::Struct { .. } => s.push_front(fixed_field_name),
                    _ => {}
                }

                s[0].insert(0, '.');

                if stars > 0 {
                    derefs.push(FrontPart::Deref(stars));
                    s[0].push(')');
                }

                stars = stars_to_set;

                current_obj = *target_obj;
            }
            _ => {
                // if only_read {
                let mut type_expr = TypeExpr::from_value_expr(&current_obj, type_env);

                if deref_needs_to_be_mut {
                    if !can_do_mut_stuff_through(&current_obj, type_env) {
                        failure_with_occurence(
                            "This needs to allow mutable access",
                            current_obj.1,
                            [(
                                "This needs to allow mutable access".to_string(),
                                current_obj.1,
                            )],
                        );
                    }
                    while let TypeExpr::RefMut(v) = type_expr.0 {
                        type_expr.0 = v.0;
                    }
                    if let TypeExpr::Ref(_) = type_expr.0 {
                        panic!(
                            "need only mut refs for mut stuff {}..{} {}",
                            span.start,
                            span.end,
                            &span.context.file_contents[span.end - 20..],
                        );
                    }
                } else {
                    while let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = type_expr.0 {
                        type_expr.0 = v.0;
                    }
                }

                let (instr_emit, instr_res) = current_obj.0.emit(type_env, env, span);
                res_instr.extend(instr_emit);
                if let Some(instr_res) = instr_res {
                    let IrValue::Var(n) = instr_res else {
                        panic!("no var? {instr_res:?}")
                    };
                    s.push_front(n);
                } else {
                    return (res_instr.into(), None);
                }

                if stars > 0 {
                    derefs.push(FrontPart::Deref(stars));
                    s[0].push(')');
                }
                break;
                // } else {
                //     panic!("need var, got {current_obj:?}")
                // }
            }
        }
    }

    for front_part in derefs.iter().rev() {
        match front_part {
            FrontPart::Deref(deref) => {
                for _ in 0..*deref {
                    s[0].insert(0, '*');
                }
                s[0].insert(0, '(');
            }
            FrontPart::ExtCall(e, stars_count) => {
                for _ in 0..*stars_count {
                    s[0].insert(0, '*');
                }
                s[0].insert_str(0, e);
                s[0].insert(e.len(), '(');
            }
            FrontPart::ExtCall2(e, stars_count) => {
                for _ in 0..*stars_count {
                    s[0].insert(0, '*');
                }
                s[0].insert_str(0, e);
            }
        }
    }
    (res_instr.into(), Some(Into::<Vec<String>>::into(s)))
}

fn walk_access(
    obj: &Spanned<ValueExpr>,
    type_env: &mut TypeEnv,
    env: &mut ToIr,
    span: SS,
    only_read: bool,
    derefs_need_to_be_mut: bool,
    last_needs_mut: bool,
) -> (Vec<IrInstruction>, Option<String>) {
    let (i, r) = walk_access_raw(
        obj,
        type_env,
        env,
        span,
        only_read,
        derefs_need_to_be_mut,
        last_needs_mut,
    );
    (i, r.map(|v| v.join("")))
}

fn emit_array(
    exprs: &Vec<Spanned<ValueExpr>>,
    arr_type: &TypeExpr,
    type_env: &mut TypeEnv,
    env: &mut ToIr,
    span: SS,
) -> (Vec<IrInstruction>, Option<IrValue>) {
    let mut total_instr = Vec::new();
    let mut array_contents = Vec::new();

    for expr in exprs {
        let (expr_instr, expr_res) = expr.0.direct_or_with_instr(type_env, env, span);
        total_instr.extend(expr_instr);
        if let Some(expr_res) = expr_res {
            array_contents.push(expr_res);
        } else {
            return (total_instr, None);
        }
    }

    let res_var_name = env.new_var();
    total_instr.extend([
        IrInstruction::VarDecl(
            res_var_name.clone(),
            arr_type.as_go_type_annotation(type_env),
        ),
        IrInstruction::VarAssignment(
            res_var_name.clone(),
            IrValue::Array(arr_type.as_go_type_annotation(type_env), array_contents),
        ),
    ]);

    (total_instr, Some(IrValue::Var(res_var_name)))
}

impl ValueExpr {
    pub fn direct_emit(&self, type_env: &mut TypeEnv, env: &mut ToIr, span: SS) -> Option<IrValue> {
        match self {
            ValueExpr::Bool(b) => Some(IrValue::Bool(*b)),
            ValueExpr::Char(c) => Some(IrValue::Char(*c)),
            ValueExpr::Int(i, _ty) => Some(IrValue::Int(*i)),
            ValueExpr::Float(f) => Some(IrValue::Float(*f)),
            ValueExpr::String(s, is_const) => Some(IrValue::String(s.clone(), *is_const)),
            ValueExpr::Lambda(b) => {
                let LambdaFunctionExpr {
                    is_mut: _,
                    params,
                    return_type,
                    value_expr,
                } = &**b;

                let mut rparams = Vec::new();
                for p in params {
                    rparams.push((
                        p.0.clone(),
                        p.1.as_ref().unwrap().0.as_go_type_annotation(type_env),
                    ));
                }

                let return_type = return_type
                    .as_ref()
                    .or(Some((TypeExpr::Tuple(vec![]), span)).as_ref())
                    .as_ref()
                    .map(|(x, _)| x.as_go_type_annotation(type_env))
                    .unwrap();

                let (mut b_instr, b_res) = value_expr.0.emit(type_env, env, span);
                if let Some(b_res) = b_res {
                    b_instr.push(IrInstruction::Return(b_res.into()));
                }
                b_instr.push(function_epilogue_2(&return_type));
                Some(IrValue::Lambda(rparams, Some(return_type), b_instr))
            }
            _ => None,
        }
    }

    pub fn direct_or_with_instr(
        &self,
        type_env: &mut TypeEnv,
        env: &mut ToIr,
        span: SS,
    ) -> (Vec<IrInstruction>, Option<IrValue>) {
        if let Some(ir_value) = self.direct_emit(type_env, env, span) {
            (Vec::new(), Some(ir_value))
        } else {
            let (instr, res) = self.emit(type_env, env, span);
            (instr, res)
        }
    }

    pub fn emit(
        &self,
        type_env: &mut TypeEnv,
        env: &mut ToIr,
        span: SS,
    ) -> (Vec<IrInstruction>, Option<IrValue>) {
        match self {
            ValueExpr::Negate(t) => {
                let (mut inner_instr, inner_res) = t.0.emit(type_env, env, t.1);

                let Some(r) = inner_res else {
                    return (inner_instr, None);
                };

                let var_name = env.new_var();
                let inner_t = TypeExpr::from_value_expr(&(self.clone(), span), type_env);
                inner_instr.extend([
                    IrInstruction::VarDecl(
                        var_name.clone(),
                        inner_t.0.as_clean_go_type_name(type_env),
                    ),
                    IrInstruction::VarAssignment(var_name.clone(), IrValue::Negate(r.into())),
                ]);

                (inner_instr, as_rvar(var_name))
            }
            ValueExpr::RawStruct { .. } => {
                panic!("Compiler Bug: Raw struct should be replaced {self:?}")
            }
            ValueExpr::Async(e) => {
                let return_type = TypeExpr::from_value_expr(&(self.clone(), span), type_env);
                let inner_return_type = TypeExpr::from_value_expr(e, type_env);

                let var_name = env.new_var();
                let mut res_instr = Vec::new();

                res_instr.push(IrInstruction::VarDecl(
                    var_name.clone(),
                    return_type.0.as_go_type_annotation(type_env),
                ));
                res_instr.push(IrInstruction::VarAssignment(
                    var_name.clone(),
                    IrValue::Imm(format!(
                        "{}()",
                        mangle(&[
                            "std",
                            "sync",
                            "Channel",
                            "new",
                            &inner_return_type.0.as_clean_go_type_name(type_env),
                        ])
                    )),
                ));

                let ValueExpr::FunctionCall {
                    target: e_target,
                    params: e_params,
                    type_params: _,
                } = &e.0
                else {
                    panic!("Compiler Bug: Can only async on function calls");
                };

                let (e_target_instr, e_target_res) = e_target.0.emit(type_env, env, e_target.1);

                res_instr.extend(e_target_instr);

                let Some(IrValue::Var(e_target_res) | IrValue::Imm(e_target_res)) = e_target_res
                else {
                    return (res_instr, None);
                };

                let mut e_params_res = Vec::new();

                for e_param in e_params {
                    let (e_param_instr, e_param_res) = e_param.0.emit(type_env, env, e_param.1);
                    res_instr.extend(e_param_instr);
                    let Some(IrValue::Var(e_param_res) | IrValue::Imm(e_param_res)) = e_param_res
                    else {
                        return (res_instr, None);
                    };
                    e_params_res.push(e_param_res);
                }

                let call_e = IrInstruction::InlineGo(format!(
                    "go func() {{ async_res := {e_target_res}({})\n{var_name}.send(async_res) }}()",
                    e_params_res.join(",")
                ));
                res_instr.push(call_e);
                (res_instr, as_rvar(var_name))
            }
            ValueExpr::Defer(e) => {
                let (mut inner_emit, _) = e.0.emit(type_env, env, span);
                let last = inner_emit.pop().expect("nothing emitted?");
                match last {
                    IrInstruction::VarAssignment(.., IrValue::Imm(call)) => {
                        inner_emit.push(IrInstruction::Defer(Box::new(IrInstruction::InlineGo(
                            call.clone(),
                        ))));
                    }
                    IrInstruction::FunCall(_, target, params) => {
                        inner_emit.push(IrInstruction::Defer(Box::new(IrInstruction::FunCall(
                            None, target, params,
                        ))));
                    }
                    _ => panic!("invalid for defer {inner_emit:?}"),
                };
                (inner_emit, None)
            }
            ValueExpr::As(v, t) => {
                if let ValueExpr::Array(exprs) = &v.0 {
                    emit_array(exprs, &t.0, type_env, env, span)
                } else if matches!(t.0, TypeExpr::Int | TypeExpr::UInt | TypeExpr::Float) {
                    let new_type = t.0.as_go_type_annotation(type_env);
                    let (mut res_instr, res) = v.0.emit(type_env, env, span);

                    let Some(IrValue::Var(go_v) | IrValue::Imm(go_v)) = res else {
                        return (res_instr, None);
                    };

                    let var_name = env.new_var();
                    res_instr.extend([
                        IrInstruction::VarDecl(var_name.clone(), new_type.clone()),
                        IrInstruction::VarAssignment(
                            var_name.clone(),
                            IrValue::Var(format!("{new_type}({go_v})")),
                        ),
                    ]);
                    (res_instr, as_rvar(var_name))
                } else {
                    v.0.emit(type_env, env, v.1)
                }
            }
            ValueExpr::For {
                ident: (ident, _, ident_type),
                target,
                block,
            } => {
                let ident_type = ident_type.as_ref().expect("needs type");
                let ident_type_anno = ident_type.as_go_type_annotation(type_env);
                let (mut target_instr, target_res) = target.0.emit(type_env, env, span);

                let target_type = TypeExpr::from_value_expr(target, type_env);

                let TypeExpr::Struct {
                    name: _,
                    type_params,
                } = target_type.0
                else {
                    panic!("Compiler Bug: For only works with iter struct {target_type:?}")
                };

                if let Some(target_res_var_name) = target_res {
                    let label = env.new_label().clone();
                    let (body_res_instr, _) = block.0.emit(type_env, env, span);
                    env.labels.pop();

                    let iter_res_var = env.new_var();
                    let case_tmp_var = env.new_var();

                    let mut body = Vec::new();
                    body.extend([
                        IrInstruction::VarDecl(ident.clone(), ident_type_anno.clone()),
                        IrInstruction::VarDecl(iter_res_var.clone(), "any".to_string()),
                        IrInstruction::FunCall(
                            Some(iter_res_var.clone()),
                            IrValue::FieldAccess(
                                Box::new(target_res_var_name.clone()),
                                "next".to_string(),
                            ),
                            vec![],
                        ),
                        IrInstruction::SwitchType(
                            IrValue::Var(iter_res_var.clone()),
                            vec![
                                Case {
                                    type_name: type_params[0].0.as_go_type_annotation(type_env),
                                    instrs: vec![IrInstruction::VarAssignment(
                                        ident.clone(),
                                        IrValue::Var(case_tmp_var.clone()),
                                    )],
                                    identifier_binding: Some(case_tmp_var.clone()),
                                    condition: None,
                                    conditional_branches: None,
                                    span: empty_range(),
                                },
                                Case {
                                    type_name: "Tag__no_next_elem".to_string(),
                                    instrs: vec![IrInstruction::Break(Some(label.clone()))],
                                    identifier_binding: None,
                                    condition: None,
                                    conditional_branches: None,
                                    span: empty_range(),
                                },
                            ],
                        ),
                    ]);
                    body.extend(body_res_instr);
                    let as_loop = IrInstruction::Loop(body, label);
                    target_instr.push(as_loop);
                    (target_instr, None)
                } else {
                    (target_instr, None)
                }
            }
            ValueExpr::Deref(v) => {
                let target_type = TypeExpr::from_value_expr(&(self.clone(), span), type_env);
                let res_type = target_type.0.as_go_type_annotation(type_env).to_string();
                let (mut emit_instr, emit_res) = v.0.emit(type_env, env, span);
                if let Some(emit_res) = emit_res {
                    let var_name = env.new_var();
                    let ptr_var_decl = [
                        IrInstruction::VarDecl(var_name.clone(), res_type),
                        IrInstruction::VarAssignment(
                            var_name.clone(),
                            IrValue::Deref(Box::new(emit_res)),
                        ),
                    ];
                    emit_instr.extend(ptr_var_decl);
                    (emit_instr, Some(IrValue::Var(var_name)))
                } else {
                    (emit_instr, None)
                }
            }
            ValueExpr::Ref(v) | ValueExpr::RefMut(v) => {
                let t = TypeExpr::from_value_expr(v, type_env);
                let ptr_type = format!("*{}", t.0.as_go_type_annotation(type_env));

                let need_mut = matches!(self, ValueExpr::RefMut(..));

                #[allow(clippy::never_loop)]
                loop {
                    let target_obj = if let ValueExpr::FieldAccess {
                        target_obj,
                        field_name: _,
                    }
                    | ValueExpr::ArrayAccess(target_obj, _) = &v.0
                    {
                        Some(target_obj.as_ref().clone())
                    } else if let ValueExpr::Variable(..) = &v.0 {
                        Some(v.as_ref().clone())
                    } else {
                        None
                    };

                    if let Some(target_obj) = target_obj {
                        let (mut walk_instr, walk_res) =
                            walk_access_raw(v, type_env, env, span, false, false, need_mut);
                        let t = TypeExpr::from_value_expr_dereferenced(&target_obj, type_env);
                        let is_accessing_duck = matches!(t.0, TypeExpr::Duck(..));
                        if let Some(mut walk_res) = walk_res {
                            let val_to_set: IrValue;
                            if is_accessing_duck
                                && let ValueExpr::FieldAccess {
                                    target_obj: _,
                                    field_name,
                                } = &v.0
                            {
                                let r = walk_res.last_mut().expect("not last?");
                                *r = r.replacen(
                                    &format!("Get{field_name}()"),
                                    &format!("GetPtr{field_name}()"),
                                    1,
                                );
                                val_to_set = IrValue::Imm(walk_res.join(""));
                            } else {
                                val_to_set =
                                    IrValue::Pointer(Box::new(IrValue::Imm(walk_res.join(""))));
                            }

                            let var_name = env.new_var();
                            let ptr_var_decl = [
                                IrInstruction::VarDecl(var_name.clone(), ptr_type),
                                IrInstruction::VarAssignment(var_name.clone(), val_to_set),
                            ];
                            walk_instr.extend(ptr_var_decl);
                            break (walk_instr, Some(IrValue::Var(var_name)));
                        } else {
                            break (walk_instr, None);
                        }
                    } else {
                        let (mut normal_emit_instr, normal_emit_res) = v.0.emit(type_env, env, span);
                        if let Some(emit_res) = normal_emit_res {
                            let var_name = env.new_var();
                            let ptr_var_decl = [
                                IrInstruction::VarDecl(var_name.clone(), ptr_type),
                                IrInstruction::VarAssignment(
                                    var_name.clone(),
                                    IrValue::Pointer(emit_res.into()),
                                ),
                            ];
                            normal_emit_instr.extend(ptr_var_decl);
                            break (normal_emit_instr, Some(IrValue::Var(var_name)));
                        } else {
                            break (normal_emit_instr, None);
                        }
                    }
                }
            }
            ValueExpr::Sub(lhs, rhs) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = lhs.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }
                let (v2_instr, v2_res) = rhs.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let type_expr = TypeExpr::from_value_expr(lhs, type_env).0.unconst();

                let var = env.new_var();
                ir.push(IrInstruction::VarDecl(
                    var.clone(),
                    type_expr.as_go_type_annotation(type_env),
                ));

                ir.push(IrInstruction::Sub(
                    var.clone(),
                    v1_res.unwrap(),
                    v2_res.unwrap(),
                    type_expr,
                ));

                (ir, as_rvar(var))
            }
            ValueExpr::Div(lhs, rhs) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = lhs.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }
                let (v2_instr, v2_res) = rhs.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let type_expr = TypeExpr::from_value_expr(lhs, type_env).0.unconst();

                let var = env.new_var();
                ir.push(IrInstruction::VarDecl(
                    var.clone(),
                    type_expr.as_go_type_annotation(type_env),
                ));

                ir.push(IrInstruction::Div(
                    var.clone(),
                    v1_res.unwrap(),
                    v2_res.unwrap(),
                    type_expr,
                ));

                (ir, as_rvar(var))
            }
            ValueExpr::Mod(lhs, rhs) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = lhs.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }
                let (v2_instr, v2_res) = rhs.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let type_expr = TypeExpr::from_value_expr(lhs, type_env).0.unconst();

                let var = env.new_var();
                ir.push(IrInstruction::VarDecl(
                    var.clone(),
                    type_expr.as_go_type_annotation(type_env),
                ));

                ir.push(IrInstruction::Mod(
                    var.clone(),
                    v1_res.unwrap(),
                    v2_res.unwrap(),
                    type_expr,
                ));

                (ir, as_rvar(var))
            }
            ValueExpr::HtmlString(contents) => {
                let mut component_dependencies = HashSet::new();
                find_client_components(contents, &mut component_dependencies, type_env);

                let mut contents = contents.clone();
                let mut i = 0;

                let mut instr = Vec::new();

                let mut render_calls_to_push = Vec::new();

                'outer: while i < contents.len() {
                    let current = &mut contents[i];
                    match current {
                        ValHtmlStringContents::String(s) => {
                            *s = s.replace("<>", "").replace("</>", "");
                            let mut j = 0;

                            while !s.is_empty() && j < s.len() - 1 {
                                while j < s.len() && !s.is_char_boundary(j) {
                                    j += 1;
                                }

                                let slice = &s[j..];
                                if slice.starts_with("<") && !slice.starts_with("</") {
                                    let mut end_of_tag = None;
                                    for tok in ["/>", ">"] {
                                        let found = slice.find(tok);
                                        if let Some(found) = found {
                                            if let Some(min) = end_of_tag
                                                && min < found
                                            {
                                                continue;
                                            }
                                            end_of_tag = Some(found);
                                        }
                                    }

                                    let end_index = end_of_tag.unwrap_or(slice.len());
                                    let end_of_ident =
                                        slice[..end_index].find(" ").unwrap_or(end_index);
                                    let found = &slice[1..end_of_ident];
                                    let cloned_ident = found.to_string();
                                    if component_dependencies.contains(found) {
                                        let mut o = Vec::new();
                                        let mut full_str = slice[..end_index].to_string();
                                        full_str.insert(end_of_ident, '}');
                                        full_str.insert_str(1, "${");
                                        o.push(ValHtmlStringContents::String(full_str.clone()));
                                        s.drain(j..j + slice[..end_index].len());

                                        let skip = s[j..].starts_with("/>");

                                        if skip {
                                            s.drain(j..j + 2);
                                        }

                                        let start = i + 1;
                                        let mut i2 = start;

                                        while !skip && i2 < contents.len() {
                                            let n = &mut contents[i2];
                                            match n {
                                                ValHtmlStringContents::Expr(_) => {
                                                    o.push(n.clone());
                                                }
                                                ValHtmlStringContents::String(s) => {
                                                    let close_tag = s.find("/>");
                                                    if let Some(idx) = close_tag {
                                                        o.push(ValHtmlStringContents::String(
                                                            s.drain(..(idx + 2)).collect(),
                                                        ));
                                                        break;
                                                    } else {
                                                        o.push(ValHtmlStringContents::String(
                                                            s.clone(),
                                                        ));
                                                    }
                                                }
                                            }
                                            i2 += 1;
                                        }

                                        let mut html_str = String::new();
                                        let mut printf_vars = Vec::new();

                                        for part in o {
                                            match part {
                                                ValHtmlStringContents::String(s) => {
                                                    html_str.push_str(&s)
                                                }
                                                ValHtmlStringContents::Expr(e) => {
                                                    let (e_instr, e_res) =
                                                        e.0.emit(type_env, env, e.1);
                                                    instr.extend(e_instr);
                                                    if let Some(e_res) = e_res {
                                                        let IrValue::Var(e_res) = e_res else {
                                                            panic!("no var {e_res:?}")
                                                        };
                                                        html_str.push_str("${%s}");
                                                        printf_vars.push(e_res);
                                                    } else {
                                                        return (instr, None);
                                                    }
                                                }
                                            }
                                        }
                                        if skip {
                                            html_str.push_str("/>");
                                        }

                                        let id = format!("{}_{}", cloned_ident, env.new_var());
                                        let html_to_return =
                                            format!("<div duckx-render=\"{id}\"></div>");

                                        render_calls_to_push.push((
                                            format!(
                                                "fmt.Sprintf(\"{}\", {})",
                                                escape_string_for_go(&html_str),
                                                printf_vars
                                                    .iter()
                                                    .map(|x| format!(
                                                        "emit_go_to_js({})",
                                                        escape_string_for_go(x)
                                                    ))
                                                    .collect::<Vec<_>>()
                                                    .join(", "),
                                            ),
                                            id,
                                        ));

                                        let ValHtmlStringContents::String(s) = &mut contents[i]
                                        else {
                                            panic!()
                                        };

                                        s.insert_str(j, &html_to_return);

                                        contents.drain(start..i2);
                                        if i > 1 {
                                            i -= 1;
                                        }

                                        continue 'outer;
                                    } else if true
                                        && let Some(duckx_component) =
                                            type_env.get_duckx_component(found).cloned()
                                    {
                                        let mut o = Vec::new();
                                        let full_str = slice[..end_index].to_string();
                                        o.push(ValHtmlStringContents::String(
                                            full_str[1 + found.len()..].to_string(),
                                        ));
                                        s.drain(j..j + slice[..end_index].len());
                                        let skip = s[j..].starts_with("/>");

                                        if skip {
                                            s.drain(j..j + 2);
                                        }

                                        let start = i + 1;
                                        let mut i2 = start;

                                        let mut props_init = HashMap::new();
                                        let mut current_param = None::<String>;

                                        while !skip && i2 < contents.len() {
                                            let n = &mut contents[i2];
                                            match n {
                                                ValHtmlStringContents::Expr(_) => {
                                                    o.push(n.clone());
                                                }
                                                ValHtmlStringContents::String(s) => {
                                                    let close_tag = s.find("/>");
                                                    if let Some(idx) = close_tag {
                                                        let f = s
                                                            .drain(..(idx + 2))
                                                            .collect::<String>();
                                                        o.push(ValHtmlStringContents::String(
                                                            f[..f.len() - 2].to_string(),
                                                        ));
                                                        break;
                                                    } else {
                                                        o.push(ValHtmlStringContents::String(
                                                            s.clone(),
                                                        ));
                                                    }
                                                }
                                            }
                                            i2 += 1;
                                        }

                                        for part in o {
                                            match part {
                                                ValHtmlStringContents::Expr(e) => {
                                                    let current_param_name =
                                                        current_param.expect("no param provided");
                                                    current_param = None;
                                                    let (e_instr, e_res) =
                                                        e.0.emit(type_env, env, e.1);
                                                    instr.extend(e_instr);
                                                    if let Some(e_res) = e_res {
                                                        let IrValue::Var(e_res) = e_res else {
                                                            panic!("not a var? {e_res:?}")
                                                        };
                                                        props_init.insert(
                                                            current_param_name,
                                                            (
                                                                e_res,
                                                                TypeExpr::from_value_expr(
                                                                    &e, type_env,
                                                                ),
                                                            ),
                                                        );
                                                    } else {
                                                        return (instr, None);
                                                    }
                                                }
                                                ValHtmlStringContents::String(s) => {
                                                    if s == "/>" {
                                                        continue;
                                                    }
                                                    if let Some(param) = current_param.as_ref() {
                                                        panic!("{param} has no value");
                                                    }
                                                    let first_non_space =
                                                        s.find(|x: char| x != ' ');
                                                    if let Some(first_non_space) = first_non_space {
                                                        let slice = &s[first_non_space..];
                                                        let end =
                                                            slice.find(' ').unwrap_or(slice.len());
                                                        let slice = &slice[..end];
                                                        if let Some(equals_idx) = slice.find('=') {
                                                            if equals_idx == 0 {
                                                                panic!("needs param name");
                                                            }
                                                            if slice
                                                                .get(
                                                                    equals_idx + 1..=equals_idx + 1,
                                                                )
                                                                .filter(|x| *x != " ")
                                                                .is_some()
                                                            {
                                                                panic!("== not allowed");
                                                            }
                                                            current_param = Some(
                                                                slice[..equals_idx].to_string(),
                                                            );
                                                        } else {
                                                            panic!("wrong syntax (= missing)");
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                        let TypeExpr::Duck(Duck { fields }) =
                                            duckx_component.props_type.0.clone()
                                        else {
                                            panic!("not taking a duck??")
                                        };

                                        if fields.iter().any(|f| !props_init.contains_key(&f.name))
                                        {
                                            panic!("missing fields");
                                        }

                                        if fields.len() != props_init.len() {
                                            panic!("too many fields");
                                        }

                                        let mut props_init =
                                            props_init.into_iter().collect::<Vec<_>>();
                                        props_init.sort_by_key(|f| f.0.clone());

                                        if !skip {
                                            contents.drain(start..i2);
                                        }
                                        contents.insert(
                                            if skip { i } else { start },
                                            ValHtmlStringContents::Expr(
                                                ValueExpr::FunctionCall {
                                                    target: ValueExpr::Variable(
                                                        true,
                                                        duckx_component.name.clone(),
                                                        Some(TypeExpr::Fun(
                                                            vec![(
                                                                None,
                                                                duckx_component.props_type.clone(),
                                                            )],
                                                            TypeExpr::Html.into_empty_span().into(),
                                                            false,
                                                        )),
                                                        Some(false),
                                                        false,
                                                    )
                                                    .into_empty_span()
                                                    .into(),
                                                    params: vec![
                                                        ValueExpr::Duck(
                                                            props_init
                                                                .into_iter()
                                                                .map(|(a, b)| {
                                                                    (
                                                                        a,
                                                                        ValueExpr::Variable(
                                                                            true,
                                                                            b.0.clone(),
                                                                            Some(b.1.0),
                                                                            Some(false),
                                                                            true,
                                                                        )
                                                                        .into_empty_span(),
                                                                    )
                                                                })
                                                                .collect(),
                                                        )
                                                        .into_empty_span(),
                                                    ],
                                                    type_params: vec![],
                                                }
                                                .into_empty_span(),
                                            ),
                                        );

                                        if i > 1 {
                                            i -= 1;
                                        }

                                        continue 'outer;
                                    }
                                }
                                j += 1;
                            }
                        }
                        ValHtmlStringContents::Expr(_) => {}
                    }
                    i += 1;
                }

                let var_name = env.new_var();
                instr.push(IrInstruction::VarDecl(
                    var_name.clone(),
                    "func (env *TemplEnv) string".to_string(),
                ));

                let mut return_printf = String::new();
                let mut return_printf_vars = Vec::new();

                for elem in &contents {
                    match elem {
                        ValHtmlStringContents::String(s) => {
                            return_printf.push_str(&s.replace("%", "%%"))
                        }
                        ValHtmlStringContents::Expr(e) => {
                            let ty = TypeExpr::from_value_expr(e, type_env);
                            match ty.0 {
                                TypeExpr::Html => {
                                    let (e_instr, e_res_var) = e.0.emit(type_env, env, e.1);
                                    instr.extend(e_instr);
                                    if let Some(e_res_var) = e_res_var {
                                        let IrValue::Var(var_name) = e_res_var else {
                                            panic!("not a var {e_res_var:?}")
                                        };
                                        return_printf.push_str("%s");
                                        return_printf_vars.push(format!("{var_name}(env)"));
                                    } else {
                                        return (instr, None);
                                    }
                                }
                                TypeExpr::String(..) => {
                                    let (e_instr, e_res_var) = e.0.emit(type_env, env, e.1);
                                    instr.extend(e_instr);
                                    if let Some(e_res_var) = e_res_var {
                                        let IrValue::Var(var_name) = e_res_var else {
                                            panic!("not a var {e_res_var:?}")
                                        };
                                        return_printf.push_str("%s");
                                        return_printf_vars.push(var_name.to_string());
                                    } else {
                                        return (instr, None);
                                    }
                                }
                                TypeExpr::Int => {
                                    let (e_instr, e_res_var) = e.0.emit(type_env, env, e.1);
                                    instr.extend(e_instr);
                                    if let Some(e_res_var) = e_res_var {
                                        let IrValue::Var(var_name) = e_res_var else {
                                            panic!("not a var {e_res_var:?}")
                                        };
                                        return_printf.push_str("%v");
                                        return_printf_vars.push(var_name.to_string());
                                    } else {
                                        return (instr, None);
                                    }
                                }
                                TypeExpr::Bool(..) => {
                                    let (e_instr, e_res_var) = e.0.emit(type_env, env, e.1);
                                    instr.extend(e_instr);
                                    if let Some(e_res_var) = e_res_var {
                                        let IrValue::Var(var_name) = e_res_var else {
                                            panic!("not a var {e_res_var:?}")
                                        };
                                        return_printf.push_str("%v");
                                        return_printf_vars.push(var_name.to_string());
                                    } else {
                                        return (instr, None);
                                    }
                                }
                                TypeExpr::Float => {
                                    let (e_instr, e_res_var) = e.0.emit(type_env, env, e.1);
                                    instr.extend(e_instr);
                                    if let Some(e_res_var) = e_res_var {
                                        let IrValue::Var(var_name) = e_res_var else {
                                            panic!("not a var {e_res_var:?}")
                                        };
                                        return_printf.push_str("%v");
                                        return_printf_vars.push(var_name.to_string());
                                    } else {
                                        return (instr, None);
                                    }
                                }
                                TypeExpr::Array(..) => {
                                    let (e_instr, e_res_var) = e.0.emit(type_env, env, e.1);
                                    instr.extend(e_instr);
                                    if let Some(e_res_var) = e_res_var {
                                        let IrValue::Var(var_name) = e_res_var else {
                                            panic!("not a var {e_res_var:?}")
                                        };
                                        return_printf.push_str("%s");
                                        return_printf_vars.push(format!(
                                            r#"
                                            func() string {{
                                            res := ""
                                            for _, e := range {var_name} {{
                                            res += e(env)
                                            }}
                                            return res
                                            }}()"#
                                        ));
                                    } else {
                                        return (instr, None);
                                    }
                                }
                                _ => panic!("not html string compatible {ty:?}"),
                            }
                        }
                    }
                }

                instr.push(IrInstruction::InlineGo(
                    [
                        format!("{var_name} = func (env *TemplEnv) string {{"),
                        component_dependencies.clone().into_iter().fold(
                            String::new(),
                            |mut acc, x| {
                                let src = type_env.get_component(x.as_str()).unwrap();
                                let js_src = format!(
                                    "function {}(props){{\n{}\n}}",
                                    src.name, src.javascript_source.0
                                );
                                acc.push_str(&format!(
                                    "env.push_client_component(\"{}\")\n",
                                    escape_string_for_go(js_src.as_str())
                                ));
                                acc
                            },
                        ),
                        render_calls_to_push.clone().into_iter().fold(
                            String::new(),
                            |mut acc, (js, id)| {
                                acc.push_str(&format!(
                                    "env.push_render({}, \"{}\")\n",
                                    js,
                                    escape_string_for_go(&id)
                                ));
                                acc
                            },
                        ),
                        if return_printf_vars.is_empty() {
                            format!(
                                "return fmt.Sprintf(\"{}\")",
                                escape_string_for_go(&return_printf)
                            )
                        } else {
                            format!(
                                "return fmt.Sprintf(\"{}\", {})",
                                escape_string_for_go(&return_printf),
                                return_printf_vars.join(", ")
                            )
                        },
                        "}".to_string(),
                    ]
                    .join("\n"),
                ));

                (instr, Some(IrValue::Var(var_name)))
            }
            ValueExpr::FormattedString(contents) => {
                let mut instr = Vec::new();

                let mut template = String::new();
                let mut concat_params = Vec::new();

                for c in contents {
                    match c {
                        ValFmtStringContents::String(s) => {
                            concat_params.push(IrValue::String(s.to_owned(), false))
                        }
                        ValFmtStringContents::Expr(expr) => {
                            template.push_str("%s");
                            let (param_instr, param_res) =
                                expr.0.direct_or_with_instr(type_env, env, span);
                            instr.extend(param_instr);
                            if param_res.is_none() {
                                return (instr, None);
                            }

                            concat_params.push(param_res.unwrap());
                        }
                    }
                }

                let res_name = env.new_var();

                instr.push(IrInstruction::VarDecl(res_name.clone(), "string".into()));
                instr.push(IrInstruction::StringConcat(res_name.clone(), concat_params));

                (instr, Some(IrValue::Var(res_name)))
            }
            ValueExpr::Match {
                value_expr,
                arms,
                else_arm,
                span,
            } => {
                let (mut instructions, match_on_res) = value_expr.0.emit(type_env, env, *span);
                let match_on_value = match match_on_res {
                    Some(v) => v,
                    None => return (instructions, None),
                };

                let result_type =
                    TypeExpr::from_value_expr(&(self.clone(), value_expr.1), type_env);
                let result_type_annotation = result_type.0.as_go_type_annotation(type_env);

                let result_var_name = env.new_var();
                instructions.push(IrInstruction::VarDecl(
                    result_var_name.clone(),
                    result_type_annotation,
                ));

                let mut cases = Vec::new();
                for arm in arms {
                    let type_name = arm.type_case.0.as_go_type_annotation(type_env);

                    let (mut arm_instrs, arm_res) =
                        arm.value_expr.0.direct_or_with_instr(type_env, env, *span);
                    if let Some(res) = arm_res {
                        arm_instrs.push(IrInstruction::VarAssignment(result_var_name.clone(), res));
                    }

                    cases.push(Case {
                        type_name,
                        instrs: arm_instrs,
                        identifier_binding: arm.identifier_binding.clone(),
                        condition: arm.condition.clone(),
                        conditional_branches: None,
                        span: arm.span,
                    });
                }

                if let Some(arm) = else_arm {
                    let _type_name = arm.type_case.0.as_clean_go_type_name(type_env);

                    let (mut arm_instrs, arm_res) =
                        arm.value_expr.0.direct_or_with_instr(type_env, env, *span);
                    if let Some(res) = arm_res {
                        arm_instrs.push(IrInstruction::VarAssignment(result_var_name.clone(), res));
                    }

                    cases.push(Case {
                        type_name: "__else".to_string(),
                        instrs: arm_instrs,
                        identifier_binding: arm.identifier_binding.clone(),
                        condition: arm.condition.clone(),
                        conditional_branches: None,
                        span: arm.span,
                    });
                }

                let mut merged_cases = vec![];
                for case in cases.clone().iter_mut() {
                    if merged_cases
                        .iter()
                        .any(|other_case: &Case| other_case.type_name == case.type_name)
                    {
                        continue;
                    }

                    let matching_cases = cases
                        .iter()
                        .filter(|ocase| **ocase != *case && ocase.type_name == case.type_name)
                        .collect::<Vec<_>>();

                    if case.condition.is_none() && matching_cases.is_empty() && else_arm.is_none() {
                        merged_cases.push(case.clone());
                        continue;
                    }

                    let the_one = cases.iter().find(|hopefully_the_one| {
                        hopefully_the_one.type_name == case.type_name
                            && hopefully_the_one.condition.is_none()
                    });

                    if the_one.is_none() && else_arm.is_none() {
                        failure_with_occurence(
                            "Unexhaustive Match",
                            *span,
                            vec![
                                (
                                    format!(
                                        "this only partially covers {} and you're not having an else branch",
                                        case.type_name
                                    ),
                                    case.span,
                                ),
                                (
                                    "not all possibilites are covered by this match".to_string(),
                                    *span,
                                ),
                            ],
                        );
                    }

                    let mut the_one = the_one.unwrap_or(case).clone();
                    let conditional_branches = cases
                        .iter()
                        .filter(|case| {
                            case.condition.is_some() && case.type_name == the_one.type_name
                        })
                        .map(|case| {
                            let cond = case.condition.as_ref().unwrap().clone();
                            (cond.0.emit(type_env, env, case.span), case.clone())
                        })
                        .collect::<Vec<_>>();

                    if !conditional_branches.is_empty() {
                        the_one.conditional_branches = Some(conditional_branches);
                    }

                    merged_cases.push(the_one);
                }

                let cases = merged_cases;

                let match_var = env.new_var();
                let (IrValue::Var(match_pre_var) | IrValue::Imm(match_pre_var)) = match_on_value
                else {
                    panic!("need var for match {match_on_value:?}")
                };

                instructions.push(IrInstruction::InlineGo(format!(
                    "\nvar {match_var} any\n_ = {match_var}\n{match_var} = {match_pre_var}"
                )));

                instructions.push(IrInstruction::SwitchType(IrValue::Var(match_var), cases));
                (instructions, as_rvar(result_var_name))
            }
            ValueExpr::ArrayAccess(target, idx) => {
                let (target_instr, target_res) = target.0.direct_or_with_instr(type_env, env, span);

                if target_res.is_none() {
                    return (target_instr, None);
                }

                let (idx_instr, idx_res) = idx.0.direct_or_with_instr(type_env, env, span);

                if idx_res.is_none() {
                    let mut v = Vec::new();
                    v.extend(target_instr);
                    v.extend(idx_instr);
                    return (v, None);
                }

                let mut res_instr = Vec::new();
                res_instr.extend(target_instr);
                res_instr.extend(idx_instr);

                let res_type =
                    TypeExpr::from_value_expr(&(self.clone(), target.as_ref().1), type_env)
                        .0
                        .as_go_type_annotation(type_env);
                let res_var_name = env.new_var();

                res_instr.push(IrInstruction::VarDecl(res_var_name.clone(), res_type));
                res_instr.push(IrInstruction::VarAssignment(
                    res_var_name.clone(),
                    IrValue::ArrayAccess(target_res.unwrap().into(), idx_res.unwrap().into()),
                ));

                (res_instr, Some(IrValue::Var(res_var_name)))
            }
            ValueExpr::Array(exprs) => {
                let arr_type = TypeExpr::from_value_expr(&(self.clone(), span), type_env);
                emit_array(exprs, &arr_type.0, type_env, env, span)
            }
            ValueExpr::VarDecl(b) => {
                let Declaration {
                    name,
                    type_expr,
                    initializer,
                    is_const: _,
                } = &b.0;

                let type_expression = type_expr
                    .as_ref()
                    .expect("compiler error: i expect that the type should be replaced by now")
                    .0
                    .as_go_type_annotation(type_env);

                let mut v = Vec::new();
                v.push(IrInstruction::VarDecl(name.clone(), type_expression));

                if let Some(initializer) = initializer.as_ref() {
                    if let Some(direct) = initializer.0.direct_emit(type_env, env, span) {
                        v.push(IrInstruction::VarAssignment(name.clone(), direct));
                    } else {
                        let (init_r, inti_r_res) =
                            walk_access(initializer, type_env, env, span, true, false, false);
                        v.extend(init_r);
                        if let Some(init_r_res) = inti_r_res {
                            v.push(IrInstruction::VarAssignment(
                                name.clone(),
                                IrValue::Imm(init_r_res),
                            ));
                        } else {
                            return (v, None);
                        }
                    }
                }

                (v, Some(IrValue::empty_tuple()))
            }
            ValueExpr::InlineGo(s, ty) => {
                let ty = ty
                    .as_ref()
                    .unwrap_or(&TypeExpr::unit())
                    .0
                    .as_go_type_annotation(type_env);
                let result_var = env.new_var();

                let mut res_instr = vec![IrInstruction::VarDecl(result_var.clone(), ty)];
                let inline_go_text = s.replace("$", &result_var);
                res_instr.push(IrInstruction::InlineGo(inline_go_text));

                (res_instr, Some(IrValue::Var(result_var)))
            }
            ValueExpr::While { condition, body } => {
                let (mut cond_instr, cond_res) =
                    condition.0.direct_or_with_instr(type_env, env, span);
                if cond_res.is_none() {
                    return (cond_instr, None);
                }

                let label = env.new_label().clone();

                let (body, _) = body.0.direct_or_with_instr(type_env, env, span);
                env.labels.pop();

                let cond_res = cond_res.unwrap();
                cond_instr.push(IrInstruction::If(
                    cond_res,
                    body,
                    Some(vec![IrInstruction::Break(Some(label.clone()))]),
                ));

                (vec![IrInstruction::Loop(cond_instr, label)], None)
            }
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                let res_type = TypeExpr::from_value_expr(&(self.clone(), span), type_env);
                let (mut i, cond_res) = condition.0.direct_or_with_instr(type_env, env, span);
                if cond_res.is_none() {
                    return (i, None);
                }

                let res_var_name = env.new_var();
                i.push(IrInstruction::VarDecl(
                    res_var_name.clone(),
                    res_type.0.as_go_type_annotation(type_env),
                ));

                let (mut then_instr, then_res) = then.0.direct_or_with_instr(type_env, env, span);
                if let Some(then_res) = then_res {
                    then_instr.push(IrInstruction::VarAssignment(res_var_name.clone(), then_res));
                }

                let r#else = r#else
                    .clone()
                    .map(|x| x.0.direct_or_with_instr(type_env, env, span))
                    .map(|mut x| {
                        if let Some(o) = x.1 {
                            x.0.push(IrInstruction::VarAssignment(res_var_name.clone(), o));
                        }
                        x.0
                    });

                i.push(IrInstruction::If(cond_res.unwrap(), then_instr, r#else));

                (i, Some(as_var(res_var_name)))
            }
            ValueExpr::VarAssign(b) => {
                let assign = &b.0;

                let mut a_res = None::<IrValue>;
                let mut res = Vec::new();

                if let Some(direct) = assign.value_expr.0.direct_emit(type_env, env, span) {
                    a_res = Some(direct);
                } else {
                    let (walk_instr, walk_res) =
                        walk_access(&assign.value_expr, type_env, env, span, true, false, false);
                    res.extend(walk_instr);
                    if let Some(walk_res) = walk_res {
                        a_res = Some(IrValue::Imm(walk_res));
                    }
                }

                if let Some(a_res) = a_res {
                    let target = &assign.target;
                    if let ValueExpr::FieldAccess {
                        target_obj,
                        field_name,
                    } = &target.0
                    {
                        let (walk_instr, walk_res) = walk_access_raw(
                            &target.clone(),
                            type_env,
                            env,
                            span,
                            false,
                            true,
                            true,
                        );
                        res.extend(walk_instr);
                        let target_res = match walk_res {
                            Some(mut s) => {
                                s.pop();
                                s
                            }
                            None => return (res, None),
                        }
                        .join("");

                        let target_ty =
                            TypeExpr::from_value_expr_dereferenced(target_obj, type_env);
                        match target_ty.0 {
                            TypeExpr::Duck(_) => {
                                res.push(IrInstruction::FunCall(
                                    None,
                                    IrValue::Var(format!("{target_res}.Set{field_name}")),
                                    vec![a_res],
                                ));
                            }
                            TypeExpr::Tuple(_) => {
                                res.push(IrInstruction::VarAssignment(
                                    format!("{target_res}.field_{field_name}"),
                                    a_res,
                                ));
                            }
                            TypeExpr::Struct { .. } => {
                                res.push(IrInstruction::VarAssignment(
                                    format!("{target_res}.{field_name}"),
                                    a_res,
                                ));
                            }
                            _ => panic!("can't set field on non object"),
                        }
                    } else if let ValueExpr::ArrayAccess(_, idx) = &target.0 {
                        //todo(@Apfelfrosch) handle indices of type ! properly (do it in rest of emit too)
                        let (idx_instr, Some(IrValue::Var(idx_res))) =
                            idx.0.emit(type_env, env, idx.1)
                        else {
                            panic!("no var: {idx:?}")
                        };

                        res.extend(idx_instr);

                        let (walk_instr, walk_res) =
                            walk_access_raw(target, type_env, env, span, false, true, false);
                        res.extend(walk_instr);
                        let target_res = match walk_res {
                            Some(mut s) => {
                                s.pop();
                                s
                            }
                            None => return (res, None),
                        }
                        .join("");

                        res.push(IrInstruction::VarAssignment(
                            format!("{target_res}[{idx_res}]"),
                            a_res,
                        ));
                    } else if let ValueExpr::Deref(..) = target.0 {
                        let mut target_to_use = target.clone();
                        let mut stars = String::new();

                        while let (ValueExpr::Deref(new_target), span) = target_to_use {
                            let target_type = TypeExpr::from_value_expr(
                                &(new_target.0.clone(), new_target.1),
                                type_env,
                            );

                            if !matches!(target_type.0, TypeExpr::RefMut(..)) {
                                if matches!(target_type.0, TypeExpr::Ref(..)) {
                                    failure_with_occurence(
                                        "Can only dereference a mutable reference",
                                        span,
                                        vec![(
                                            "This is not a mutable reference".to_string(),
                                            new_target.1,
                                        )],
                                    );
                                }
                                failure_with_occurence(
                                    "Can only dereference a reference",
                                    span,
                                    vec![("This is not a reference".to_string(), new_target.1)],
                                );
                            }

                            stars.push('*');
                            target_to_use = *new_target;
                        }

                        let (walk_instr, walk_res) =
                            walk_access(&target_to_use, type_env, env, span, false, true, false);
                        res.extend(walk_instr);
                        let target_res = match walk_res {
                            Some(s) => s,
                            None => return (res, None),
                        };
                        res.push(IrInstruction::VarAssignment(
                            format!("{stars}{target_res}"),
                            a_res,
                        ));
                    } else {
                        let (walk_instr, walk_res) =
                            walk_access(target, type_env, env, span, false, true, true);
                        res.extend(walk_instr);
                        let target_res = match walk_res {
                            Some(s) => s,
                            None => return (res, None),
                        };
                        res.push(IrInstruction::VarAssignment(target_res.to_string(), a_res));
                    }

                    (res, Some(IrValue::empty_tuple()))
                } else {
                    (res, None)
                }
            }
            ValueExpr::Add(v1, v2) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = v1.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }
                let (v2_instr, v2_res) = v2.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let type_expr = TypeExpr::from_value_expr(v1, type_env).0.unconst();

                let var = env.new_var();
                ir.push(IrInstruction::VarDecl(
                    var.clone(),
                    type_expr.as_go_type_annotation(type_env),
                ));

                ir.push(IrInstruction::Add(
                    var.clone(),
                    v1_res.unwrap(),
                    v2_res.unwrap(),
                    type_expr,
                ));

                (ir, as_rvar(var))
            }
            ValueExpr::Mul(v1, v2) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = v1.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }
                let (v2_instr, v2_res) = v2.0.direct_or_with_instr(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let type_expr = TypeExpr::from_value_expr(v1, type_env).0.unconst();

                let var = env.new_var();
                ir.push(IrInstruction::VarDecl(
                    var.clone(),
                    type_expr.as_go_type_annotation(type_env),
                ));

                ir.push(IrInstruction::Mul(
                    var.clone(),
                    v1_res.unwrap(),
                    v2_res.unwrap(),
                    type_expr,
                ));

                (ir, as_rvar(var))
            }
            ValueExpr::Block(block_exprs) => {
                let mut res_instr = Vec::new();
                let mut res_var = None;

                for block_expr in block_exprs.iter() {
                    let ty = TypeExpr::from_value_expr(block_expr, type_env);

                    let (block_instr, block_res) =
                        block_expr.0.direct_or_with_instr(type_env, env, span);

                    res_instr.extend(block_instr);

                    if ty.0.is_never() {
                        return (res_instr, None);
                    }

                    res_var = block_res;
                }

                let mut final_instr = Vec::new();
                let self_return_type = TypeExpr::from_value_expr(&(self.clone(), span), type_env);

                let fresvar = env.new_var();
                res_var = res_var.or(Some(IrValue::Tuple("Tup_".into(), vec![])));

                if self_return_type.0.is_never() {
                    res_var = None;
                } else {
                    final_instr.push(IrInstruction::VarDecl(
                        fresvar.clone(),
                        self_return_type.0.as_go_type_annotation(type_env),
                    ));
                    res_instr.push(IrInstruction::VarAssignment(
                        fresvar.clone(),
                        res_var.unwrap(),
                    ));
                    res_var = Some(IrValue::Var(fresvar));
                }

                if !res_instr.is_empty() {
                    final_instr.push(IrInstruction::Block(res_instr));
                }
                (final_instr, res_var)
            }
            ValueExpr::Tuple(fields) => {
                let mut res = Vec::new();
                let mut res_vars = Vec::new();
                let name = TypeExpr::from_value_expr(&(self.clone(), span), type_env)
                    .0
                    .as_go_type_annotation(type_env);

                for (field_expr, _) in fields {
                    let (field_instr, field_res) =
                        field_expr.direct_or_with_instr(type_env, env, span);
                    res.extend(field_instr);
                    if let Some(field_res) = field_res {
                        res_vars.push(field_res);
                    } else {
                        return (res, None);
                    }
                }

                let res_var = env.new_var();
                res.push(IrInstruction::VarDecl(res_var.clone(), name.clone()));
                res.push(IrInstruction::VarAssignment(
                    res_var.clone(),
                    IrValue::Tuple(name, res_vars),
                ));

                (res, as_rvar(res_var))
            }
            ValueExpr::BoolNegate(expr) => {
                let (mut instr, e_res_var) = expr.0.direct_or_with_instr(type_env, env, span);
                if let Some(e_res_var) = e_res_var {
                    let res = env.new_var();
                    instr.push(IrInstruction::VarDecl(res.clone(), "bool".into()));
                    instr.push(IrInstruction::VarAssignment(
                        res.clone(),
                        IrValue::BoolNegate(e_res_var.into()),
                    ));
                    (instr, as_rvar(res))
                } else {
                    (instr, None)
                }
            }
            ValueExpr::Break => (vec![IrInstruction::Break(env.top_label_cloned())], None),
            ValueExpr::Continue => (vec![IrInstruction::Continue(env.top_label_cloned())], None),
            ValueExpr::Return(expr) => {
                if let Some(expr) = expr {
                    let expr = &expr.0;
                    let (mut instr, res) = expr.direct_or_with_instr(type_env, env, span);
                    if res.is_some() {
                        instr.push(IrInstruction::Return(res));
                    }
                    (instr, None)
                } else {
                    (vec![IrInstruction::Return(None)], None)
                }
            }
            ValueExpr::FunctionCall {
                target: v_target,
                params,
                type_params,
                ..
            } => {
                // todo: type_params

                let v_target = &mut v_target.clone();
                let return_type = Some(
                    TypeExpr::from_value_expr(&self.clone().into_empty_span(), type_env)
                        .0
                        .into_empty_span(),
                );

                if !type_params.is_empty()
                    && let ValueExpr::FieldAccess {
                        target_obj: _,
                        field_name,
                    } = &mut v_target.0
                {
                    *field_name = [field_name.clone()]
                        .into_iter()
                        .chain(
                            type_params
                                .iter()
                                .map(|(t, _)| t.as_clean_go_type_name(type_env)),
                        )
                        .collect::<Vec<_>>()
                        .join(MANGLE_SEP);
                }

                let res = v_target.0.direct_emit(type_env, env, span);
                let mut instr = Vec::new();
                #[allow(clippy::unnecessary_unwrap)]
                if res.is_none() {
                    let (walk_instr, walk_res) = walk_access(
                        &(self.clone(), span),
                        type_env,
                        env,
                        span,
                        false,
                        false,
                        false,
                    );
                    instr.extend(walk_instr);
                    if walk_res.is_none() {
                        return (instr, None);
                    }
                    let walk_res = walk_res.unwrap();
                    if let Some(return_type) = return_type
                        && !return_type.0.is_never()
                    {
                        let res = env.new_var();
                        instr.push(IrInstruction::VarDecl(
                            res.clone(),
                            return_type.0.as_go_type_annotation(type_env),
                        ));
                        instr.push(IrInstruction::VarAssignment(
                            res.clone(),
                            IrValue::Imm(walk_res),
                        ));
                        (instr, Some(IrValue::Var(res)))
                    } else {
                        instr.push(IrInstruction::InlineGo(walk_res));
                        (instr, None)
                    }
                } else {
                    let call_target = res.unwrap();
                    let mut v_p_res = Vec::new();
                    for (param, _) in params {
                        let (p_instr, p_res) = param.direct_or_with_instr(type_env, env, span);
                        instr.extend(p_instr);
                        if let Some(p_res) = p_res {
                            v_p_res.push(p_res);
                        } else {
                            return (instr, None);
                        }
                    }

                    if let Some(return_type) = return_type
                        && !return_type.0.is_never()
                    {
                        let res = env.new_var();
                        instr.push(IrInstruction::VarDecl(
                            res.clone(),
                            return_type.0.as_go_type_annotation(type_env),
                        ));
                        instr.push(IrInstruction::FunCall(
                            Some(res.clone()),
                            call_target,
                            v_p_res,
                        ));
                        (instr, Some(IrValue::Var(res)))
                    } else {
                        instr.push(IrInstruction::FunCall(None, call_target, v_p_res));
                        (instr, None)
                    }
                }
            }
            ValueExpr::RawVariable(_, p) => (
                vec![],
                as_rvar(fix_ident_for_go(&mangle(p), type_env.all_go_imports)),
            ),
            ValueExpr::Variable(_, x, var_type, _, needs_copy) => {
                let x = fix_ident_for_go(x, type_env.all_go_imports);
                if *needs_copy {
                    let var_type = var_type
                        .as_ref()
                        .unwrap_or_else(|| panic!("Var {x} doesnt have type"));

                    let mut res_instr = Vec::new();
                    let res_var_name = env.new_var();
                    let anno = var_type.as_go_type_annotation(type_env);

                    res_instr.extend([
                        IrInstruction::VarDecl(res_var_name.clone(), anno.clone()),
                        IrInstruction::VarAssignment(
                            res_var_name.clone(),
                            IrValue::Imm(var_type.call_copy(&x, type_env)),
                        ),
                    ]);

                    (res_instr, as_rvar(res_var_name))
                } else {
                    (vec![], as_rvar(x.to_owned()))
                }
            }
            ValueExpr::Equals(v1, v2) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = v1.0.emit(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }
                let (v2_instr, v2_res) = v2.0.emit(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let Some(IrValue::Var(v1_var) | IrValue::Imm(v1_var)) = v1_res else {
                    panic!()
                };
                let Some(IrValue::Var(v2_var) | IrValue::Imm(v2_var)) = v2_res else {
                    panic!()
                };

                let t1 = TypeExpr::from_value_expr(v1, type_env);

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::VarAssignment(
                        var.clone(),
                        IrValue::Imm(t1.0.call_eq(&v1_var, &v2_var, type_env)),
                    ),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::NotEquals(lhs, rhs) => {
                let in_equals = ValueExpr::BoolNegate(
                    (ValueExpr::Equals(lhs.clone(), rhs.clone()), span).into(),
                );
                let (equals_ir, equals_res) = in_equals.emit(type_env, env, span);
                (equals_ir, equals_res)
            }
            ValueExpr::LessThan(lhs, rhs) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = lhs.0.emit(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }

                let (v2_instr, v2_res) = rhs.0.emit(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let Some(IrValue::Var(v1_var) | IrValue::Imm(v1_var)) = v1_res else {
                    panic!()
                };
                let Some(IrValue::Var(v2_var) | IrValue::Imm(v2_var)) = v2_res else {
                    panic!()
                };

                let ord_call = TypeExpr::from_value_expr(lhs, type_env)
                    .0
                    .call_ord(&v1_var, &v2_var, type_env);

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::InlineGo(format!(
                        r#"switch {ord_call}.(type) {{
                            case Tag__smaller:
                            {var} = true
                            default:
                            {var} = false
                            }}
                            "#
                    )),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::LessThanOrEquals(lhs, rhs) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = lhs.0.emit(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }

                let (v2_instr, v2_res) = rhs.0.emit(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let Some(IrValue::Var(v1_var) | IrValue::Imm(v1_var)) = v1_res else {
                    panic!()
                };
                let Some(IrValue::Var(v2_var) | IrValue::Imm(v2_var)) = v2_res else {
                    panic!()
                };

                let ord_call = TypeExpr::from_value_expr(lhs, type_env)
                    .0
                    .call_ord(&v1_var, &v2_var, type_env);

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::InlineGo(format!(
                        r#"switch {ord_call}.(type) {{
                            case Tag__smaller:
                            {var} = true
                            case Tag__equal:
                            {var} = true
                            default:
                            {var} = false
                            }}
                            "#
                    )),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::GreaterThan(lhs, rhs) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = lhs.0.emit(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }

                let (v2_instr, v2_res) = rhs.0.emit(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let Some(IrValue::Var(v1_var) | IrValue::Imm(v1_var)) = v1_res else {
                    panic!()
                };
                let Some(IrValue::Var(v2_var) | IrValue::Imm(v2_var)) = v2_res else {
                    panic!()
                };

                let ord_call = TypeExpr::from_value_expr(lhs, type_env)
                    .0
                    .call_ord(&v1_var, &v2_var, type_env);

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::InlineGo(format!(
                        r#"switch {ord_call}.(type) {{
                            case Tag__greater:
                            {var} = true
                            default:
                            {var} = false
                            }}
                            "#
                    )),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::GreaterThanOrEquals(lhs, rhs) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = lhs.0.emit(type_env, env, span);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }

                let (v2_instr, v2_res) = rhs.0.emit(type_env, env, span);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let Some(IrValue::Var(v1_var) | IrValue::Imm(v1_var)) = v1_res else {
                    panic!()
                };
                let Some(IrValue::Var(v2_var) | IrValue::Imm(v2_var)) = v2_res else {
                    panic!()
                };

                let ord_call = TypeExpr::from_value_expr(lhs, type_env)
                    .0
                    .call_ord(&v1_var, &v2_var, type_env);

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::InlineGo(format!(
                        r#"switch {ord_call}.(type) {{
                            case Tag__greater:
                            {var} = true
                            case Tag__equal:
                            {var} = true
                            default:
                            {var} = false
                            }}
                            "#
                    )),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::And(lhs, rhs) => {
                let mut ir = Vec::new();

                let (lhs_instr, lhs_res) = lhs.0.direct_or_with_instr(type_env, env, span);
                ir.extend(lhs_instr);
                let lhs_val = match lhs_res {
                    Some(val) => val,
                    None => return (ir, None),
                };

                let result_var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(result_var.clone(), "bool".into()),
                    IrInstruction::VarAssignment(result_var.clone(), lhs_val),
                ]);

                let (mut rhs_instr, rhs_res) = rhs.0.direct_or_with_instr(type_env, env, span);
                let rhs_val = rhs_res
                    .expect("The right-hand side of an 'and' expression must produce a value");

                rhs_instr.push(IrInstruction::VarAssignment(result_var.clone(), rhs_val));

                ir.push(IrInstruction::If(
                    IrValue::Var(result_var.clone()),
                    rhs_instr,
                    None,
                ));

                (ir, as_rvar(result_var))
            }
            ValueExpr::Or(lhs, rhs) => {
                let mut ir = Vec::new();

                let (lhs_instr, lhs_res) = lhs.0.direct_or_with_instr(type_env, env, span);
                ir.extend(lhs_instr);
                let lhs_val = match lhs_res {
                    Some(val) => val,
                    None => return (ir, None),
                };

                let result_var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(result_var.clone(), "bool".into()),
                    IrInstruction::VarAssignment(result_var.clone(), lhs_val),
                ]);

                let (mut rhs_instr, rhs_res) = rhs.0.direct_or_with_instr(type_env, env, span);
                let rhs_val = rhs_res
                    .expect("The right-hand side of an 'or' expression must produce a value");

                rhs_instr.push(IrInstruction::VarAssignment(result_var.clone(), rhs_val));

                ir.push(IrInstruction::If(
                    IrValue::BoolNegate(Box::new(IrValue::Var(result_var.clone()))),
                    rhs_instr,
                    None,
                ));

                (ir, as_rvar(result_var))
            }
            ValueExpr::FieldAccess {
                target_obj: _,
                field_name: _,
            } => {
                let (i, r) = walk_access(
                    &(self.clone(), span),
                    type_env,
                    env,
                    span,
                    true,
                    false,
                    false,
                );
                if let Some(t_res) = r {
                    return (i, Some(IrValue::Imm(t_res)));
                } else {
                    return (i, None);
                }
            }
            ValueExpr::Duck(fields) => {
                let name = TypeExpr::from_value_expr(&(self.clone(), span), type_env)
                    .0
                    .as_clean_go_type_name(type_env);

                let mut res = Vec::new();
                let mut res_vars = Vec::new();
                for (field_name, (field_expr, _)) in fields {
                    let (field_instr, field_res) =
                        field_expr.direct_or_with_instr(type_env, env, span);
                    res.extend(field_instr);
                    if let Some(field_res) = field_res {
                        res_vars.push((field_name.clone(), field_res));
                    } else {
                        return (res, None);
                    }
                }

                let res_var = env.new_var();
                res.extend([
                    IrInstruction::VarDecl(
                        res_var.clone(),
                        TypeExpr::from_value_expr(&(self.clone(), span), type_env)
                            .0
                            .as_go_type_annotation(type_env),
                    ),
                    IrInstruction::VarAssignment(res_var.clone(), IrValue::Duck(name, res_vars)),
                ]);

                (res, as_rvar(res_var))
            }
            ValueExpr::Struct { fields, .. } => {
                let name = TypeExpr::from_value_expr(&(self.clone(), span), type_env)
                    .0
                    .as_clean_go_type_name(type_env);

                let type_anno = TypeExpr::from_value_expr(&(self.clone(), span), type_env)
                    .0
                    .as_go_type_annotation(type_env);

                let mut res = Vec::new();
                let mut res_vars = Vec::new();
                for (field_name, (field_expr, _)) in fields {
                    let (field_instr, field_res) =
                        field_expr.direct_or_with_instr(type_env, env, span);
                    res.extend(field_instr);
                    if let Some(field_res) = field_res {
                        res_vars.push((field_name.clone(), field_res));
                    } else {
                        return (res, None);
                    }
                }

                let res_var = env.new_var();
                res.extend([
                    IrInstruction::VarDecl(res_var.clone(), type_anno),
                    IrInstruction::VarAssignment(res_var.clone(), IrValue::Struct(name, res_vars)),
                ]);

                (res, as_rvar(res_var))
            }
            ValueExpr::Tag(..) => {
                let type_name = TypeExpr::from_value_expr(&(self.clone(), span), type_env)
                    .0
                    .as_clean_go_type_name(type_env);

                let mut res = Vec::new();
                let res_var = env.new_var();
                res.extend([
                    IrInstruction::VarDecl(res_var.clone(), type_name.to_string()),
                    IrInstruction::VarAssignment(res_var.clone(), IrValue::Tag(type_name)),
                ]);

                (res, as_rvar(res_var))
            }
            ValueExpr::Int(..)
            | ValueExpr::Char(..)
            | ValueExpr::Lambda(..)
            | ValueExpr::Bool(..)
            | ValueExpr::Float(..)
            | ValueExpr::String(..) => {
                if let Some(d) = self.direct_emit(type_env, env, span) {
                    let ty = TypeExpr::from_value_expr(&(self.clone(), span), type_env);
                    let res_var = env.new_var();
                    let instr = vec![
                        IrInstruction::VarDecl(
                            res_var.clone(),
                            ty.0.as_go_type_annotation(type_env),
                        ),
                        IrInstruction::VarAssignment(res_var.clone(), d),
                    ];
                    (instr, Some(IrValue::Var(res_var)))
                } else {
                    todo!()
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use crate::{
        emit::value::{Case, IrInstruction, IrValue, ToIr},
        parse::{
            lexer::lex_parser,
            make_input,
            type_parser::TypeExpr,
            value_parser::{empty_range, value_expr_into_empty_range, value_expr_parser},
        },
        semantics::type_resolve::TypeEnv,
    };

    fn decl(name: impl Into<String>, t: impl Into<String>) -> IrInstruction {
        IrInstruction::VarDecl(name.into(), t.into())
    }

    #[test]
    fn test_code_emit() {
        let test_cases = vec![
            (
                "1 + 1",
                vec![
                    decl("var_0", "int"),
                    IrInstruction::Add(
                        "var_0".into(),
                        IrValue::Int(1),
                        IrValue::Int(1),
                        TypeExpr::Int,
                    ),
                ],
            ),
            (
                "1 * 1",
                vec![
                    decl("var_0", "int"),
                    IrInstruction::Mul(
                        "var_0".into(),
                        IrValue::Int(1),
                        IrValue::Int(1),
                        TypeExpr::Int,
                    ),
                ],
            ),
            (
                "let a: String = \"A\"",
                vec![
                    IrInstruction::VarDecl("a".into(), "string".into()),
                    IrInstruction::VarAssignment("a".into(), IrValue::String("A".into(), true)),
                ],
            ),
            (
                "{1;}",
                vec![
                    IrInstruction::VarDecl("var_1".to_string(), "Tup_".to_string()),
                    IrInstruction::Block(vec![
                        IrInstruction::VarDecl("var_0".to_string(), "Tup_".to_string()),
                        IrInstruction::VarAssignment(
                            "var_0".to_string(),
                            IrValue::Tuple("Tup_".to_string(), vec![]),
                        ),
                        IrInstruction::VarAssignment(
                            "var_1".to_string(),
                            IrValue::Var("var_0".to_string()),
                        ),
                    ]),
                ],
            ),
            (
                "!true",
                vec![
                    decl("var_0", "bool"),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::BoolNegate(IrValue::Bool(true).into()),
                    ),
                ],
            ),
            ("(true, break, 2)", vec![IrInstruction::Break(None)]),
            (
                "(true, return, 2)",
                vec![
                    IrInstruction::VarDecl("var_0".to_string(), "Tup_".to_string()),
                    IrInstruction::VarAssignment(
                        "var_0".to_string(),
                        IrValue::Tuple("Tup_".to_string(), vec![]),
                    ),
                    IrInstruction::Return(Some(IrValue::Var("var_0".to_string()))),
                ],
            ),
            ("(true, continue, 3)", vec![IrInstruction::Continue(None)]),
            (
                "[] as {}[]",
                vec![
                    IrInstruction::VarDecl("var_0".into(), "[]interface{}".into()),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Array("[]interface{}".into(), vec![]),
                    ),
                ],
            ),
            (
                "[[] as Int[]]",
                vec![
                    IrInstruction::VarDecl("var_0".into(), "[]int".into()),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Array("[]int".into(), vec![]),
                    ),
                    IrInstruction::VarDecl("var_1".into(), "[][]int".into()),
                    IrInstruction::VarAssignment(
                        "var_1".into(),
                        IrValue::Array("[][]int".into(), vec![IrValue::Var("var_0".into())]),
                    ),
                ],
            ),
            (
                "[1]",
                vec![
                    IrInstruction::VarDecl("var_0".into(), "[]int".into()),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Array("[]int".into(), vec![IrValue::Int(1)]),
                    ),
                ],
            ),
            (
                "{ x: 123 }",
                vec![
                    decl("var_0", "interface {\n   Hasx[int]\n}"),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Duck("Duck_x_int".into(), vec![("x".into(), IrValue::Int(123))]),
                    ),
                ],
            ),
            (
                "match 1 { Int @x => 2 }",
                vec![
                    decl("var_0", "int"),
                    IrInstruction::VarAssignment("var_0".to_string(), IrValue::Int(1)),
                    decl("var_1", "int"),
                    IrInstruction::InlineGo(
                        "\nvar var_2 any\n_ = var_2\nvar_2 = var_0".to_string(),
                    ),
                    IrInstruction::SwitchType(
                        IrValue::Var("var_2".to_string()),
                        vec![Case {
                            type_name: "int".to_string(),
                            instrs: vec![IrInstruction::VarAssignment(
                                "var_1".to_string(),
                                IrValue::Int(2),
                            )],
                            identifier_binding: Some("x".to_string()),
                            condition: None,
                            conditional_branches: None,
                            span: empty_range(),
                        }],
                    ),
                ],
            ),
            (
                "match 1 + 1 { Int @x => 100 }",
                vec![
                    decl("var_0", "int"),
                    IrInstruction::Add(
                        "var_0".into(),
                        IrValue::Int(1),
                        IrValue::Int(1),
                        TypeExpr::Int,
                    ),
                    decl("var_1", "int"),
                    IrInstruction::InlineGo(
                        "\nvar var_2 any\n_ = var_2\nvar_2 = var_0".to_string(),
                    ),
                    IrInstruction::SwitchType(
                        IrValue::Var("var_2".into()),
                        vec![Case {
                            type_name: "int".to_string(),
                            instrs: vec![IrInstruction::VarAssignment(
                                "var_1".to_string(),
                                IrValue::Int(100),
                            )],
                            identifier_binding: Some("x".to_string()),
                            condition: None,
                            conditional_branches: None,
                            span: empty_range(),
                        }],
                    ),
                ],
            ),
        ];

        for (src, exp) in test_cases {
            let lexed = lex_parser("test", src).parse(src).unwrap();

            let mut parsed = value_expr_parser(make_input)
                .parse(make_input(empty_range(), &lexed))
                .into_result()
                .expect(&src);

            value_expr_into_empty_range(&mut parsed);

            let ir = parsed
                .0
                .emit(&mut TypeEnv::default(), &mut ToIr::default(), empty_range());
            assert_eq!(exp, ir.0, "{src}");
        }
    }
}
