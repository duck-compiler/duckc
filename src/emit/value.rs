use std::{
    collections::{HashMap, HashSet, VecDeque},
    panic,
};

use crate::{
    emit::types::escape_string_for_go,
    parse::{
        SS, Spanned,
        duckx_component_parser::find_client_components,
        failure, failure_with_occurence,
        function_parser::LambdaFunctionExpr,
        type_parser::{Duck, TypeExpr},
        value_parser::{Declaration, ValFmtStringContents, ValHtmlStringContents, ValueExpr},
    },
    semantics::{ident_mangler::mangle, type_resolve::TypeEnv},
};

#[derive(Debug, Clone, Default)]
pub struct ToIr {
    pub var_counter: usize,
}

/// Expression further down should use this
/// if they want the result
type IrRes = String;

type Identifier = String;
type Param = (String, String);
type ReturnType = Option<String>;

#[derive(Debug, Clone, PartialEq)]
pub enum IrInstruction {
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
    Break,
    Continue,
    Return(Option<IrValue>),
    InlineGo(String),
    If(
        IrValue,                    // bool_value
        Vec<IrInstruction>,         // body
        Option<Vec<IrInstruction>>, // else
    ),
    Loop(Vec<IrInstruction>),
    Block(Vec<IrInstruction>),

    // Top-Level Statements
    GoPackage(String),
    GoImports(Vec<(Option<String>, String)>),
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
    Int(i64),
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
    MethodCall(Box<IrValue>, String, Vec<IrValue>),
    ArrayAccess(Box<IrValue>, Box<IrValue>),
    Imm(String),
    Pointer(Box<IrValue>),
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
            .as_go_type_annotation(&mut TypeEnv::default()),
            vec![],
        )
    }
}

impl ToIr {
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
        } => {
            if let ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } = &target.0
            {
                let ty = TypeExpr::from_value_expr_resolved_type_name_dereferenced(
                    &target_obj,
                    type_env,
                );
                if let TypeExpr::TypeName(_, type_name, _) = ty {
                    if let Some(struct_def) = type_env.get_struct_def_opt(&type_name) {
                        if struct_def.mut_methods.contains(&field_name.to_string()) {
                            return true;
                        }
                    }
                }
            }
            false
        }
        _ => false,
    }
}

pub fn can_do_mut_stuff_through(v: &Spanned<ValueExpr>, type_env: &mut TypeEnv) -> bool {
    let mut ty = TypeExpr::from_value_expr_resolved_type_name(v, type_env);

    if matches!(ty, TypeExpr::RefMut(..)) {
        while let TypeExpr::RefMut(to) = ty {
            if matches!(&to.0, TypeExpr::Ref(..)) {
                return false;
            }
            ty = to.0;
        }
        true
    } else if matches!(ty, TypeExpr::Ref(..)) {
        false
    } else if let ValueExpr::ArrayAccess(target_obj, _) = &v.0 {
        can_do_mut_stuff_through(target_obj, type_env)
    } else if let ValueExpr::FieldAccess {
        target_obj,
        field_name: _,
    } = &v.0
    {
        can_do_mut_stuff_through(target_obj, type_env)
    } else {
        !matches!(&v.0, ValueExpr::Variable(_, _, _, Some(true)))
    }
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
    let mut res_instr = VecDeque::new();
    let mut current_obj = obj.clone();
    let mut s = VecDeque::new();

    let mut derefs = Vec::new();
    let mut stars = 0;

    let mut is_calling_fun = false;

    loop {
        let cloned = is_calling_fun;
        is_calling_fun = false;
        match current_obj.0 {
            ValueExpr::Variable(_, name, _type_expr, is_const) => {
                s.push_front(name.clone());

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
                    derefs.push(stars);
                    s[0].push(')');
                }
                break;
            }
            ValueExpr::ArrayAccess(target_obj, index) => {
                let (instr, res) = index.0.emit(type_env, env, span);

                let mut type_expr =
                    TypeExpr::from_value_expr_resolved_type_name(&target_obj, type_env);

                let mut stars_to_set = 0;
                if deref_needs_to_be_mut {
                    if !can_do_mut_stuff_through(&target_obj, type_env) {
                        failure_with_occurence(
                            "This needs to allow mutable access".to_string(),
                            target_obj.1,
                            [(
                                "This needs to allow mutable access".to_string(),
                                target_obj.1,
                            )],
                        );
                    }

                    while let TypeExpr::RefMut(v) = type_expr {
                        type_expr = v.0;
                        stars_to_set += 1;
                    }

                    if let TypeExpr::Ref(_) = type_expr {
                        panic!(
                            "need only mut refs for mut stuff {}..{}",
                            span.start, span.end
                        );
                    }
                } else {
                    while let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = type_expr {
                        type_expr = v.0;
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
                    derefs.push(stars);
                    s[0].push(')');
                }

                stars = stars_to_set;

                current_obj = *target_obj;
            }
            ValueExpr::FunctionCall {
                target,
                params,
                type_params: _,
                ..
            } => {
                is_calling_fun = true;
                let mut param_res = Vec::new();

                if let ValueExpr::FieldAccess {
                    target_obj,
                    field_name,
                } = &target.0
                {
                    let ty = TypeExpr::from_value_expr_resolved_type_name_dereferenced(
                        target_obj, type_env,
                    );

                    if let TypeExpr::Struct(struct_name) = ty {
                        let struct_def = type_env.get_struct_def(struct_name.as_str());
                        if struct_def.mut_methods.contains(field_name)
                            && !can_do_mut_stuff_through(target_obj, type_env)
                        {
                            failure_with_occurence(
                                "This needs to allow mutable access".to_string(),
                                target.1,
                                [(
                                    "This needs to allow mutable access".to_string(),
                                    target_obj.1,
                                )],
                            );
                        }
                    } else if let TypeExpr::Duck(duck) = ty {
                        if let Some(duck_field) = duck
                            .fields
                            .iter()
                            .find(|field| field.name.as_str() == field_name.as_str())
                        {
                            if let TypeExpr::Fun(_, _, true) = duck_field.type_expr.0
                                && !can_do_mut_stuff_through(target_obj, type_env)
                            {
                                failure_with_occurence(
                                    "This needs to allow mutable access".to_string(),
                                    target.1,
                                    [(
                                        "This needs to allow mutable access".to_string(),
                                        target_obj.1,
                                    )],
                                );
                            }
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

                        param_res.push(res);
                    } else {
                        return (res_instr.into(), None);
                    }
                }

                let mut type_expr = TypeExpr::from_value_expr_resolved_type_name(&target, type_env);

                let mut stars_to_set = 0;
                if deref_needs_to_be_mut {
                    while let TypeExpr::RefMut(v) = type_expr {
                        type_expr = v.0;
                        stars_to_set += 1;
                    }
                    if let TypeExpr::Ref(_) = type_expr {
                        panic!(
                            "need only mut refs for mut stuff {}..{}",
                            span.start, span.end
                        );
                    }
                } else {
                    while let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = type_expr {
                        type_expr = v.0;
                        stars_to_set += 1;
                    }
                }

                s.push_front(format!("({})", param_res.join(", ")));

                if stars > 0 {
                    derefs.push(stars);
                    s[0].push(')');
                }

                stars = stars_to_set;
                current_obj = *target;
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let mut type_expr =
                    TypeExpr::from_value_expr_resolved_type_name(&target_obj, type_env);

                let mut stars_to_set = 0;
                if deref_needs_to_be_mut {
                    if !can_do_mut_stuff_through(&target_obj, type_env) {
                        failure_with_occurence(
                            "This needs to allow mutable access".to_string(),
                            target_obj.1,
                            [(
                                "This needs to allow mutable access".to_string(),
                                target_obj.1,
                            )],
                        );
                    }

                    while let TypeExpr::RefMut(v) = type_expr {
                        type_expr = v.0;
                        stars_to_set += 1;
                    }
                    if let TypeExpr::Ref(_) = type_expr {
                        panic!(
                            "need only mut refs for mut stuff {}..{}",
                            span.start, span.end
                        );
                    }
                } else {
                    while let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = type_expr {
                        type_expr = v.0;
                        stars_to_set += 1;
                    }
                }

                let type_expr = TypeExpr::from_value_expr_resolved_type_name_dereferenced(
                    &target_obj,
                    type_env,
                );

                match type_expr {
                    TypeExpr::Tuple(..) => s.push_front(format!("field_{field_name}")),
                    TypeExpr::Duck(Duck { fields }) => {
                        let found_field = fields
                            .iter()
                            .find(|x| x.name == field_name)
                            .expect("Field doesn't exist");
                        if found_field.type_expr.0.is_array()
                            || found_field.type_expr.0.is_duck()
                            || found_field.type_expr.0.is_struct()
                            || found_field.type_expr.0.is_fun()
                            || only_read
                        {
                            s.push_front(format!("Get{field_name}()"));
                        } else {
                            s.push_front(format!("GetPtr{field_name}()"));
                        }
                    }
                    TypeExpr::Struct(..) => s.push_front(field_name.to_string()),
                    _ => {}
                }

                s[0].insert(0, '.');

                if stars > 0 {
                    derefs.push(stars);
                    s[0].push(')');
                }

                stars = stars_to_set;

                current_obj = *target_obj;
            }
            _ => {
                // if only_read {
                let mut type_expr =
                    TypeExpr::from_value_expr_resolved_type_name(&current_obj, type_env);

                if deref_needs_to_be_mut {
                    if !can_do_mut_stuff_through(&current_obj, type_env) {
                        failure_with_occurence(
                            "This needs to allow mutable access".to_string(),
                            current_obj.1,
                            [(
                                "This needs to allow mutable access".to_string(),
                                current_obj.1,
                            )],
                        );
                    }
                    while let TypeExpr::RefMut(v) = type_expr {
                        type_expr = v.0;
                    }
                    if let TypeExpr::Ref(_) = type_expr {
                        panic!(
                            "need only mut refs for mut stuff {}..{} {}",
                            span.start,
                            span.end,
                            &span.context.file_contents[span.end - 20..],
                        );
                    }
                } else {
                    while let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = type_expr {
                        type_expr = v.0;
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
                    derefs.push(stars);
                    s[0].push(')');
                }
                break;
                // } else {
                //     panic!("need var, got {current_obj:?}")
                // }
            }
        }
    }

    for deref in derefs.iter().rev() {
        for _ in 0..*deref {
            s[0].insert(0, '*');
        }
        s[0].insert(0, '(');
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

impl ValueExpr {
    pub fn direct_emit(&self, type_env: &mut TypeEnv, env: &mut ToIr, span: SS) -> Option<IrValue> {
        match self {
            ValueExpr::Bool(b) => Some(IrValue::Bool(*b)),
            ValueExpr::Char(c) => Some(IrValue::Char(*c)),
            ValueExpr::Int(i) => Some(IrValue::Int(*i)),
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
                    rparams.push((p.0.clone(), p.1.0.as_go_type_annotation(type_env)));
                }

                let return_type = return_type
                    .as_ref()
                    .map(|(x, _)| x.as_go_type_annotation(type_env));

                let (mut b_instr, b_res) = value_expr.0.emit(type_env, env, span);
                if return_type.is_some()
                    && let Some(b_res) = b_res
                {
                    b_instr.push(IrInstruction::Return(b_res.into()));
                }
                Some(IrValue::Lambda(rparams, return_type, b_instr))
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
            ValueExpr::ExtensionAccess { target_obj, extension_name } => {
                let target_type = TypeExpr::from_value_expr(target_obj, type_env);

                let extension_fn_name = target_type.build_extension_access_function_name(extension_name, type_env);
                if !type_env.extension_functions.contains_key(&extension_fn_name) {
                    panic!("doesn't have extension fuuun")
                }

                let extension_fn_type = type_env.extension_functions.get(&extension_fn_name)
                    .expect("we've just checked that it exists")
                    .clone();

                let (mut emit_instr, emit_res) = target_obj.as_ref().0.emit(type_env, env, span);
                if let Some(emit_res) = emit_res {
                    let var_name = env.new_var();
                    emit_instr.push(IrInstruction::VarDecl(var_name.clone(), extension_fn_type.0.as_go_type_annotation(type_env)));
                    return (
                        emit_instr
                            .iter()
                            .chain(vec![
                                IrInstruction::FunCall(
                                    Some(var_name.clone()),
                                    IrValue::Imm(extension_fn_name.to_string()),
                                    vec![emit_res]
                                )
                            ].iter())
                            .map(|i| i.clone())
                            .collect::<Vec<_>>(),
                        as_rvar(var_name)
                    )
                } else {
                    return (
                        vec![],
                        None
                    )
                }
            }
            ValueExpr::Deref(v) => {
                let target_type =
                    TypeExpr::from_value_expr_resolved_type_name(&(self.clone(), span), type_env);
                let res_type = target_type.as_go_type_annotation(type_env).to_string();
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
                let t = TypeExpr::from_value_expr_resolved_type_name(v, type_env);
                let ptr_type = format!("*{}", t.as_go_type_annotation(type_env));

                let need_mut = matches!(self, ValueExpr::RefMut(..));

                #[allow(clippy::never_loop)]
                loop {
                    let (mut walk_instr, walk_res) =
                        walk_access_raw(v, type_env, env, span, false, false, need_mut);

                    if let ValueExpr::FieldAccess {
                        target_obj,
                        field_name: _,
                    }
                    | ValueExpr::ArrayAccess(target_obj, _) = &v.0
                    {
                        let t = TypeExpr::from_value_expr_resolved_type_name_dereferenced(
                            target_obj, type_env,
                        );
                        let is_accessing_duck = matches!(t, TypeExpr::Duck(..));
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
                    }

                    if let Some(emit_res) = walk_res {
                        let var_name = env.new_var();
                        let ptr_var_decl = [
                            IrInstruction::VarDecl(var_name.clone(), ptr_type),
                            IrInstruction::VarAssignment(
                                var_name.clone(),
                                IrValue::Pointer(IrValue::Imm(emit_res.join(".")).into()),
                            ),
                        ];
                        walk_instr.extend(ptr_var_decl);
                        break (walk_instr, Some(IrValue::Var(var_name)));
                    } else {
                        break (walk_instr, None);
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

                let type_expr = TypeExpr::from_value_expr(lhs, type_env).unconst();

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

                let type_expr = TypeExpr::from_value_expr(lhs, type_env).unconst();

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

                let type_expr = TypeExpr::from_value_expr(lhs, type_env).unconst();

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
                                                            Some(
                                                                TypeExpr::Html
                                                                    .into_empty_span()
                                                                    .into(),
                                                            ),
                                                            false,
                                                        )),
                                                        Some(false),
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
                                                                            Some(b.1),
                                                                            Some(false),
                                                                        )
                                                                        .into_empty_span(),
                                                                    )
                                                                })
                                                                .collect(),
                                                        )
                                                        .into_empty_span(),
                                                    ],
                                                    type_params: None,
                                                    is_extension_call: false,
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
                            match ty {
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
                                TypeExpr::Int(..) => {
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
                                    src.name, src.typescript_source.0
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
                let (mut instructions, match_on_res) =
                    value_expr.0.direct_or_with_instr(type_env, env, *span);
                let match_on_value = match match_on_res {
                    Some(v) => v,
                    None => return (instructions, None),
                };

                let result_type =
                    TypeExpr::from_value_expr(&(self.clone(), value_expr.1), type_env);
                let result_type_annotation = result_type.as_go_type_annotation(type_env);

                let result_var_name = env.new_var();
                if !result_type.is_unit() {
                    instructions.push(IrInstruction::VarDecl(
                        result_var_name.clone(),
                        result_type_annotation,
                    ));
                }

                let mut cases = Vec::new();
                for arm in arms {
                    let type_name = arm.type_case.0.as_go_type_annotation(type_env);

                    let (mut arm_instrs, arm_res) =
                        arm.value_expr.0.direct_or_with_instr(type_env, env, *span);
                    if !result_type.is_unit()
                        && let Some(res) = arm_res
                    {
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
                    if !result_type.is_unit()
                        && let Some(res) = arm_res
                    {
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
                            "Unexhaustive Match".to_string(),
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

                instructions.push(IrInstruction::SwitchType(match_on_value, cases));
                (
                    instructions,
                    if result_type.is_unit() {
                        None
                    } else {
                        as_rvar(result_var_name)
                    },
                )
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
                        .as_go_type_annotation(type_env);
                let res_var_name = env.new_var();

                res_instr.push(IrInstruction::VarDecl(res_var_name.clone(), res_type));
                res_instr.push(IrInstruction::VarAssignment(
                    res_var_name.clone(),
                    IrValue::ArrayAccess(target_res.unwrap().into(), idx_res.unwrap().into()),
                ));

                (res_instr, Some(IrValue::Var(res_var_name)))
            }
            ValueExpr::Array(_, exprs) => {
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

                let arr_type = TypeExpr::from_value_expr(&(self.clone(), span), type_env);

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

                (v, Some(IrValue::empty_tuple()))
            }
            ValueExpr::InlineGo(s) => (vec![IrInstruction::InlineGo(s.clone())], None),
            ValueExpr::While { condition, body } => {
                let (mut cond_instr, cond_res) =
                    condition.0.direct_or_with_instr(type_env, env, span);
                if cond_res.is_none() {
                    return (cond_instr, None);
                }

                let (body, _) = body.0.direct_or_with_instr(type_env, env, span);
                let cond_res = cond_res.unwrap();
                cond_instr.push(IrInstruction::If(
                    cond_res,
                    body,
                    Some(vec![IrInstruction::Break]),
                ));

                (vec![IrInstruction::Loop(cond_instr)], None)
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
                if !res_type.is_unit() {
                    i.push(IrInstruction::VarDecl(
                        res_var_name.clone(),
                        res_type.as_go_type_annotation(type_env),
                    ));
                }

                let (mut then_instr, then_res) = then.0.direct_or_with_instr(type_env, env, span);
                if !res_type.is_unit()
                    && let Some(then_res) = then_res
                {
                    then_instr.push(IrInstruction::VarAssignment(res_var_name.clone(), then_res));
                }

                let r#else = r#else
                    .clone()
                    .map(|x| x.0.direct_or_with_instr(type_env, env, span))
                    .map(|mut x| {
                        if !res_type.is_unit()
                            && let Some(o) = x.1
                        {
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

                        let target_ty = TypeExpr::from_value_expr_resolved_type_name_dereferenced(
                            target_obj, type_env,
                        );
                        match target_ty {
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
                            TypeExpr::Struct(_) => {
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

                            if !matches!(target_type, TypeExpr::RefMut(..)) {
                                if matches!(target_type, TypeExpr::Ref(..)) {
                                    failure_with_occurence(
                                        "Can only dereference a mutable reference".to_string(),
                                        span,
                                        vec![(
                                            "This is not a mutable reference".to_string(),
                                            new_target.1,
                                        )],
                                    );
                                }
                                failure_with_occurence(
                                    "Can only dereference a reference".to_string(),
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

                let type_expr = TypeExpr::from_value_expr(v1, type_env).unconst();

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

                let type_expr = TypeExpr::from_value_expr(v1, type_env).unconst();

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

                for (i, (block_expr, _)) in block_exprs.iter().enumerate() {
                    if i == block_exprs.len() - 1
                        && let ValueExpr::Tuple(t) = block_expr
                        && t.is_empty()
                    {
                        res_var = None;
                        continue;
                    }
                    let (block_instr, block_res) =
                        block_expr.direct_or_with_instr(type_env, env, span);

                    for current in block_instr.iter() {
                        res_instr.push(current.clone());
                        if matches!(
                            current,
                            IrInstruction::Return(..)
                                | IrInstruction::Break
                                | IrInstruction::Continue
                        ) {
                            res_var = None;
                            return (res_instr, res_var);
                        }
                    }

                    res_var = block_res;
                }

                let mut final_instr = Vec::new();
                let self_return_type = TypeExpr::from_value_expr(&(self.clone(), span), type_env);
                if !self_return_type.is_unit() {
                    let fresvar = env.new_var();

                    res_var = res_var.or(Some(IrValue::Tuple("Tup_".into(), vec![])));
                    final_instr.push(IrInstruction::VarDecl(
                        fresvar.clone(),
                        self_return_type.as_go_type_annotation(type_env),
                    ));
                    res_instr.push(IrInstruction::VarAssignment(
                        fresvar.clone(),
                        res_var.unwrap(),
                    ));
                    res_var = Some(IrValue::Var(fresvar))
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
            ValueExpr::Break => (vec![IrInstruction::Break], None),
            ValueExpr::Continue => (vec![IrInstruction::Continue], None),
            ValueExpr::Return(expr) => {
                if let Some(expr) = expr {
                    let (expr, _) = &**expr;
                    let (mut instr, res) = expr.direct_or_with_instr(type_env, env, span);
                    instr.push(IrInstruction::Return(res));
                    (instr, None)
                } else {
                    (vec![IrInstruction::Return(None)], None)
                }
            }
            ValueExpr::FunctionCall {
                target: v_target,
                params,
                type_params: _,
                ..
            } => {
                // todo: type_params

                let TypeExpr::Fun(_, return_type, _) =
                    TypeExpr::from_value_expr(v_target, type_env)
                else {
                    panic!("can only call function")
                };

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
                    if let Some(return_type) = return_type {
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

                    if let Some(return_type) = return_type {
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
            ValueExpr::RawVariable(_, p) => (vec![], as_rvar(mangle(p))),
            ValueExpr::Variable(_, x, _, _) => (vec![], as_rvar(x.to_owned())),
            ValueExpr::Equals(v1, v2) => {
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

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::Equals(
                        var.clone(),
                        v1_res.unwrap(),
                        v2_res.unwrap(),
                        TypeExpr::from_value_expr(v1, type_env).unconst(),
                    ),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::NotEquals(lhs, rhs) => {
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

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::NotEquals(
                        var.clone(),
                        v1_res.unwrap(),
                        v2_res.unwrap(),
                        TypeExpr::from_value_expr(lhs, type_env).unconst(),
                    ),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::LessThan(lhs, rhs) => {
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

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::LessThan(
                        var.clone(),
                        v1_res.unwrap(),
                        v2_res.unwrap(),
                        TypeExpr::from_value_expr(lhs, type_env).unconst(),
                    ),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::LessThanOrEquals(lhs, rhs) => {
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

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::LessThanOrEquals(
                        var.clone(),
                        v1_res.unwrap(),
                        v2_res.unwrap(),
                        TypeExpr::from_value_expr(lhs, type_env).unconst(),
                    ),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::GreaterThan(lhs, rhs) => {
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

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::GreaterThan(
                        var.clone(),
                        v1_res.unwrap(),
                        v2_res.unwrap(),
                        TypeExpr::from_value_expr(lhs, type_env).unconst(),
                    ),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::GreaterThanOrEquals(lhs, rhs) => {
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

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::GreaterThanOrEquals(
                        var.clone(),
                        v1_res.unwrap(),
                        v2_res.unwrap(),
                        TypeExpr::from_value_expr(lhs, type_env).unconst(),
                    ),
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
                            .as_go_type_annotation(type_env),
                    ),
                    IrInstruction::VarAssignment(res_var.clone(), IrValue::Duck(name, res_vars)),
                ]);

                (res, as_rvar(res_var))
            }
            ValueExpr::Struct { fields, .. } => {
                let name = TypeExpr::from_value_expr(&(self.clone(), span), type_env)
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
                    IrInstruction::VarDecl(res_var.clone(), format!("*{name}")),
                    IrInstruction::VarAssignment(res_var.clone(), IrValue::Struct(name, res_vars)),
                ]);

                (res, as_rvar(res_var))
            }
            ValueExpr::Tag(..) => {
                let type_name = TypeExpr::from_value_expr(&(self.clone(), span), type_env)
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
                    let mut instr = Vec::new();
                    if !ty.is_unit() {
                        instr.push(IrInstruction::VarDecl(
                            res_var.clone(),
                            ty.as_go_type_annotation(type_env),
                        ));
                        instr.push(IrInstruction::VarAssignment(res_var.clone(), d))
                    }
                    (
                        instr,
                        if ty.is_unit() {
                            None
                        } else {
                            Some(IrValue::Var(res_var))
                        },
                    )
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
                        TypeExpr::Int(None),
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
                        TypeExpr::Int(None),
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
            ("{1;}", vec![]),
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
            ("(true, break, 2)", vec![IrInstruction::Break]),
            ("(true, return, 2)", vec![IrInstruction::Return(None)]),
            ("(true, continue, 3)", vec![IrInstruction::Continue]),
            (
                "1 == 2",
                vec![
                    decl("var_0", "bool"),
                    IrInstruction::Equals(
                        "var_0".into(),
                        IrValue::Int(1),
                        IrValue::Int(2),
                        TypeExpr::Int(None),
                    ),
                ],
            ),
            (
                ".{}[]",
                vec![
                    IrInstruction::VarDecl("var_0".into(), "[]interface{}".into()),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Array("[]interface{}".into(), vec![]),
                    ),
                ],
            ),
            (
                ".Int[][.Int[]]",
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
                    IrInstruction::SwitchType(
                        IrValue::Int(1),
                        vec![Case {
                            type_name: "int".to_string(),
                            instrs: vec![IrInstruction::VarAssignment(
                                "var_0".to_string(),
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
                        TypeExpr::Int(None),
                    ),
                    decl("var_1", "int"),
                    IrInstruction::SwitchType(
                        IrValue::Var("var_0".into()),
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
                .unwrap();

            value_expr_into_empty_range(&mut parsed);

            let ir = parsed
                .0
                .emit(&mut TypeEnv::default(), &mut ToIr::default(), empty_range());
            assert_eq!(exp, ir.0, "{src}");
        }
    }
}
