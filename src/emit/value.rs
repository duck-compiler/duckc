use std::collections::VecDeque;

use crate::{
    parse::{
        function_parser::LambdaFunctionExpr,
        struct_parser::StructDefinition,
        type_parser::{Duck, TypeExpr},
        value_parser::{Declaration, ValFmtStringContents, ValueExpr},
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
    FunCall(Option<IrRes>, IrValue, Vec<IrValue>),
    StringConcat(IrRes, Vec<IrValue>),
    Add(IrRes, IrValue, IrValue, TypeExpr),
    Mul(IrRes, IrValue, IrValue, TypeExpr),
    Equals(IrRes, IrValue, IrValue, TypeExpr),
    Break,
    Continue,
    Return(Option<IrValue>),
    InlineGo(String),
    If(IrValue, Vec<IrInstruction>, Option<Vec<IrInstruction>>),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub type_name: String,
    pub instrs: Vec<IrInstruction>,
    pub identifier_binding: Option<String>,
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
    Nil,
}

impl IrValue {
    pub fn empty_tuple() -> Self {
        Self::Tuple(
            TypeExpr::from_value_expr(&ValueExpr::Tuple(vec![]), &mut TypeEnv::default())
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

fn walk_access(
    obj: ValueExpr,
    type_env: &mut TypeEnv,
    env: &mut ToIr,
) -> (Vec<IrInstruction>, Option<String>) {
    let mut res_instr = VecDeque::new();
    let mut current_obj = obj;
    let mut s = VecDeque::new();

    loop {
        match current_obj {
            ValueExpr::Variable(_, name, _) => {
                s.push_front(name.clone());
                break;
            }
            ValueExpr::ArrayAccess(next_obj, index) => {
                let (instr, res) = index.0.emit(type_env, env);

                instr
                    .into_iter()
                    .rev()
                    .for_each(|x| res_instr.push_front(x));
                if let Some(res) = res {
                    let IrValue::Var(res) = res else {
                        panic!("need var");
                    };

                    s.push_front(format!("[{res}.as_dgo_int()]"));
                } else {
                    return (res_instr.into(), None);
                }
                current_obj = next_obj.0;
            }
            ValueExpr::FunctionCall { .. } => {
                let (i, r) = current_obj.emit(type_env, env);
                i.into_iter().rev().for_each(|x| res_instr.push_front(x));
                if let Some(res) = r {
                    let IrValue::Var(res) = res else {
                        panic!("need var");
                    };

                    s.push_front(res.to_string());
                } else {
                    return (res_instr.into(), None);
                }
                break;
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let type_expr =
                    TypeExpr::from_value_expr_resolved_type_name(&target_obj.0, type_env);
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
                        {
                            s.push_front(format!("Get{field_name}()"));
                        } else {
                            s.push_front(format!("GetPtr{field_name}()"));
                        }
                    }
                    TypeExpr::Struct(..) => s.push_front(field_name.to_string()),
                    a => panic!("can only access object like {a:?} {:?}", target_obj.0),
                }
                current_obj = target_obj.0;
            }
            _ => panic!("need var, got {current_obj:?}"),
        }
    }
    (
        res_instr.into(),
        Some(Into::<Vec<String>>::into(s).join(".")),
    )
}

impl ValueExpr {
    pub fn direct_emit(&self, type_env: &mut TypeEnv, env: &mut ToIr) -> Option<IrValue> {
        match self {
            ValueExpr::Bool(b) => Some(IrValue::Bool(*b)),
            ValueExpr::Char(c) => Some(IrValue::Char(*c)),
            ValueExpr::Int(i) => Some(IrValue::Int(*i)),
            ValueExpr::Float(f) => Some(IrValue::Float(*f)),
            ValueExpr::String(s, is_const) => Some(IrValue::String(s.clone(), *is_const)),
            ValueExpr::Lambda(b) => {
                let LambdaFunctionExpr {
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

                let (mut b_instr, b_res) = value_expr.0.emit(type_env, env);
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
    ) -> (Vec<IrInstruction>, Option<IrValue>) {
        if let Some(ir_value) = self.direct_emit(type_env, env) {
            (Vec::new(), Some(ir_value))
        } else {
            let (instr, res) = self.emit(type_env, env);
            (instr, res)
        }
    }

    pub fn emit(
        &self,
        type_env: &mut TypeEnv,
        env: &mut ToIr,
    ) -> (Vec<IrInstruction>, Option<IrValue>) {
        match self {
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
                                expr.0.direct_or_with_instr(type_env, env);
                            instr.extend(param_instr);
                            if param_res.is_none() {
                                return (instr, None);
                            }

                            concat_params.push(param_res.unwrap());
                        }
                    }
                }

                let res_name = env.new_var();

                instr.push(IrInstruction::VarDecl(
                    res_name.clone(),
                    "DuckString".into(),
                ));
                instr.push(IrInstruction::StringConcat(res_name.clone(), concat_params));

                (instr, Some(IrValue::Var(res_name)))
            }
            ValueExpr::Match {
                value_expr,
                arms,
                else_arm,
            } => {
                let (mut instructions, match_on_res) =
                    value_expr.0.direct_or_with_instr(type_env, env);
                let match_on_value = match match_on_res {
                    Some(v) => v,
                    None => return (instructions, None),
                };

                let result_type = TypeExpr::from_value_expr(self, type_env);
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
                    let type_name = arm.type_case.0.as_go_concrete_annotation(type_env);

                    let (mut arm_instrs, arm_res) =
                        arm.value_expr.0.direct_or_with_instr(type_env, env);
                    if !result_type.is_unit()
                        && let Some(res) = arm_res
                    {
                        arm_instrs.push(IrInstruction::VarAssignment(result_var_name.clone(), res));
                    }

                    cases.push(Case {
                        type_name,
                        instrs: arm_instrs,
                        identifier_binding: arm.identifier_binding.clone(),
                    });
                }

                if let Some(arm) = else_arm {
                    let _type_name = arm.type_case.0.as_clean_go_type_name(type_env);

                    let (mut arm_instrs, arm_res) =
                        arm.value_expr.0.direct_or_with_instr(type_env, env);
                    if !result_type.is_unit()
                        && let Some(res) = arm_res
                    {
                        arm_instrs.push(IrInstruction::VarAssignment(result_var_name.clone(), res));
                    }

                    cases.push(Case {
                        type_name: "__else".to_string(),
                        instrs: arm_instrs,
                        identifier_binding: arm.identifier_binding.clone(),
                    });
                }

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
                let (target_instr, target_res) = target.0.direct_or_with_instr(type_env, env);

                if target_res.is_none() {
                    return (target_instr, None);
                }

                let (idx_instr, idx_res) = idx.0.direct_or_with_instr(type_env, env);

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
                    TypeExpr::from_value_expr(self, type_env).as_go_type_annotation(type_env);
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
                    let (expr_instr, expr_res) = expr.0.direct_or_with_instr(type_env, env);
                    total_instr.extend(expr_instr);
                    if let Some(expr_res) = expr_res {
                        array_contents.push(expr_res);
                    } else {
                        return (total_instr, None);
                    }
                }

                let arr_type = TypeExpr::from_value_expr(self, type_env);

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
                } = &b.0;

                let type_expression = type_expr
                    .as_ref()
                    .expect("compiler error: i expect that the type should be replaced by now")
                    .0
                    .as_go_type_annotation(type_env);

                let mut v = Vec::new();
                v.push(IrInstruction::VarDecl(name.clone(), type_expression));
                let (init_r, inti_r_res) = initializer.0.direct_or_with_instr(type_env, env);
                v.extend(init_r);
                if let Some(init_r_res) = inti_r_res {
                    v.push(IrInstruction::VarAssignment(name.clone(), init_r_res));
                }
                (v, Some(IrValue::empty_tuple()))
            }
            ValueExpr::InlineGo(s) => (vec![IrInstruction::InlineGo(s.clone())], None),
            ValueExpr::While { condition, body } => {
                let (mut cond_instr, cond_res) = condition.0.direct_or_with_instr(type_env, env);
                if cond_res.is_none() {
                    return (cond_instr, None);
                }

                let (body, _) = body.0.direct_or_with_instr(type_env, env);
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
                let res_type = TypeExpr::from_value_expr(self, type_env);
                let (mut i, cond_res) = condition.0.direct_or_with_instr(type_env, env);
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

                let (mut then_instr, then_res) = then.0.direct_or_with_instr(type_env, env);
                if !res_type.is_unit()
                    && let Some(then_res) = then_res
                {
                    then_instr.push(IrInstruction::VarAssignment(res_var_name.clone(), then_res));
                }

                let r#else = r#else
                    .clone()
                    .map(|x| x.0.direct_or_with_instr(type_env, env))
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
                let (i, res) = assign.value_expr.0.direct_or_with_instr(type_env, env);
                if let Some(a_res) = res {
                    let target = &assign.target.0;
                    let mut res = Vec::new();

                    res.extend(i);

                    if let ValueExpr::FieldAccess {
                        target_obj,
                        field_name,
                    } = target
                    {
                        let (walk_instr, walk_res) =
                            walk_access(target_obj.0.clone(), type_env, env);
                        res.extend(walk_instr);
                        let target_res = match walk_res {
                            Some(s) => s,
                            None => return (res, None),
                        };

                        let target_ty = TypeExpr::from_value_expr(&target_obj.0, type_env);
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
                    } else if let ValueExpr::ArrayAccess(target, idx) = target {
                        //todo(@Apfelfrosch) handle indices of type ! properly (do it in rest of emit too)
                        let (idx_instr, Some(IrValue::Var(idx_res))) = idx.0.emit(type_env, env)
                        else {
                            panic!("no var: {idx:?}")
                        };

                        res.extend(idx_instr);

                        let (walk_instr, walk_res) = walk_access(target.0.clone(), type_env, env);
                        res.extend(walk_instr);
                        let target_res = match walk_res {
                            Some(s) => s,
                            None => return (res, None),
                        };

                        res.push(IrInstruction::VarAssignment(
                            format!("{target_res}[{idx_res}.as_dgo_int()]"),
                            a_res,
                        ));
                    } else {
                        let (target_instr, Some(IrValue::Var(target_res))) =
                            target.emit(type_env, env)
                        else {
                            panic!("can only assign var")
                        };

                        res.extend(target_instr);
                        res.push(IrInstruction::VarAssignment(target_res, a_res));
                    }

                    (res, Some(IrValue::empty_tuple()))
                } else {
                    (i, Some(IrValue::empty_tuple()))
                }
            }
            ValueExpr::Add(v1, v2) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = v1.0.direct_or_with_instr(type_env, env);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }
                let (v2_instr, v2_res) = v2.0.direct_or_with_instr(type_env, env);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let type_expr = TypeExpr::from_value_expr(&v1.0, type_env);

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

                let (v1_instr, v1_res) = v1.0.direct_or_with_instr(type_env, env);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }
                let (v2_instr, v2_res) = v2.0.direct_or_with_instr(type_env, env);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let type_expr = TypeExpr::from_value_expr(&v1.0, type_env);

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
                    let (block_instr, block_res) = block_expr.direct_or_with_instr(type_env, env);

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
                let self_return_type = TypeExpr::from_value_expr(self, type_env);
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
                let name =
                    TypeExpr::from_value_expr(self, type_env).as_go_type_annotation(type_env);

                for (field_expr, _) in fields {
                    let (field_instr, field_res) = field_expr.direct_or_with_instr(type_env, env);
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
                let (mut instr, e_res_var) = expr.0.direct_or_with_instr(type_env, env);
                if let Some(e_res_var) = e_res_var {
                    let res = env.new_var();
                    instr.push(IrInstruction::VarDecl(res.clone(), "DuckBool".into()));
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
                    let (mut instr, res) = expr.direct_or_with_instr(type_env, env);
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
            } => {
                // todo: type_params

                let mut res = v_target.0.direct_emit(type_env, env);

                let mut instr = Vec::new();
                if res.is_none() {
                    let (walk_instr, walk_res) = walk_access(v_target.0.clone(), type_env, env);
                    if walk_res.is_none() {
                        return (instr, None);
                    }
                    res = walk_res.map(IrValue::Var);
                    instr.extend(walk_instr);
                }
                let call_target = res.unwrap();

                let mut v_p_res = Vec::new();
                for (param, _) in params {
                    let (p_instr, p_res) = param.direct_or_with_instr(type_env, env);
                    instr.extend(p_instr);
                    if let Some(p_res) = p_res {
                        v_p_res.push(p_res);
                    } else {
                        return (instr, None);
                    }
                }

                let TypeExpr::Fun(_, return_type) =
                    TypeExpr::from_value_expr(&v_target.0, type_env)
                else {
                    panic!("can only call function")
                };

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
            ValueExpr::RawVariable(_, p) => (vec![], as_rvar(mangle(p))),
            ValueExpr::Variable(_, x, _) => (vec![], as_rvar(x.to_owned())),
            ValueExpr::Equals(v1, v2) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = v1.0.direct_or_with_instr(type_env, env);
                ir.extend(v1_instr);
                if v1_res.is_none() {
                    return (ir, None);
                }
                let (v2_instr, v2_res) = v2.0.direct_or_with_instr(type_env, env);
                ir.extend(v2_instr);
                if v2_res.is_none() {
                    return (ir, None);
                }

                let var = env.new_var();
                ir.extend([
                    IrInstruction::VarDecl(var.clone(), "DuckBool".into()),
                    IrInstruction::Equals(
                        var.clone(),
                        v1_res.unwrap(),
                        v2_res.unwrap(),
                        TypeExpr::from_value_expr(&v1.0, type_env),
                    ),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let field_name = field_name.clone();
                let (mut i, t_res) = target_obj.0.emit(type_env, env);
                if let Some(t_res) = t_res {
                    let target_type = TypeExpr::from_value_expr(&target_obj.0, type_env);
                    match target_type {
                        TypeExpr::Duck(Duck { fields }) => {
                            let f = fields.iter().find(|f| f.name == field_name).unwrap();
                            let res = env.new_var();
                            i.push(IrInstruction::VarDecl(
                                res.clone(),
                                f.type_expr.0.as_go_type_annotation(type_env),
                            ));
                            i.push(IrInstruction::FunCall(
                                Some(res.clone()),
                                IrValue::FieldAccess(
                                    Box::new(t_res),
                                    format!("Get{}", field_name.clone()),
                                ),
                                vec![],
                            ));
                            return (i, Some(IrValue::Var(res)));
                        }
                        TypeExpr::Tuple(fields) => {
                            let field_as_idx = field_name
                                .parse::<usize>()
                                .expect("non numeric tuple index");
                            let field_name = format!("field_{field_name}");
                            let res = env.new_var();
                            i.push(IrInstruction::VarDecl(
                                res.clone(),
                                fields[field_as_idx].0.as_go_type_annotation(type_env),
                            ));
                            i.push(IrInstruction::VarAssignment(
                                res.clone(),
                                IrValue::FieldAccess(Box::new(t_res), field_name.clone()),
                            ));
                            return (i, Some(IrValue::Var(res)));
                        }
                        TypeExpr::Struct(struct_name) => {
                            let StructDefinition {
                                name,
                                fields,
                                methods: _,
                                generics: _,
                            } = type_env.get_struct_def(struct_name.as_str());
                            let f = fields
                                .iter()
                                .find(|f| f.name == field_name)
                                .cloned()
                                .unwrap_or_else(|| panic!("{field_name} {name} {fields:?}"));
                            let res = env.new_var();
                            i.push(IrInstruction::VarDecl(
                                res.clone(),
                                f.type_expr.0.as_go_type_annotation(type_env),
                            ));
                            i.push(IrInstruction::VarAssignment(
                                res.clone(),
                                IrValue::FieldAccess(Box::new(t_res), field_name.clone()),
                            ));
                            return (i, Some(IrValue::Var(res)));
                        }
                        _ => panic!("can only access object like {self:?} {target_type:?}"),
                    }
                } else {
                    return (i, None);
                }
            }
            ValueExpr::Duck(fields) => {
                let name =
                    TypeExpr::from_value_expr(self, type_env).as_clean_go_type_name(type_env);

                let mut res = Vec::new();
                let mut res_vars = Vec::new();
                for (field_name, (field_expr, _)) in fields {
                    let (field_instr, field_res) = field_expr.direct_or_with_instr(type_env, env);
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
                        TypeExpr::from_value_expr(self, type_env).as_go_type_annotation(type_env),
                    ),
                    IrInstruction::VarAssignment(res_var.clone(), IrValue::Duck(name, res_vars)),
                ]);

                (res, as_rvar(res_var))
            }
            ValueExpr::Struct { fields, .. } => {
                let name =
                    TypeExpr::from_value_expr(self, type_env).as_clean_go_type_name(type_env);

                let mut res = Vec::new();
                let mut res_vars = Vec::new();
                for (field_name, (field_expr, _)) in fields {
                    let (field_instr, field_res) = field_expr.direct_or_with_instr(type_env, env);
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
                let type_name =
                    TypeExpr::from_value_expr(self, type_env).as_clean_go_type_name(type_env);

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
                if let Some(d) = self.direct_emit(type_env, env) {
                    let ty = TypeExpr::from_value_expr(self, type_env);
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
            value_parser::{empty_range, value_expr_parser},
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
                    decl("var_0", "DuckInt"),
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
                    decl("var_0", "DuckInt"),
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
                    IrInstruction::VarDecl("a".into(), "DuckString".into()),
                    IrInstruction::VarAssignment("a".into(), IrValue::String("A".into(), true)),
                ],
            ),
            ("{1;}", vec![]),
            (
                "!true",
                vec![
                    decl("var_0", "DuckBool"),
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
                    decl("var_0", "DuckBool"),
                    IrInstruction::Equals(
                        "var_0".into(),
                        IrValue::Int(1),
                        IrValue::Int(2),
                        TypeExpr::Int,
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
                    IrInstruction::VarDecl("var_0".into(), "[]DuckInt".into()),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Array("[]DuckInt".into(), vec![]),
                    ),
                    IrInstruction::VarDecl("var_1".into(), "[][]DuckInt".into()),
                    IrInstruction::VarAssignment(
                        "var_1".into(),
                        IrValue::Array("[][]DuckInt".into(), vec![IrValue::Var("var_0".into())]),
                    ),
                ],
            ),
            (
                "[1]",
                vec![
                    IrInstruction::VarDecl("var_0".into(), "[]DuckInt".into()),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Array("[]DuckInt".into(), vec![IrValue::Int(1)]),
                    ),
                ],
            ),
            (
                "{ x: 123 }",
                vec![
                    decl("var_0", "interface {\n   Hasx[DuckInt]\n}"),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Duck(
                            "Duck_x_DuckInt".into(),
                            vec![("x".into(), IrValue::Int(123))],
                        ),
                    ),
                ],
            ),
            (
                "match (1) { Int @x -> 2 }",
                vec![
                    decl("var_0", "DuckInt"),
                    IrInstruction::SwitchType(
                        IrValue::Int(1),
                        vec![Case {
                            type_name: "ConcDuckInt".into(),
                            instrs: vec![IrInstruction::VarAssignment(
                                "var_0".into(),
                                IrValue::Int(2),
                            )],
                            identifier_binding: Some("x".into()),
                        }],
                    ),
                ],
            ),
            (
                "match (1 + 1) { Int @x -> 100 }",
                vec![
                    decl("var_0", "DuckInt"),
                    IrInstruction::Add(
                        "var_0".into(),
                        IrValue::Int(1),
                        IrValue::Int(1),
                        TypeExpr::Int,
                    ),
                    decl("var_1", "DuckInt"),
                    IrInstruction::SwitchType(
                        IrValue::Var("var_0".into()),
                        vec![Case {
                            type_name: "ConcDuckInt".into(),
                            instrs: vec![IrInstruction::VarAssignment(
                                "var_1".into(),
                                IrValue::Int(100),
                            )],
                            identifier_binding: Some("x".into()),
                        }],
                    ),
                ],
            ),
        ];

        for (src, exp) in test_cases {
            let lexed = lex_parser("test", src).parse(src).unwrap();
            let parsed = value_expr_parser(make_input)
                .parse(make_input(empty_range(), &lexed))
                .unwrap()
                .0;
            let ir = parsed.emit(&mut TypeEnv::default(), &mut ToIr::default());
            assert_eq!(exp, ir.0, "{src}");
        }
    }
}
