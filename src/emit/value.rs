use crate::{
    parse::{
        function_parser::LambdaFunctionExpr,
        type_parser::{Duck, Struct, TypeExpr},
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
    pub bound_to_identifier: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
    Array(String, Vec<IrValue>),
    Lambda(Vec<(String, String)>, Option<String>, Vec<IrInstruction>),
    Tuple(String, Vec<IrValue>),
    Duck(String, Vec<(String, IrValue)>),
    Struct(String, Vec<(String, IrValue)>),
    Var(String),
    BoolNegate(Box<IrValue>),
    FieldAccess(Box<IrValue>, String),
    ArrayAccess(Box<IrValue>, Box<IrValue>),
    Imm(String),
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

impl ValueExpr {
    pub fn direct_emit(&self, type_env: &mut TypeEnv, env: &mut ToIr) -> Option<IrValue> {
        match self {
            ValueExpr::Bool(b) => Some(IrValue::Bool(*b)),
            ValueExpr::Char(c) => Some(IrValue::Char(*c)),
            ValueExpr::Int(i) => Some(IrValue::Int(*i)),
            ValueExpr::Float(f) => Some(IrValue::Float(*f)),
            ValueExpr::String(s) => Some(IrValue::String(s.clone())),
            ValueExpr::Lambda(b) => {
                let LambdaFunctionExpr {
                    params,
                    return_type,
                    value_expr,
                } = &**b;

                let mut rparams = Vec::new();
                for p in params {
                    rparams.push((p.0.clone(), p.1.0.as_clean_go_type_name(type_env)));
                }

                let return_type = return_type
                    .as_ref()
                    .map(|(x, _)| x.as_clean_go_type_name(type_env));

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
                        ValFmtStringContents::Char(c) => {
                            concat_params.push(IrValue::String(c.to_string()))
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
            ValueExpr::Match { value_expr, arms } => {
                let (mut instructions, match_on_res) =
                    value_expr.0.direct_or_with_instr(type_env, env);
                let match_on_value = match match_on_res {
                    Some(v) => v,
                    None => return (instructions, None),
                };

                let result_type_annotation = TypeExpr::from_value_expr(self, type_env).as_go_type_annotation(type_env);

                let result_var_name = env.new_var();
                instructions.push(IrInstruction::VarDecl(
                    result_var_name.clone(),
                    result_type_annotation,
                ));

                let mut cases = Vec::new();
                for arm in arms {
                    let type_name = arm.type_case.0.as_go_type_annotation(type_env);

                    let (mut arm_instrs, arm_res) =
                        arm.value_expr.0.direct_or_with_instr(type_env, env);
                    if let Some(res) = arm_res {
                        arm_instrs.push(IrInstruction::VarAssignment(result_var_name.clone(), res));
                    }

                    cases.push(Case {
                        type_name,
                        instrs: arm_instrs,
                        bound_to_identifier: arm.bound_to_identifier.clone(),
                    });
                }

                instructions.push(IrInstruction::SwitchType(match_on_value, cases));
                (instructions, as_rvar(result_var_name))
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
                let ty = type_expr.0.as_go_type_annotation(type_env);
                let mut v = Vec::new();
                v.push(IrInstruction::VarDecl(name.clone(), ty));
                if let Some(initializer) = initializer {
                    let (init_r, inti_r_res) = initializer.0.direct_or_with_instr(type_env, env);
                    v.extend(init_r);
                    if let Some(init_r_res) = inti_r_res {
                        v.push(IrInstruction::VarAssignment(name.clone(), init_r_res));
                    }
                }
                (v, Some(IrValue::empty_tuple()))
            }
            ValueExpr::InlineGo(s) => (
                vec![IrInstruction::InlineGo(s.clone())],
                Some(IrValue::Tuple("struct {\n\n}".into(), vec![])),
            ),
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
                let res_type =
                    TypeExpr::from_value_expr(self, type_env).as_go_type_annotation(type_env);
                let (mut i, cond_res) = condition.0.direct_or_with_instr(type_env, env);
                if cond_res.is_none() {
                    return (i, None);
                }

                let res_var_name = env.new_var();
                i.push(IrInstruction::VarDecl(res_var_name.clone(), res_type));

                let (mut then_instr, then_res) = then.0.direct_or_with_instr(type_env, env);
                if let Some(then_res) = then_res {
                    then_instr.push(IrInstruction::VarAssignment(res_var_name.clone(), then_res));
                }

                let r#else = r#else
                    .clone()
                    .map(|x| x.0.direct_or_with_instr(type_env, env))
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
                        let (target_instr, Some(IrValue::Var(target_res))) =
                            target_obj.0.emit(type_env, env)
                        else {
                            panic!("no var {target_obj:?}");
                        };
                        let target_ty = TypeExpr::from_value_expr(&target_obj.0, type_env);
                        res.extend(target_instr);
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
                        let (target_instr, Some(IrValue::Var(target_res))) =
                            target.0.emit(type_env, env)
                        else {
                            panic!("no var {target:?}");
                        };

                        let (idx_instr, Some(IrValue::Var(idx_res))) = idx.0.emit(type_env, env)
                        else {
                            panic!("no var: {idx:?}")
                        };

                        res.extend(target_instr);
                        res.extend(idx_instr);

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
                let mut res = Vec::new();
                let mut res_var = None;

                for (block_expr, _) in block_exprs {
                    let (block_instr, block_res) = block_expr.direct_or_with_instr(type_env, env);

                    for current in block_instr.iter() {
                        res.push(current.clone());
                        if let IrInstruction::Return(_) = current {
                            res_var = None;
                            return (res, res_var);
                        }
                    }

                    res_var = block_res;
                }

                let mut f_res = Vec::new();
                let ty = TypeExpr::from_value_expr(self, type_env).as_go_type_annotation(type_env);
                let fresvar = env.new_var();

                res_var = res_var.or(Some(IrValue::Tuple("Tup_".into(), vec![])));
                f_res.push(IrInstruction::VarDecl(fresvar.clone(), ty));
                res.push(IrInstruction::VarAssignment(
                    fresvar.clone(),
                    res_var.unwrap(),
                ));
                f_res.push(IrInstruction::Block(res));

                (f_res, Some(IrValue::Var(fresvar)))
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
                let (mut instr, target) = v_target.0.direct_or_with_instr(type_env, env);
                if let Some(target) = target {
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
                        instr.push(IrInstruction::FunCall(Some(res.clone()), target, v_p_res));
                        (instr, Some(IrValue::Var(res)))
                    } else {
                        instr.push(IrInstruction::FunCall(None, target, v_p_res));
                        (instr, None)
                    }
                } else {
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
                    let ty = TypeExpr::from_value_expr(&target_obj.0, type_env);
                    match ty {
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
                        TypeExpr::Struct(Struct { fields }) => {
                            let f = fields.iter().find(|f| f.name == field_name).unwrap();
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
                        _ => panic!("can only access object like"),
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
            ValueExpr::Struct(fields) => {
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
                    IrInstruction::VarDecl(res_var.clone(), name.clone()),
                    IrInstruction::VarAssignment(res_var.clone(), IrValue::Struct(name, res_vars)),
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
                    let res_var = env.new_var();
                    (
                        vec![
                            IrInstruction::VarDecl(
                                res_var.clone(),
                                TypeExpr::from_value_expr(self, type_env)
                                    .as_go_type_annotation(type_env),
                            ),
                            IrInstruction::VarAssignment(res_var.clone(), d),
                        ],
                        Some(IrValue::Var(res_var)),
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
                    IrInstruction::VarAssignment("a".into(), IrValue::String("A".into())),
                ],
            ),
            (
                "{1;}",
                vec![
                    decl("var_1", "Tup_"),
                    IrInstruction::Block(vec![
                        decl("var_0", "Tup_"),
                        IrInstruction::VarAssignment("var_0".into(), IrValue::empty_tuple()),
                        IrInstruction::VarAssignment("var_1".into(), IrValue::Var("var_0".into())),
                    ]),
                ],
            ),
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
                ".{ x: 123 }",
                vec![
                    decl("var_0", "Struct_x_DuckInt"),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Struct(
                            "Struct_x_DuckInt".into(),
                            vec![("x".into(), IrValue::Int(123))],
                        ),
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
                "match (1) { Int x -> 2 }",
                vec![
                    decl("var_0", "Tup_"),
                    IrInstruction::SwitchType(
                        IrValue::Int(1),
                        vec![Case {
                            type_name: "DuckInt".into(),
                            instrs: vec![IrInstruction::VarAssignment(
                                "var_0".into(),
                                IrValue::Int(2),
                            )],
                            bound_to_identifier: "x".into(),
                        }],
                    ),
                ],
            ),
            (
                "match (1 + 1) { Int x -> x }",
                vec![
                    decl("var_0", "DuckInt"),
                    IrInstruction::Add(
                        "var_0".into(),
                        IrValue::Int(1),
                        IrValue::Int(1),
                        TypeExpr::Int,
                    ),
                    decl("var_1", "Tup_"),
                    IrInstruction::SwitchType(
                        IrValue::Var("var_0".into()),
                        vec![Case {
                            type_name: "DuckInt".into(),
                            instrs: vec![IrInstruction::VarAssignment(
                                "var_1".into(),
                                IrValue::Var("x".into()),
                            )],
                            bound_to_identifier: "x".into(),
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
