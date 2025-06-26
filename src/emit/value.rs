use crate::{
    parse::{
        assignment_and_declaration_parser::Declaration, function_parser::LambdaFunctionExpr,
        type_parser::TypeExpr, value_parser::ValueExpr,
    },
    semantics::typechecker::TypeEnv,
};

#[derive(Debug, Clone, Default)]
pub struct ToIr {
    pub var_counter: usize,
}

/// Expression further down should use this
/// if they want the result
type IrRes = String;

#[derive(Debug, Clone, PartialEq)]
pub enum IrInstruction {
    // Code Statements
    VarDecl(String, String),
    VarAssignment(IrRes, IrValue),
    FunCall(IrRes, String, Vec<IrValue>),
    Add(IrRes, IrValue, IrValue),
    Mul(IrRes, IrValue, IrValue),
    Equals(IrRes, IrValue, IrValue),
    Break,
    Continue,
    Return(Option<IrValue>),
    InlineGo(String),
    If(IrValue, Vec<IrInstruction>, Option<Vec<IrInstruction>>),
    Loop(Vec<IrInstruction>),

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
        String,                // Name
        Vec<(String, String)>, // Generics
        Vec<(String, String)>, // Methods
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
    Lambda(Vec<(String, String)>, Option<String>, Vec<IrInstruction>),
    Tuple(String, Vec<IrValue>),
    Duck(String, Vec<(String, IrValue)>),
    Struct(String, Vec<(String, IrValue)>),
    Var(String),
    BoolNegate(Box<IrValue>),
    FieldAccess(Box<IrValue>, String),
}

impl IrValue {
    pub fn empty_tuple() -> Self {
        Self::Tuple(
            TypeExpr::from_value_expr(&ValueExpr::Tuple(vec![]), &mut TypeEnv::default())
                .as_go_concrete_annotation(&mut TypeEnv::default()),
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

                let (b_instr, _) = value_expr.0.emit(type_env, env);
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
        if let Some(v) = self.direct_emit(type_env, env) {
            (Vec::new(), Some(v))
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
            ValueExpr::VarDecl(b) => {
                let Declaration {
                    name,
                    type_expr,
                    initializer,
                } = &b.0;
                let ty = type_expr.0.as_go_concrete_annotation(type_env);
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
                // TODO:
                // let res_type =
                //     TypeExpr::from_value_expr(self, type_env).as_go_concrete_annotation(type_env);
                let res_type = "interface{}".to_string();
                let (mut i, cond_res) = condition.0.direct_or_with_instr(type_env, env);
                dbg!(&condition.0);
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
                let (mut i, res) = assign.value_expr.0.direct_or_with_instr(type_env, env);
                if let Some(res) = res {
                    i.push(IrInstruction::VarAssignment(assign.name.clone(), res));
                    (i, Some(as_var(&assign.name)))
                } else {
                    (i, None)
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

                let var = env.new_var();
                ir.push(IrInstruction::VarDecl(
                    var.clone(),
                    TypeExpr::from_value_expr(&v1.0, type_env).as_go_concrete_annotation(type_env),
                ));
                ir.push(IrInstruction::Add(
                    var.clone(),
                    v1_res.unwrap(),
                    v2_res.unwrap(),
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

                let var = env.new_var();
                ir.push(IrInstruction::VarDecl(
                    var.clone(),
                    TypeExpr::from_value_expr(&v1.0, type_env).as_go_concrete_annotation(type_env),
                ));
                ir.push(IrInstruction::Mul(
                    var.clone(),
                    v1_res.unwrap(),
                    v2_res.unwrap(),
                ));

                (ir, as_rvar(var))
            }
            ValueExpr::Block(block_exprs) => {
                let mut res = Vec::new();
                let mut res_var = None;
                for (block_expr, _) in block_exprs {
                    let (block_instr, block_res) = block_expr.direct_or_with_instr(type_env, env);
                    res.extend(block_instr);
                    if block_res.is_none() {
                        return (res, None);
                    }
                    res_var = block_res;
                }
                (res, res_var)
            }
            ValueExpr::Tuple(fields) => {
                let mut res = Vec::new();
                let mut res_vars = Vec::new();
                let name =
                    TypeExpr::from_value_expr(self, type_env).as_go_concrete_annotation(type_env);
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
                    let (mut instr, res) = expr.direct_or_with_instr(type_env, env);
                    instr.push(IrInstruction::Return(res));
                    (instr, None)
                } else {
                    (vec![IrInstruction::Return(None)], None)
                }
            }
            ValueExpr::FunctionCall { target, params } => {
                let (mut instr, target) = target.0.emit(type_env, env);
                if let Some(target) = target {
                    let IrValue::Var(x) = target else {
                        panic!("can only call var")
                    };

                    let mut v_p_res = Vec::new();
                    for (p, _) in params {
                        let (p_instr, p_res) = p.direct_or_with_instr(type_env, env);
                        instr.extend(p_instr);
                        if let Some(p_res) = p_res {
                            v_p_res.push(p_res);
                        } else {
                            return (instr, None);
                        }
                    }

                    let TypeExpr::Fun(_, return_type) = TypeExpr::from_value_expr(self, type_env)
                    else {
                        panic!("can only call function")
                    };

                    if let Some(return_type) = return_type {
                        let res = env.new_var();
                        instr.push(IrInstruction::VarDecl(
                            res.clone(),
                            return_type.0.as_go_concrete_annotation(type_env),
                        ));
                        instr.push(IrInstruction::FunCall(res.clone(), x, v_p_res));
                        (instr, Some(IrValue::Var(res)))
                    } else {
                        (instr, None)
                    }
                } else {
                    (instr, None)
                }
            }
            ValueExpr::Variable(x, _) => (vec![], as_rvar(x.to_owned())),
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
                    IrInstruction::VarDecl(var.clone(), "bool".into()),
                    IrInstruction::Equals(var.clone(), v1_res.unwrap(), v2_res.unwrap()),
                ]);

                (ir, as_rvar(var))
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let (i, t_res) = target_obj.0.emit(type_env, env);
                if let Some(t_res) = t_res {
                    (
                        i,
                        Some(IrValue::FieldAccess(t_res.into(), field_name.clone())),
                    )
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
                    IrInstruction::VarDecl(res_var.clone(), name.clone()),
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
                dbg!("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
                if let Some(d) = self.direct_emit(type_env, env) {
                    let res_var = env.new_var();
                    dbg!((
                        vec![
                            IrInstruction::VarDecl(
                                res_var.clone(),
                                TypeExpr::from_value_expr(self, type_env)
                                    .as_go_type_annotation(type_env),
                            ),
                            IrInstruction::VarAssignment(res_var.clone(), d),
                        ],
                        Some(IrValue::Var(res_var)),
                    ))
                } else {
                    dbg!(self);
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
        emit::value::{IrInstruction, IrValue, ToIr},
        parse::{
            lexer::lexer,
            make_input,
            value_parser::{empty_range, value_expr_parser},
        },
        semantics::typechecker::TypeEnv,
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
                    IrInstruction::Add("var_0".into(), IrValue::Int(1), IrValue::Int(1)),
                ],
            ),
            (
                "1 * 1",
                vec![
                    decl("var_0", "int"),
                    IrInstruction::Mul("var_0".into(), IrValue::Int(1), IrValue::Int(1)),
                ],
            ),
            (
                "let a: String = \"A\"",
                vec![
                    IrInstruction::VarDecl("a".into(), "string".into()),
                    IrInstruction::VarAssignment("a".into(), IrValue::String("A".into())),
                ],
            ),
            (
                "{1;}",
                vec![
                    decl("var_0", "struct {\n\n}"),
                    IrInstruction::VarAssignment("var_0".into(), IrValue::empty_tuple()),
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
            ("(true, break, 2)", vec![IrInstruction::Break]),
            ("(true, return, 2)", vec![IrInstruction::Return(None)]),
            ("(true, continue, 3)", vec![IrInstruction::Continue]),
            (
                "1 == 2",
                vec![
                    decl("var_0", "bool"),
                    IrInstruction::Equals("var_0".into(), IrValue::Int(1), IrValue::Int(2)),
                ],
            ),
            (
                ".{ x: 123 }",
                vec![
                    decl("var_0", "Struct_x_int"),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Struct(
                            "Struct_x_int".into(),
                            vec![("x".into(), IrValue::Int(123))],
                        ),
                    ),
                ],
            ),
            (
                "{ x: 123 }",
                vec![
                    decl("var_0", "Duck_x_int"),
                    IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::Duck("Duck_x_int".into(), vec![("x".into(), IrValue::Int(123))]),
                    ),
                ],
            ),
            (
                "while (true) {}",
                vec![IrInstruction::Loop(vec![IrInstruction::If(
                    IrValue::Bool(true),
                    vec![
                        decl("var_0", "struct {\n\n}"),
                        IrInstruction::VarAssignment("var_0".into(), IrValue::empty_tuple()),
                    ],
                    Some(vec![IrInstruction::Break]),
                )])],
            ),
        ];

        for (src, exp) in test_cases {
            let lexed = lexer("test", src).parse(src).unwrap();
            let parsed = value_expr_parser(make_input)
                .parse(make_input(empty_range(), &lexed))
                .unwrap()
                .0;
            let ir = parsed.emit(&mut TypeEnv::default(), &mut ToIr::default());
            assert_eq!(exp, ir.0, "{src}");
        }
    }
}
