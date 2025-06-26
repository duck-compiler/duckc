use std::{cell::RefCell, rc::Rc};

use crate::{
    parse::{
        assignment_and_declaration_parser::Declaration, function_parser::LambdaFunctionExpr,
        type_parser::TypeExpr, value_parser::ValueExpr,
    },
    semantics::typechecker::TypeEnv,
};

#[derive(Clone, Debug)]
pub struct GoMethodDef {
    pub name: String,
    pub return_type: Option<String>,
    pub params: Vec<(String, String)>,
    pub body: Vec<String>,
}

impl GoMethodDef {
    pub fn emit(&self, receiver: Option<String>, interface_style: bool) -> Vec<String> {
        let name_param_return_type = format!(
            "{}({}) {}",
            self.name,
            self.params
                .iter()
                .map(|(name, data_type)| format!("{name} {data_type}"))
                .reduce(|acc, x| format!("{acc}, {x}"))
                .unwrap_or(String::new()),
            self.return_type.as_ref().unwrap_or(&String::new())
        );
        if interface_style {
            vec![name_param_return_type, "\n".to_string()]
        } else {
            vec![
                format!(
                    "func {} {} {}\n",
                    receiver.unwrap_or_default(),
                    name_param_return_type,
                    "{"
                ),
                self.body
                    .iter()
                    .map(ToOwned::to_owned)
                    .reduce(|acc, x| format!("{acc}{x}\n"))
                    .unwrap_or_default(),
                "\n}".to_string(),
                "\n".to_string(),
            ]
        }
    }
}

#[derive(Clone, Debug)]
pub enum GoTypeDef {
    Struct {
        name: String,
        fields: Vec<(String, String)>,
        methods: Vec<GoMethodDef>,
    },
    Interface {
        name: String,
        methods: Vec<GoMethodDef>,
    },
}

impl GoTypeDef {
    pub fn name(&self) -> &str {
        match self {
            GoTypeDef::Struct {
                name,
                fields: _,
                methods: _,
            } => name,
            GoTypeDef::Interface { name, methods: _ } => name,
        }
    }
    pub fn emit(&self) -> Vec<String> {
        match self {
            GoTypeDef::Struct {
                name,
                fields,
                methods,
            } => {
                let mut res = Vec::new();
                res.push(format!("type {} struct {}\n", name, "{"));
                for (name, field_type) in fields {
                    res.push(format!("{name} {field_type}\n"));
                }
                res.push("}\n".to_string());
                for method in methods {
                    res.extend(method.emit(Some(format!("(self {name})")), false));
                }
                res
            }
            GoTypeDef::Interface { name, methods } => {
                let mut res = Vec::new();
                res.push(format!("type {} interface {}\n", name, "{"));
                for method in methods {
                    res.extend(method.emit(None, true).into_iter());
                }
                res.push("}\n".to_string());
                res
            }
        }
    }
}

#[derive(PartialEq, Clone, Debug, Default)]
pub struct GoImport {
    pub path: String,
    pub alias: Option<String>,
}

#[derive(Clone, Debug)]
pub struct EmitEnvironment {
    pub imports: Rc<RefCell<Vec<GoImport>>>,
    pub types: Rc<RefCell<Vec<GoTypeDef>>>,
    pub var_counter: Rc<RefCell<usize>>,
}

impl Default for EmitEnvironment {
    fn default() -> Self {
        EmitEnvironment {
            imports: Rc::new(RefCell::new(Vec::new())),
            types: Rc::new(RefCell::new(Vec::new())),
            var_counter: Rc::new(RefCell::new(0)),
        }
    }
}

impl EmitEnvironment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push_types(&self, types: impl Iterator<Item = GoTypeDef>) {
        let mut x = self.types.borrow_mut();
        for type_def in types {
            if !x.iter().any(|e| e.name() == type_def.name()) {
                x.push(type_def);
            }
        }
    }

    pub fn push_import(&self, import: impl Into<GoImport>) -> Option<String> {
        let mut imports = self.imports.borrow_mut();
        let import = import.into();
        for i in imports.iter() {
            if i.path == import.path {
                return i.alias.clone();
            }
        }
        imports.push(import);
        None
    }

    pub fn emit_types(&self) -> String {
        self.types
            .borrow()
            .iter()
            .map(|x| x.emit().join(""))
            .collect::<Vec<_>>()
            .join("")
    }

    pub fn emit_imports_and_types(&self) -> String {
        format!(
            "import (\n{}\n)\n{}",
            self.imports
                .borrow()
                .iter()
                .map(|x| format!(
                    "{} \"{}\"",
                    x.alias.as_ref().unwrap_or(&String::new()),
                    x.path
                ))
                .collect::<Vec<_>>()
                .join("\n"),
            self.emit_types()
        )
    }
}

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
    GoPaackage(String),
    GoImports(Vec<(Option<String>, String)>),
    FunDef(
        String,
        Vec<(String, String)>,
        Option<String>,
        Vec<IrInstruction>,
    ),
    StructDef(String, Vec<(String, String)>),
    InterfaceDef(String, Vec<(String, String)>),
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

pub fn emit(
    _value_expr: ValueExpr,
    _env: EmitEnvironment,
    _type_env: &mut TypeEnv,
) -> (Vec<String>, Option<String>) {
    todo!()
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
        dbg!(self);
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
                    let res = env.new_var();
                    instr.push(IrInstruction::FunCall(res.clone(), x, v_p_res));
                    (instr, Some(IrValue::Var(res)))
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
                ir.push(IrInstruction::Equals(
                    var.clone(),
                    v1_res.unwrap(),
                    v2_res.unwrap(),
                ));

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
                res.push(IrInstruction::VarAssignment(
                    res_var.clone(),
                    IrValue::Duck(name, res_vars),
                ));

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
                res.push(IrInstruction::VarAssignment(
                    res_var.clone(),
                    IrValue::Struct(name, res_vars),
                ));

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
                        vec![IrInstruction::VarAssignment(res_var.clone(), d)],
                        Some(IrValue::Var(res_var)),
                    )
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
        emit::value::{IrInstruction, IrValue, ToIr, as_var},
        parse::{
            lexer::lexer,
            make_input,
            value_parser::{empty_range, value_expr_parser},
        },
        semantics::typechecker::TypeEnv,
    };

    #[test]
    fn test_code_emit() {
        let test_cases = vec![
            (
                "1 + 1",
                vec![IrInstruction::Add(
                    "var_0".into(),
                    IrValue::Int(1),
                    IrValue::Int(1),
                )],
            ),
            (
                "1 * 1",
                vec![IrInstruction::Mul(
                    "var_0".into(),
                    IrValue::Int(1),
                    IrValue::Int(1),
                )],
            ),
            (
                "a + 1",
                vec![IrInstruction::Add(
                    "var_0".into(),
                    IrValue::Var("a".into()),
                    IrValue::Int(1),
                )],
            ),
            (
                "a + b",
                vec![IrInstruction::Add(
                    "var_0".into(),
                    IrValue::Var("a".into()),
                    IrValue::Var("b".into()),
                )],
            ),
            (
                "let a: String = \"A\"",
                vec![
                    IrInstruction::VarDecl("a".into(), "string".into()),
                    IrInstruction::VarAssignment("a".into(), IrValue::String("A".into())),
                ],
            ),
            (
                "a + b + c",
                vec![
                    IrInstruction::Add(
                        "var_0".into(),
                        IrValue::Var("a".into()),
                        IrValue::Var("b".into()),
                    ),
                    IrInstruction::Add(
                        "var_1".into(),
                        IrValue::Var("var_0".into()),
                        IrValue::Var("c".into()),
                    ),
                ],
            ),
            (
                "{1;}",
                vec![IrInstruction::VarAssignment(
                    "var_0".into(),
                    IrValue::empty_tuple(),
                )],
            ),
            (
                "!true",
                vec![IrInstruction::VarAssignment(
                    "var_0".into(),
                    IrValue::BoolNegate(IrValue::Bool(true).into()),
                )],
            ),
            ("(true, break, 2)", vec![IrInstruction::Break]),
            ("(true, return, 2)", vec![IrInstruction::Return(None)]),
            ("(true, continue, 3)", vec![IrInstruction::Continue]),
            (
                "x()",
                vec![IrInstruction::FunCall("var_0".into(), "x".into(), vec![])],
            ),
            (
                "x(y())",
                vec![
                    IrInstruction::FunCall("var_0".into(), "y".into(), vec![]),
                    IrInstruction::FunCall("var_1".into(), "x".into(), vec![as_var("var_0")]),
                ],
            ),
            (
                "x(y(), z())",
                vec![
                    IrInstruction::FunCall("var_0".into(), "y".into(), vec![]),
                    IrInstruction::FunCall("var_1".into(), "z".into(), vec![]),
                    IrInstruction::FunCall(
                        "var_2".into(),
                        "x".into(),
                        vec![as_var("var_0"), as_var("var_1")],
                    ),
                ],
            ),
            (
                "1 == 2",
                vec![IrInstruction::Equals(
                    "var_0".into(),
                    IrValue::Int(1),
                    IrValue::Int(2),
                )],
            ),
            (
                "x() == y()",
                vec![
                    IrInstruction::FunCall("var_0".into(), "x".into(), vec![]),
                    IrInstruction::FunCall("var_1".into(), "y".into(), vec![]),
                    IrInstruction::Equals("var_2".into(), as_var("var_0"), as_var("var_1")),
                ],
            ),
            (
                "x.y == x.z",
                vec![IrInstruction::Equals(
                    "var_0".into(),
                    IrValue::FieldAccess(as_var("x").into(), "y".into()),
                    IrValue::FieldAccess(as_var("x").into(), "z".into()),
                )],
            ),
            (
                "x.y.z == x.z.ww",
                vec![IrInstruction::Equals(
                    "var_0".into(),
                    IrValue::FieldAccess(
                        IrValue::FieldAccess(as_var("x").into(), "y".into()).into(),
                        "z".into(),
                    ),
                    IrValue::FieldAccess(
                        IrValue::FieldAccess(as_var("x").into(), "z".into()).into(),
                        "ww".into(),
                    ),
                )],
            ),
            (
                ".{ x: 123 }",
                vec![IrInstruction::VarAssignment(
                    "var_0".into(),
                    IrValue::Struct("Struct_x_int".into(), vec![("x".into(), IrValue::Int(123))]),
                )],
            ),
            (
                "{ x: 123 }",
                vec![IrInstruction::VarAssignment(
                    "var_0".into(),
                    IrValue::Duck("Duck_x_int".into(), vec![("x".into(), IrValue::Int(123))]),
                )],
            ),
            (
                "while (true) {}",
                vec![IrInstruction::Loop(vec![IrInstruction::If(
                    IrValue::Bool(true),
                    vec![IrInstruction::VarAssignment(
                        "var_0".into(),
                        IrValue::empty_tuple(),
                    )],
                    Some(vec![IrInstruction::Break]),
                )])],
            ),
            (
                "(() => x(1,2,3)) == (() => true)",
                vec![IrInstruction::Equals(
                    "var_2".into(),
                    IrValue::Lambda(
                        vec![],
                        None,
                        vec![IrInstruction::FunCall(
                            "var_0".into(),
                            "x".into(),
                            vec![IrValue::Int(1), IrValue::Int(2), IrValue::Int(3)],
                        )],
                    )
                    .into(),
                    IrValue::Lambda(
                        vec![],
                        None,
                        vec![IrInstruction::VarAssignment(
                            "var_1".into(),
                            IrValue::Bool(true),
                        )],
                    )
                    .into(),
                )],
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
