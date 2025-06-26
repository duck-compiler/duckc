use std::{cell::RefCell, rc::Rc};

use crate::{
    parse::{function_parser::LambdaFunctionExpr, value_parser::ValueExpr},
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

// var_0 := fmt.Println
// var_1 := other_func
// var_2 := var_1()
// var_0(var_2)
//

/// Expression further down should use this
/// if they want the result
type IrRes = String;

#[derive(Debug, Clone, PartialEq)]
pub enum IrInstruction {
    FunDef(),
    VarAssignment(IrRes, IrValue),
    FunCall(IrRes, String, Vec<IrValue>),
    Plus(IrRes, IrValue, IrValue),
    Break,
    Continue,
    Return(Option<IrValue>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
    Lambda(Vec<(String, String)>, Option<String>),
    Tuple(Vec<IrValue>),
    Duck(String, Vec<IrValue>),
    Struct(String, Vec<IrValue>),
    Var(String),
    BoolNegate(Box<IrValue>),
    Equals(Box<IrValue>, Box<IrValue>),
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
    pub fn direct_emit(&self, type_env: &mut TypeEnv) -> Option<IrValue> {
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
                    ..
                } = &**b;

                let mut res_params = Vec::new();
                for p in params {
                    res_params.push((p.0.clone(), p.1.0.as_clean_go_type_name(type_env)))
                }

                Some(IrValue::Lambda(
                    res_params,
                    return_type
                        .as_ref()
                        .cloned()
                        .map(|x| x.0.as_clean_go_type_name(type_env)),
                ))
            }
            _ => None,
        }
    }

    pub fn direct_or_with_instr(
        &self,
        type_env: &mut TypeEnv,
        env: &mut ToIr,
    ) -> (Vec<IrInstruction>, Option<IrValue>) {
        if let Some(v) = self.direct_emit(type_env) {
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
                ir.push(IrInstruction::Plus(
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
                    IrValue::Tuple(res_vars),
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
            _ => {
                if let Some(d) = self.direct_emit(type_env) {
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
                vec![IrInstruction::Plus(
                    "var_0".into(),
                    IrValue::Int(1),
                    IrValue::Int(1),
                )],
            ),
            (
                "a + 1",
                vec![IrInstruction::Plus(
                    "var_0".into(),
                    IrValue::Var("a".into()),
                    IrValue::Int(1),
                )],
            ),
            (
                "a + b",
                vec![IrInstruction::Plus(
                    "var_0".into(),
                    IrValue::Var("a".into()),
                    IrValue::Var("b".into()),
                )],
            ),
            (
                "a + b + c",
                vec![
                    IrInstruction::Plus(
                        "var_0".into(),
                        IrValue::Var("a".into()),
                        IrValue::Var("b".into()),
                    ),
                    IrInstruction::Plus(
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
                    IrValue::Tuple(vec![]),
                )],
            ),
            (
                "!true",
                vec![IrInstruction::VarAssignment(
                    "var_0".into(),
                    IrValue::BoolNegate(IrValue::Bool(true).into()),
                )],
            ),
            ("(true, break, x)", vec![IrInstruction::Break]),
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
