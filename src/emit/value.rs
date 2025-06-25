use std::{cell::RefCell, rc::Rc};

use crate::{
    parse::{
        function_parser::LambdaFunctionExpr,
        value_parser::ValueExpr,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
    Lambda(Vec<(String, String)>, Option<String>),
    Tuple(Vec<String>),
    Duck(String, Vec<String>),
    Struct(String, Vec<String>),
    Var(String),
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
            (instr, res.map(IrValue::Var))
        }
    }

    pub fn emit(
        &self,
        type_env: &mut TypeEnv,
        env: &mut ToIr,
    ) -> (Vec<IrInstruction>, Option<String>) {
        match self {
            ValueExpr::Add(v1, v2) => {
                let mut ir = Vec::new();

                let (v1_instr, v1_res) = v1.0.direct_or_with_instr(type_env, env);
                let (v2_instr, v2_res) = v2.0.direct_or_with_instr(type_env, env);

                ir.extend(v1_instr);
                ir.extend(v2_instr);

                let var = env.new_var();
                ir.push(IrInstruction::Plus(
                    var.clone(),
                    v1_res.unwrap(),
                    v2_res.unwrap(),
                ));

                (ir, Some(var))
            }
            ValueExpr::Variable(x, _) => (vec![], Some(x.to_owned())),
            _ => {
                todo!()
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
