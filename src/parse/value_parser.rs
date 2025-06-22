use std::{cell::RefCell, rc::Rc};

use crate::parse::{
    assignment_and_declaration_parser::{Assignment, Declaration}, function_parser::{LambdaFunctionExpr, Param}, type_parser::type_expression_parser,
};

use super::lexer::Token;
use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    FunctionCall {
        target: Box<ValueExpr>,
        params: Vec<ValueExpr>,
    },
    Int(i64),
    String(String),
    Bool(bool),
    Float(f64),
    Char(char),
    Variable(String),
    If {
        condition: Box<ValueExpr>,
        then: Box<ValueExpr>,
        r#else: Box<ValueExpr>,
    },
    While {
        condition: Box<ValueExpr>,
        body: Box<ValueExpr>,
    },
    Tuple(Vec<ValueExpr>),
    Block(Vec<ValueExpr>),
    Break,
    Continue,
    Duck(Vec<(String, ValueExpr)>),
    Struct(Vec<(String, ValueExpr)>),
    FieldAccess {
        target_obj: Box<ValueExpr>,
        field_name: String,
    },
    Return(Option<Box<ValueExpr>>),
    VarAssign(Box<Assignment>),
    VarDecl(Box<Declaration>),
    Add(Box<ValueExpr>, Box<ValueExpr>),
    Mul(Box<ValueExpr>, Box<ValueExpr>),
    BoolNegate(Box<ValueExpr>),
    Equals(Box<ValueExpr>, Box<ValueExpr>),
    InlineGo(String),
    Lambda(Box<LambdaFunctionExpr>),
}

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

    pub fn emit_imports_and_types(&self) -> String {
        format!(
            "import (\n{}\n)\n{}",
            self.imports
                .borrow()
                .iter()
                .map(|x| format!("{} \"{}\"", x.alias.as_ref().unwrap_or(&String::new()), x.path))
                .collect::<Vec<_>>()
                .join("\n"),
            self.types
                .borrow()
                .iter()
                .map(|x| x.emit().join(""))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

impl ValueExpr {
    pub fn into_block(self) -> ValueExpr {
        ValueExpr::Block(vec![self])
    }

    pub fn needs_semicolon(&self) -> bool {
        match self {
            ValueExpr::If {
                condition: _,
                then: _,
                r#else: _,
            } => false,
            ValueExpr::While {
                condition: _,
                body: _,
            } => false,
            ValueExpr::Block(_) => false,
            ValueExpr::InlineGo(_) => false,
            _ => true,
        }
    }
}

pub fn emit(x: ValueExpr, env: EmitEnvironment) -> (Vec<String>, Option<String>) {
    let new_var = || {
        let x = format!("var_{}", env.var_counter.borrow());
        *env.var_counter.borrow_mut() += 1;
        x
    };

    let single = |instr: &str| {
        let r = new_var();
        (vec![format!("{r} := {instr}\n")], Some(r.to_string()))
    };

    let no_var = |instr: &str| (vec![instr.to_owned()], None);

    match x {
        ValueExpr::Lambda(expr) => {
            let LambdaFunctionExpr { params, return_type, value_expr } = *expr;
            for param in &params {
                param.1.emit_into_env(env.clone());
            }
            let (mut v_instr, res_name) = emit(value_expr, env.clone());
            if let Some(res_name) = res_name {
                v_instr.push(format!("_ = {res_name}\n"))
            }

            let res_var = new_var();
            let mut instr = Vec::new();
            let lambda_creation = format!("{res_var} := func({}) {} {}\n",
                params.iter()
                    .map(|(name, t)| format!("{name} {}", t.emit().0))
                    .collect::<Vec<_>>().join(", "),
                return_type.map(|t| t.emit().0).unwrap_or_default(),
                "{");

            instr.push(lambda_creation);
            instr.extend(v_instr);
            instr.push("\n}\n".to_string());

            (instr, Some(res_var))
        }
        ValueExpr::InlineGo(code) => {
            let mut res = vec!["\n".to_string()];
            res.extend(code.split("\n").map(|x| format!("{x}\n")));
            res.push("\n".to_string());
            (res, None)
        }
        ValueExpr::Equals(x, y) => {
            let (mut x_instr, Some(x_res)) = emit(*x, env.clone()) else {
                panic!()
            };
            let (y_instr, Some(y_res)) = emit(*y, env.clone()) else {
                panic!()
            };
            x_instr.extend(y_instr);
            let res_var = new_var();
            x_instr.push(format!("{res_var} := {x_res} == {y_res}\n"));
            (x_instr, Some(res_var))
        }
        ValueExpr::BoolNegate(expr) => {
            let res_var = new_var();
            let (mut expr_instr, Some(expr_res)) = emit(*expr, env.clone()) else {
                panic!()
            };
            expr_instr.push(format!("{res_var} := !{expr_res}\n"));
            (expr_instr, Some(res_var))
        }
        ValueExpr::Add(x, y) => {
            let (mut x_instr, Some(x_res)) = emit(*x, env.clone()) else {
                panic!()
            };
            let (y_instr, Some(y_res)) = emit(*y, env.clone()) else {
                panic!()
            };
            x_instr.extend(y_instr);
            let res_var = new_var();
            x_instr.push(format!("{res_var} := {x_res} + {y_res}\n"));
            (x_instr, Some(res_var))
        }
        ValueExpr::Mul(x, y) => {
            let (mut x_instr, Some(x_res)) = emit(*x, env.clone()) else {
                panic!()
            };
            let (y_instr, Some(y_res)) = emit(*y, env.clone()) else {
                panic!()
            };
            x_instr.extend(y_instr);
            let res_var = new_var();
            x_instr.push(format!("{res_var} := {x_res} * {y_res}\n"));
            (x_instr, Some(res_var))
        }
        ValueExpr::VarDecl(b) => {
            let Declaration {
                name,
                type_expr: t,
                initializer,
            } = *b;
            if let Some(initializer) = initializer {
                let (instr, Some(res_var)) = emit(initializer, env.clone()) else {
                    panic!()
                };
                let mut res = Vec::new();
                res.extend(instr);
                res.push(format!("{name} := {res_var}\n"));
                (res, Some(name))
            } else {
                (vec![format!("var {name} {}\n", t.emit().0)], Some(name))
            }
        }
        ValueExpr::VarAssign(b) => {
            let Assignment { name, value_expr } = *b;
            let (mut instr, Some(res)) = emit(value_expr, env.clone()) else {
                panic!()
            };
            instr.push(format!("{name} = {res}\n"));
            (instr, Some(name))
        }
        ValueExpr::While { condition, body } => {
            let (cond_instr, Some(cond_res)) = emit(*condition, env.clone()) else {
                panic!()
            };
            let mut instr = Vec::new();

            instr.push("for {\n".to_string());
            instr.extend(cond_instr);
            instr.push(format!("if !{} {}\n", cond_res, "{"));
            instr.push("break\n".to_string());
            instr.push("}\n".to_string());

            let (body_instr, _) = emit(*body, env.clone());
            instr.extend(body_instr);

            instr.push("}\n".to_string());

            (instr, None)
        }
        ValueExpr::Return(expr) => {
            let mut instr = Vec::new();
            if let Some(expr) = expr {
                let (expr_instr, Some(expr_res)) = emit(*expr, env.clone()) else {
                    panic!()
                };
                instr.extend(expr_instr);
                instr.push(format!("return {expr_res}\n"));
            } else {
                instr.push("return\n".to_string());
            }
            (instr, None)
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            let res_var = new_var();
            let (cond_instr, Some(cond_res)) =
                emit(ValueExpr::clone(condition.as_ref()), env.clone())
            else {
                panic!()
            };

            let mut res_instr = Vec::new();
            res_instr.extend(cond_instr);
            res_instr.push(format!("var {res_var} interface{}\n", "{}"));
            res_instr.push(format!("if {} {}\n", cond_res, "{"));
            let (then_instr, res) = emit(ValueExpr::clone(then.as_ref()), env.clone());
            res_instr.extend(then_instr);
            if let Some(res) = res {
                res_instr.push(format!("{res_var} = {res}\n"))
            }
            res_instr.push("} else {\n".to_string());
            let (r#else_instr, res) = emit(ValueExpr::clone(r#else.as_ref()), env.clone());
            res_instr.extend(r#else_instr);
            if let Some(res) = res {
                res_instr.push(format!("{res_var} = {res}\n"))
            }
            res_instr.push("}".to_string());

            (res_instr, Some(res_var))
        }
        ValueExpr::Break => no_var("break"),
        ValueExpr::Continue => no_var("continue"),
        ValueExpr::Int(i) => single(&i.to_string()),
        ValueExpr::Bool(b) => single(&b.to_string()),
        ValueExpr::Float(f) => single(&f.to_string()),
        ValueExpr::Char(c) => single(&format!("'{c}'")),
        ValueExpr::String(s) => single(&format!("\"{s}\"")),
        ValueExpr::Variable(ident) => single(&ident),
        ValueExpr::Tuple(exprs) => {
            let mut instrs: Vec<String> = Vec::new();
            let mut results = Vec::new();
            for expr in exprs.into_iter() {
                let (instr, Some(res)) = emit(expr, env.clone()) else {
                    panic!()
                };
                instrs.extend(instr.into_iter());
                results.push(res);
            }

            let res = new_var();

            let mut final_instr = Vec::new();
            final_instr.push(format!("{res} := "));
            final_instr.push("struct{".to_string());
            final_instr.push(
                results
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format!("field_{i} interface{}\n", "{}",))
                    .reduce(|acc, x| format!("{acc}{x}"))
                    .unwrap_or(String::new()),
            );
            final_instr.push("}{".to_string());
            final_instr.push(
                results
                    .into_iter()
                    .reduce(|acc, x| format!("{acc}, {x}"))
                    .unwrap_or(String::new()),
            );
            final_instr.push("}\n".to_string());
            instrs.extend(final_instr);
            (instrs, Some(res))
        }
        ValueExpr::FunctionCall { target, params } => {
            let mut res = Vec::new();
            let mut target = ValueExpr::clone(target.as_ref());

            let mut with_result = true;
            if let ValueExpr::Variable(x) = &mut target
                && x == "@println"
            {
                let package_name = env.push_import(GoImport {
                    alias: None,
                    path: "fmt".into()
                }).unwrap_or("fmt".into());
                *x = format!("{package_name}.Println").to_string();
                with_result = false;
            }

            let (target_instr, Some(target_res_name)) = emit(target, env.clone()) else {
                panic!()
            };
            dbg!(&target_instr);
            res.extend(target_instr);

            let mut params_instructions = Vec::new();
            let mut param_results = Vec::new();

            for expr in params.into_iter() {
                let (instr, Some(res)) = emit(expr, env.clone()) else {
                    panic!()
                };
                params_instructions.extend(instr.into_iter());

                param_results.push(res);
            }

            let result = new_var();

            let final_instr = format!(
                "{}{target_res_name}({})\n",
                if with_result {
                    format!("{result} := ")
                } else {
                    "".to_string()
                },
                param_results
                    .clone()
                    .into_iter()
                    .reduce(|acc, x| format!("{acc}, {x}"))
                    .unwrap_or(String::new())
            );

            res.extend(params_instructions);
            res.push(final_instr);

            if with_result {
                res.push(format!("_ = {result}\n"));
            }

            (res, Some(result))
        }
        ValueExpr::Struct(fields) => {
            let mut field_instr = Vec::new();
            let mut field_res = Vec::new();

            let mut type_fields = Vec::new();
            let mut go_type_name = "Struct".to_string();

            let mut methods = Vec::new();

            for (field_name, field_init) in fields {
                let (this_field_instr, Some(this_field_res)) = emit(field_init, env.clone()) else {
                    panic!()
                };
                field_instr.extend(this_field_instr.into_iter());
                field_res.push(this_field_res);
                let go_type_name = "interface{}".to_string(); // TODO: resolved type
                type_fields.push((field_name.clone(), go_type_name.clone()));
                methods.push(GoMethodDef {
                    name: format!("Duck_Get{field_name}"),
                    body: vec![format!("return self.{field_name}")],
                    params: vec![],
                    return_type: Some(go_type_name.clone()),
                });
            }

            for (field_name, field_type) in &type_fields {
                go_type_name.push_str(&format!(
                    "_Has{field_name}_{}",
                    field_type.replace("interface{}", "Any")
                ));
            }

            let mut go_interface_name = "Duck".to_string();

            let mut cloned_type_fields = type_fields.clone();
            cloned_type_fields.sort();
            for (field_name, field_type) in &type_fields {
                go_interface_name.push_str(&format!(
                    "_Has{field_name}_{}",
                    field_type.replace("interface{}", "Any")
                ));
            }

            let go_struct = GoTypeDef::Struct {
                name: go_type_name.clone(),
                fields: type_fields,
                methods: methods.clone(),
            };
            let go_interface = GoTypeDef::Interface {
                name: go_interface_name,
                methods: methods.clone(),
            };

            env.push_types([go_struct, go_interface].into_iter());

            let res_name = new_var();

            field_instr.extend([format!(
                "{res_name} := {go_type_name}{}{}{}\n",
                "{",
                field_res.join(", "),
                "}"
            )]);

            (field_instr, Some(res_name))
        }
        ValueExpr::Duck(fields) => {
            let mut field_instr = Vec::new();
            let mut field_res = Vec::new();

            let mut type_fields = Vec::new();
            let mut go_type_name = "Duck".to_string();

            let mut methods = Vec::new();

            for (field_name, field_init) in fields {
                let (this_field_instr, Some(this_field_res)) = emit(field_init, env.clone()) else {
                    panic!()
                };
                field_instr.extend(this_field_instr.into_iter());
                field_res.push(this_field_res);
                let go_type_name = "interface{}".to_string();
                type_fields.push((field_name.clone(), go_type_name.clone()));
                methods.push(GoMethodDef {
                    name: format!("Duck_Get{field_name}"),
                    body: vec![format!("return self.{field_name}")],
                    params: vec![],
                    return_type: Some(go_type_name.clone()),
                });
            }

            type_fields.sort();

            for (field_name, field_type) in &type_fields {
                go_type_name.push_str(&format!(
                    "_Has{field_name}_{}",
                    field_type.replace("interface{}", "Any")
                ));
            }

            let go_interface_name = go_type_name.clone();
            go_type_name.push_str("_Struct");

            let go_struct = GoTypeDef::Struct {
                name: go_type_name.clone(),
                fields: type_fields,
                methods: methods.clone(),
            };
            let go_interface = GoTypeDef::Interface {
                name: go_interface_name,
                methods: methods.clone(),
            };

            env.push_types([go_struct, go_interface].into_iter());

            let res_name = new_var();

            field_instr.extend([format!(
                "{res_name} := {go_type_name}{}{}{}\n",
                "{",
                field_res.join(", "),
                "}"
            )]);

            (field_instr, Some(res_name))
        }
        ValueExpr::Block(mut exprs) => {
            if !exprs.is_empty() {
                exprs = exprs[..=exprs
                    .iter()
                    .enumerate()
                    .filter_map(|(i, x)| match x {
                        ValueExpr::Return(_) => Some(i),
                        _ => None,
                    })
                    .next()
                    .unwrap_or(exprs.len() - 1)]
                    .iter()
                    .map(Clone::clone)
                    .collect::<Vec<_>>();
            }
            dbg!(&exprs);

            let mut instrs = Vec::new();
            let mut res = Vec::new();
            for expr in exprs {
                let (instr, res_var) = emit(expr, env.clone());
                instrs.extend(instr.into_iter());
                res.push(res_var);
            }
            if let Some(Some(res)) = res.last() {
                instrs.push(format!("_ = {res}\n"));
                (instrs, Some(res.clone()))
            } else {
                (instrs, None)
            }
        }
        _ => {
            dbg!(x);
            todo!()
        }
    }
}

pub fn value_expr_parser<'src>() -> impl Parser<'src, &'src [Token], ValueExpr> {
    recursive(|value_expr_parser| {
        let lambda_parser = {
            let param_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .then_ignore(just(Token::ControlChar(':')))
                .then(type_expression_parser())
                .map(|(identifier, type_expr)| (identifier, type_expr) as Param);

            let params_parser = param_parser
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<Param>>()
                .or_not();

            let return_type_parser = just(Token::ControlChar('-'))
                .ignore_then(just(Token::ControlChar('>')))
                .ignore_then(type_expression_parser());

            just(Token::ControlChar('('))
                .ignore_then(params_parser)
                .then_ignore(just(Token::ControlChar(')')))
                .then(return_type_parser.or_not())
                .then_ignore(just(Token::ControlChar('=')))
                .then_ignore(just(Token::ControlChar('>')))
                .then(value_expr_parser.clone())
                .map(|((params, return_type), value_expr)| ValueExpr::Lambda(LambdaFunctionExpr {
                    params: params.unwrap_or_default(),
                    return_type,
                    value_expr,
                }.into()))
        };

        let params = value_expr_parser
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')));

        let tuple =
            lambda_parser.clone().or(
                (just(Token::ControlChar('('))
                    .ignore_then(just(Token::ControlChar(')')))
                    .to(ValueExpr::Tuple(vec![])))
                .or(value_expr_parser
                    .clone()
                    .separated_by(just(Token::ControlChar(',')))
                    .at_least(1)
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                    .map(|x| ValueExpr::Tuple(dbg!(x)))));

        let initializer = just(Token::ControlChar('='))
            .ignore_then(value_expr_parser.clone())
            .or_not();

        let declaration = just(Token::Let)
            .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
            .then_ignore(just(Token::ControlChar(':')))
            .then(type_expression_parser())
            .then(initializer)
            .map(|((identifier, type_expr), initializer)| {
                ValueExpr::VarDecl(
                    Declaration {
                        name: identifier,
                        type_expr,
                        initializer,
                    }
                    .into(),
                )
            });

        let struct_expression = just(Token::ControlChar('.'))
            .ignore_then(
                select_ref! { Token::Ident(ident) => ident.to_owned() }
                    .then_ignore(just(Token::ControlChar(':')))
                    .then(value_expr_parser.clone())
                    .separated_by(just(Token::ControlChar(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
                    .map(ValueExpr::Struct)
            );

        let duck_expression = select_ref! { Token::Ident(ident) => ident.to_owned() }
            .then_ignore(just(Token::ControlChar(':')))
            .then(value_expr_parser.clone())
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
            .map(|mut x| {
                x.sort_by_key(|(name, _)| name.clone());
                ValueExpr::Duck(x)
            });

        let block_expression = value_expr_parser
            .clone()
            .then(just(Token::ControlChar(';')).or_not())
            .repeated()
            .collect::<Vec<_>>()
            .map_err(|error| {
                dbg!(error);
                todo!()
            })
            .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
            .map(|mut exprs| {
                if exprs.len() >= 2 {
                    for (expr, has_semi) in &exprs[..exprs.len() - 1] {
                        if expr.needs_semicolon() && has_semi.is_none() {
                            dbg!(expr, &exprs);
                            panic!("needs_semi")
                        }
                    }
                }

                if exprs.is_empty() || exprs.last().unwrap().1.is_some() {
                    exprs.push((empty_tuple(), None));
                }

                ValueExpr::Block(
                    exprs
                        .into_iter()
                        .map(|(expr, _)| expr)
                        .collect(),
                )
            });

        let if_condition = value_expr_parser
            .clone()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')));
        let if_body = block_expression.clone();
        let if_with_condition_and_body = just(Token::If)
            .ignore_then(if_condition.clone())
            .then(if_body.clone());

        let while_condition = if_condition.clone();
        let while_body = block_expression.clone();
        let while_with_condition_and_body = just(Token::While)
            .ignore_then(while_condition.clone())
            .then(while_body.clone());

        let field_access = none_of(Token::Let)
            .filter(|t| !matches!(t, Token::ControlChar('.')))
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .then(
                (just(Token::ControlChar('.')).ignore_then(
                    select_ref! { Token::Ident(field_name) => field_name.to_owned() },
                ))
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
            )
            .map({
                let e = value_expr_parser.clone();
                move |(base_expr, field_accesses)| {
                    let base_expr = base_expr.leak() as &[Token];
                    dbg!(&base_expr);
                    let base = e.parse(base_expr).unwrap();
                    field_accesses
                        .into_iter()
                        .fold(base, |acc, x| ValueExpr::FieldAccess {
                            target_obj: acc.into(),
                            field_name: x,
                        })
                }
            });

        let int = select_ref! { Token::IntLiteral(i) => *i }.map(ValueExpr::Int);
        let bool_val = select_ref! { Token::BoolLiteral(b) => *b }.map(ValueExpr::Bool);
        let string_val =
            select_ref! { Token::StringLiteral(s) => s.to_owned() }.map(ValueExpr::String);
        let var_expr =
            select_ref! { Token::Ident(ident) => ident.to_owned() }.map(ValueExpr::Variable);
        let if_expr = if_with_condition_and_body
            .clone()
            .then(
                just(Token::Else)
                    .ignore_then(if_with_condition_and_body.clone())
                    .repeated()
                    .collect::<Vec<(ValueExpr, ValueExpr)>>(),
            )
            .then_ignore(just(Token::Else))
            .then(if_body.clone())
            .map(|(((condition, then), else_ifs), r#else)| ValueExpr::If {
                condition: Box::new(condition),
                then: Box::new(then),
                r#else: else_ifs
                    .into_iter()
                    .rfold(Box::new(r#else), |acc, (cond, then)| {
                        Box::new(ValueExpr::If {
                            condition: Box::new(cond),
                            then: Box::new(then),
                            r#else: acc,
                        })
                    }),
            });
        let char_expr = select_ref! { Token::CharLiteral(c) => *c }.map(ValueExpr::Char);
        let float_expr = select_ref! { Token::FloatLiteral(num) => *num }.map(ValueExpr::Float);

        let atom = just(Token::ControlChar('!'))
            .repeated()
            .collect::<Vec<_>>()
            .then(
                value_expr_parser
                    .clone()
                    .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                    .or(choice((
                        field_access.clone(),
                        int,
                        bool_val,
                        string_val,
                        var_expr,
                        if_expr,
                        char_expr,
                        float_expr,
                        tuple,
                        duck_expression,
                        struct_expression,
                        block_expression,
                        just(Token::Break).to(ValueExpr::Break),
                        just(Token::Continue).to(ValueExpr::Continue),
                        while_with_condition_and_body.clone().map(|(cond, body)| {
                            ValueExpr::While {
                                condition: Box::new(cond),
                                body: Box::new(body),
                            }
                        }),
                    ))),
            )
            .then(params.clone().or_not())
            .map(|((neg, target), params)| {
                let res = if let Some(params) = params {
                    ValueExpr::FunctionCall {
                        target: target.into(),
                        params,
                    }
                } else {
                    target
                };

                neg.into_iter()
                    .fold(res, |acc, _| ValueExpr::BoolNegate(acc.into()))
            });

        let assignment = select_ref! { Token::Ident(identifier) => identifier.to_string() }
            .then_ignore(just(Token::ControlChar('=')))
            .then(value_expr_parser.clone())
            .map(|(identifier, value_expr)| {
                ValueExpr::VarAssign(
                    Assignment {
                        name: identifier,
                        value_expr,
                    }
                    .into(),
                )
            });

        let prod = atom
            .clone()
            .then(
                just(Token::ControlChar('*'))
                    .ignore_then(atom.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(init, additional)| {
                additional
                    .into_iter()
                    .fold(init, |acc, x| ValueExpr::Mul(acc.into(), x.into()))
            });

        let add = prod
            .clone()
            .then(
                just(Token::ControlChar('+'))
                    .ignore_then(prod.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(init, additional)| {
                additional
                    .into_iter()
                    .fold(init, |acc, x| ValueExpr::Add(acc.into(), x.into()))
            });

        let equals = add
            .clone()
            .then_ignore(just(Token::Equals))
            .then(add.clone())
            .map(|(x, y)| ValueExpr::Equals(x.into(), y.into()));

        // let equals = add.clone()
        //     .then(just(Token::Equals).ignore_then(add.clone()).repeated().at_least(1).collect::<Vec<_>>())
        //     .map(|(init, additional)| {
        //         additional
        //             .into_iter()
        //             .fold(init, |acc, x| ValueExpr::Equals(acc.into(), x.into()))
        //     });

        let inline_go = select_ref! { Token::InlineGo(x) => x.to_owned() }
            .map(ValueExpr::InlineGo);

        choice((
            inline_go,
            assignment,
            equals,
            add,
            declaration,
            just(Token::Return)
                .ignore_then(value_expr_parser.clone().or_not())
                .map(|x: Option<ValueExpr>| ValueExpr::Return(x.map(Box::new))),
            atom,
        ))
    })
}

fn empty_tuple() -> ValueExpr {
    ValueExpr::Tuple(Vec::new())
}

#[allow(dead_code)]
fn empty_duck() -> ValueExpr {
    ValueExpr::Duck(Vec::new())
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use crate::parse::{
        assignment_and_declaration_parser::Declaration, function_parser::LambdaFunctionExpr, lexer::lexer, type_parser::{Duck, TypeExpression}, value_parser::{emit, empty_duck, empty_tuple, value_expr_parser, EmitEnvironment}
    };

    use super::ValueExpr;

    fn var(x: impl Into<String>) -> Box<ValueExpr> {
        ValueExpr::Variable(x.into()).into()
    }

    #[test]
    fn test_value_expression_parser() {
        let test_cases = vec![
            ("true", ValueExpr::Bool(true)),
            ("false", ValueExpr::Bool(false)),
            (".{ x: 5 }", ValueExpr::Struct(vec![("x".to_string(), ValueExpr::Int(5))])),
            (".{ x: 5, y: .{ x: 5 } }", ValueExpr::Struct(vec![
                ("x".to_string(), ValueExpr::Int(5)),
                ("y".to_string(), ValueExpr::Struct(vec![
                    ("x".to_string(), ValueExpr::Int(5)),
                ])),
            ])),
            ("{}", empty_duck()),
            (
                "to_upper()",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: Vec::new(),
                },
            ),
            (
                "to_upper(1)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper(1,)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper ()",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: Vec::new(),
                },
            ),
            (
                "to_upper (1)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper (1,)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper (   )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: Vec::new(),
                },
            ),
            (
                "to_upper ( 1 )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper ( 1  ,  )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper ( to_lower(1,2,add(5, 10),4), true  )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![
                        ValueExpr::FunctionCall {
                            target: var("to_lower"),
                            params: vec![
                                ValueExpr::Int(1),
                                ValueExpr::Int(2),
                                ValueExpr::FunctionCall {
                                    target: var("add"),
                                    params: vec![ValueExpr::Int(5), ValueExpr::Int(10)],
                                },
                                ValueExpr::Int(4),
                            ],
                        },
                        ValueExpr::Bool(true),
                    ],
                },
            ),
            (
                "print(\"hallo\", \"moin\")",
                ValueExpr::FunctionCall {
                    target: var("print"),
                    params: vec![
                        ValueExpr::String("hallo".into()),
                        ValueExpr::String("moin".into()),
                    ],
                },
            ),
            ("x", ValueExpr::Variable("x".into())),
            (
                "print(x, true, lol())",
                ValueExpr::FunctionCall {
                    target: var("print"),
                    params: vec![
                        ValueExpr::Variable("x".into()),
                        ValueExpr::Bool(true),
                        ValueExpr::FunctionCall {
                            target: var("lol"),
                            params: vec![],
                        },
                    ],
                },
            ),
            (
                "if (true) { 1 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Int(1).into_block().into(),
                    r#else: ValueExpr::Int(2).into_block().into(),
                },
            ),
            (
                "if (true) { 1 } else if (false) { 3 } else if (200) { 4 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Int(1).into_block().into(),
                    r#else: ValueExpr::If {
                        condition: ValueExpr::Bool(false).into(),
                        then: ValueExpr::Int(3).into_block().into(),
                        r#else: ValueExpr::If {
                            condition: ValueExpr::Int(200).into(),
                            then: ValueExpr::Int(4).into_block().into(),
                            r#else: ValueExpr::Int(2).into_block().into(),
                        }
                        .into(),
                    }
                    .into(),
                },
            ),
            (
                "(1,true,2,\"hallo\")",
                ValueExpr::Tuple(vec![
                    ValueExpr::Int(1),
                    ValueExpr::Bool(true),
                    ValueExpr::Int(2),
                    ValueExpr::String("hallo".into()),
                ]),
            ),
            ("{}", empty_duck()),
            ("{1}", ValueExpr::Int(1).into_block()),
            (
                "{1;  2   ;3;x()}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(1),
                    ValueExpr::Int(2),
                    ValueExpr::Int(3),
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    },
                ]),
            ),
            (
                "{1;  2   ;3;x({})}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(1),
                    ValueExpr::Int(2),
                    ValueExpr::Int(3),
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![empty_duck()],
                    },
                ]),
            ),
            (
                "{x();y();}",
                ValueExpr::Block(vec![
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    },
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                    },
                    empty_tuple(),
                ]),
            ),
            (
                "x({ 1; 2; y({ z(); }) }, lol)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            ValueExpr::Int(1),
                            ValueExpr::Int(2),
                            ValueExpr::FunctionCall {
                                target: var("y"),
                                params: vec![ValueExpr::Block(vec![
                                    ValueExpr::FunctionCall {
                                        target: var("z"),
                                        params: vec![],
                                    },
                                    empty_tuple(),
                                ])],
                            },
                        ]),
                        ValueExpr::Variable("lol".into()),
                    ],
                },
            ),
            (
                "while (true) {}",
                ValueExpr::While {
                    condition: ValueExpr::Bool(true).into(),
                    body: empty_tuple().into_block().into(),
                },
            ),
            (
                "while (my_func()) {}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                    }
                    .into(),
                    body: empty_tuple().into_block().into(),
                },
            ),
            (
                "while (my_func()) {1;break;}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                    }
                    .into(),
                    body: ValueExpr::Block(vec![
                        ValueExpr::Int(1),
                        ValueExpr::Break,
                        empty_tuple(),
                    ])
                    .into(),
                },
            ),
            (
                "while (my_func()) {1;continue;}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                    }
                    .into(),
                    body: ValueExpr::Block(vec![
                        ValueExpr::Int(1),
                        ValueExpr::Continue,
                        empty_tuple(),
                    ])
                    .into(),
                },
            ),
            ("()", empty_tuple()),
            (
                "(1.1, 'x')",
                ValueExpr::Tuple(vec![ValueExpr::Float(1.1), ValueExpr::Char('x')]),
            ),
            (
                "{x: 1, y: { z: true }}",
                ValueExpr::Duck(vec![
                    ("x".into(), ValueExpr::Int(1)),
                    (
                        "y".into(),
                        ValueExpr::Duck(vec![("z".into(), ValueExpr::Bool(true))]),
                    ),
                ]),
            ),
            (
                "{x: 1, y: { z: true, w: { print();2;true } }}",
                ValueExpr::Duck(vec![
                    ("x".into(), ValueExpr::Int(1)),
                    (
                        "y".into(),
                        ValueExpr::Duck(vec![
                            (
                                "w".into(),
                                ValueExpr::Block(vec![
                                    ValueExpr::FunctionCall {
                                        target: var("print"),
                                        params: vec![],
                                    },
                                    ValueExpr::Int(2),
                                    ValueExpr::Bool(true),
                                ]),
                            ),
                            ("z".into(), ValueExpr::Bool(true)),
                        ]),
                    ),
                ]),
            ),
            (
                "if (true) {{}} else {{x: 1}}",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Duck(vec![]).into_block().into(),
                    r#else: ValueExpr::Duck(vec![("x".into(), ValueExpr::Int(1))]).into_block().into(),
                },
            ),
            (
                "x.y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Variable("x".into()).into(),
                    field_name: "y".into(),
                },
            ),
            (
                "{x: 123}.y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Duck(vec![("x".into(), ValueExpr::Int(123))]).into(),
                    field_name: "y".into(),
                },
            ),
            (
                "x().y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into(),
                    field_name: "y".into(),
                },
            ),
            (
                "(x)()",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                },
            ),
            (
                "x()",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                },
            ),
            (
                "(1)()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::Int(1).into(),
                    params: vec![],
                },
            ),
            (
                "(123)()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::Int(123).into(),
                    params: vec![],
                },
            ),
            (
                "(returns_lambda())()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::FunctionCall {
                        target: var("returns_lambda"),
                        params: vec![],
                    }
                    .into(),
                    params: vec![],
                },
            ),
            (
                "x.y.z.w",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: var("x"),
                            field_name: "y".into(),
                        }
                        .into(),
                        field_name: "z".into(),
                    }
                    .into(),
                    field_name: "w".into(),
                },
            ),
            ("((1))", ValueExpr::Int(1)),
            (
                "x({();();},1)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![empty_tuple(), empty_tuple(), empty_tuple()]),
                        ValueExpr::Int(1),
                    ],
                },
            ),
            (
                "x({();{();1;};},1)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            empty_tuple(),
                            ValueExpr::Block(vec![empty_tuple(), ValueExpr::Int(1), empty_tuple()]),
                            empty_tuple(),
                        ]),
                        ValueExpr::Int(1),
                    ],
                },
            ),
            (
                "return 123",
                ValueExpr::Return(Some(Box::new(ValueExpr::Int(123).into()))),
            ),
            (
                "let x: String",
                ValueExpr::VarDecl(
                    Declaration {
                        name: "x".into(),
                        initializer: None,
                        type_expr: TypeExpression::TypeName("String".into()),
                    }
                    .into(),
                ),
            ),
            (
                "x() * y()",
                ValueExpr::Mul(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                    }
                    .into(),
                ),
            ),
            (
                "3 * 5",
                ValueExpr::Mul(ValueExpr::Int(3).into(), ValueExpr::Int(5).into()),
            ),
            (
                "3 + 5",
                ValueExpr::Add(ValueExpr::Int(3).into(), ValueExpr::Int(5).into()),
            ),
            (
                "3 * 5 * 6",
                ValueExpr::Mul(
                    ValueExpr::Mul(ValueExpr::Int(3).into(), ValueExpr::Int(5).into()).into(),
                    ValueExpr::Int(6).into(),
                ),
            ),
            (
                "x() * 5 * 6",
                ValueExpr::Mul(
                    ValueExpr::Mul(
                        ValueExpr::FunctionCall {
                            target: var("x"),
                            params: vec![],
                        }
                        .into(),
                        ValueExpr::Int(5).into(),
                    )
                    .into(),
                    ValueExpr::Int(6).into(),
                ),
            ),
            ("!true", ValueExpr::BoolNegate(ValueExpr::Bool(true).into())),
            (
                "!{1;2;true}",
                ValueExpr::BoolNegate(
                    ValueExpr::Block(vec![
                        ValueExpr::Int(1),
                        ValueExpr::Int(2),
                        ValueExpr::Bool(true),
                    ])
                    .into(),
                ),
            ),
            (
                "!x()",
                ValueExpr::BoolNegate(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into(),
                ),
            ),
            (
                "!!x()",
                ValueExpr::BoolNegate(
                    ValueExpr::BoolNegate(
                        ValueExpr::FunctionCall {
                            target: var("x"),
                            params: vec![],
                        }
                        .into(),
                    )
                    .into(),
                ),
            ),
            (
                "!!x.y.z",
                ValueExpr::BoolNegate(
                    ValueExpr::BoolNegate(
                        ValueExpr::FieldAccess {
                            target_obj: ValueExpr::FieldAccess {
                                target_obj: ValueExpr::Variable("x".into()).into(),
                                field_name: "y".into(),
                            }
                            .into(),
                            field_name: "z".into(),
                        }
                        .into(),
                    )
                    .into(),
                ),
            ),
            (
                "!x.y.z",
                ValueExpr::BoolNegate(
                    ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: ValueExpr::Variable("x".into()).into(),
                            field_name: "y".into(),
                        }
                        .into(),
                        field_name: "z".into(),
                    }
                    .into(),
                ),
            ),
            (
                "x() == y()",
                ValueExpr::Equals(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                    }
                    .into(),
                ),
            ),
            (
                "1 == 2",
                ValueExpr::Equals(ValueExpr::Int(1).into(), ValueExpr::Int(2).into()),
            ),
            (
                "!(1 == 2)",
                ValueExpr::BoolNegate(
                    ValueExpr::Equals(ValueExpr::Int(1).into(), ValueExpr::Int(2).into()).into(),
                ),
            ),
            (
                "!1 == !2",
                ValueExpr::Equals(
                    ValueExpr::BoolNegate(ValueExpr::Int(1).into()).into(),
                    ValueExpr::BoolNegate(ValueExpr::Int(2).into()).into(),
                ),
            ),
            (
                "go {}",
                ValueExpr::InlineGo(String::new()),
            ),
            (
                "go { go func() {} }",
                ValueExpr::InlineGo(String::from(" go func() {} ")),
            ),
            (
                "() => {}",
                ValueExpr::Lambda(LambdaFunctionExpr {
                    params: vec![],
                    return_type: None,
                    value_expr: ValueExpr::Duck(vec![])
                }.into())
            ),
            (
                "() => 1",
                ValueExpr::Lambda(LambdaFunctionExpr {
                    params: vec![],
                    return_type: None,
                    value_expr: ValueExpr::Int(1),
                }.into())
            ),
            (
                "() -> Int => 1",
                ValueExpr::Lambda(LambdaFunctionExpr {
                    params: vec![],
                    return_type: Some(TypeExpression::TypeName("Int".into())),
                    value_expr: ValueExpr::Int(1),
                }.into())
            ),
            (
                "(x: String) -> Int => 1",
                ValueExpr::Lambda(LambdaFunctionExpr {
                    params: vec![("x".into(), TypeExpression::TypeName("String".into()))],
                    return_type: Some(TypeExpression::TypeName("Int".into())),
                    value_expr: ValueExpr::Int(1),
                }.into())
            ),
        ];

        for (src, expected_tokens) in test_cases {
            dbg!(src);
            let lex_result = lexer().parse(src).into_result().expect(&src);
            let parse_result = value_expr_parser().parse(&lex_result);

            dbg!(&lex_result, &parse_result);

            assert_eq!(parse_result.has_errors(), false, "{}", src);
            assert_eq!(parse_result.has_output(), true, "{}", src);

            let output: ValueExpr = parse_result.into_result().expect(&src);

            assert_eq!(output, expected_tokens, "{}", src);
        }
    }

    #[test]
    fn test_code_emit() {
        let test_cases = vec![("1", "var_0 := 1\n")];

        for (src, expected_tokens) in test_cases {
            dbg!(src);
            let lex_result = lexer().parse(src).into_result().expect(&src);
            let parse_result = value_expr_parser().parse(&lex_result);

            dbg!(&lex_result, &parse_result);

            assert_eq!(parse_result.has_errors(), false, "{}", src);
            assert_eq!(parse_result.has_output(), true, "{}", src);

            let output: ValueExpr = parse_result.into_result().expect(&src);
            let env = EmitEnvironment::new();
            let output = emit(output, env);
            assert_eq!(
                expected_tokens,
                output
                    .0
                    .into_iter()
                    .reduce(|acc, x| format!("{acc}{x}"))
                    .unwrap(),
                "{}",
                src
            );
        }
    }

    #[test]
    pub fn test_declaration_parser() {
        let inputs_and_expected_outputs = vec![
            (
                "let x: String",
                Declaration {
                    name: "x".to_string(),
                    type_expr: TypeExpression::TypeName("String".to_string()),
                    initializer: None,
                },
            ),
            (
                "let y: { x: Int } = {}",
                Declaration {
                    name: "y".to_string(),
                    type_expr: TypeExpression::Duck(Duck {
                        fields: vec![(
                            "x".to_string(),
                            TypeExpression::TypeName("Int".to_string()),
                        )],
                    }),
                    initializer: Some(ValueExpr::Duck(vec![])),
                },
            ),
            (
                "let z: {}",
                Declaration {
                    name: "z".to_string(),
                    type_expr: TypeExpression::Duck(Duck { fields: vec![] }),
                    initializer: None,
                },
            ),
        ];

        for (input, expected_output) in inputs_and_expected_outputs {
            let lexer_parse_result = lexer().parse(input);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            let declaration_parse_result = value_expr_parser().parse(tokens.as_slice());
            assert_eq!(declaration_parse_result.has_errors(), false);
            assert_eq!(declaration_parse_result.has_output(), true);

            let Some(ValueExpr::VarDecl(declaration)) = declaration_parse_result.into_output()
            else {
                unreachable!()
            };

            assert_eq!(declaration, expected_output.into());
        }

        let valid_declarations = vec![
            "let x: String",
            "let x: { x: String, y: String }",
            "let y: { x: String, y: String }",
            "let z: { h: String, x: { y: String }}",
            "let x: { h: String, x: { y: String }} = 0",
            "let x: { h: String, x: { y: String }} = true",
            "let x: { h: String, x: { y: String }} = false",
            "let x: { h: Int, x: { y: Int }} = { h: 4, x: { y: 8 } }",
            "let x: Int = false",
            "let x: String = \"Hallo, Welt!\"",
            "let x: go sync.WaitGroup"
        ];

        for valid_declaration in valid_declarations {
            println!("lexing {valid_declaration}");
            let lexer_parse_result = lexer().parse(valid_declaration);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("declaration parsing {valid_declaration}");
            let typedef_parse_result = value_expr_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }

    #[test]
    pub fn test_assignment_parser() {
        let valid_assignments = vec![
            "y = 1",
            "{y = 1}",
            "while(true){y = 1}",
            "while(true){y = y + 1}",
            "{let y: Int = 0; while(true){y = 1;@println(y)}}",
            "x = 580",
            "y = 80",
            "y = true",
            "y = false",
            "y = \"Hallo\"",
        ];

        for valid_assignment in valid_assignments {
            dbg!("lexing {valid_assignment}");
            let lexer_parse_result = lexer().parse(valid_assignment);
            dbg!(&lexer_parse_result);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {valid_assignment}");
            let typedef_parse_result = value_expr_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }
}
