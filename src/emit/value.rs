use std::{cell::RefCell, rc::Rc};

use crate::{
    parse::{
        assignment_and_declaration_parser::{Assignment, Declaration},
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

pub fn emit(
    x: ValueExpr,
    env: EmitEnvironment,
    type_env: &mut TypeEnv,
) -> (Vec<String>, Option<String>) {
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
            let LambdaFunctionExpr {
                params,
                return_type,
                value_expr,
            } = *expr;
            for param in &params {
                param.1.as_go_implementation(type_env);
            }
            let (mut v_instr, res_name) = emit(value_expr.0, env.clone(), type_env);
            if let Some(res_name) = res_name {
                v_instr.push(format!("_ = {res_name}\n"))
            }

            let res_var = new_var();
            let mut instr = Vec::new();
            let lambda_creation = format!(
                "{res_var} := func({}) {} {}\n",
                params
                    .iter()
                    .map(|(name, t)| format!("{name} {}", t.emit().0))
                    .collect::<Vec<_>>()
                    .join(", "),
                return_type.map(|t| t.emit().0).unwrap_or_default(),
                "{"
            );

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
            let (mut x_instr, Some(x_res)) = emit(x.0.clone(), env.clone(), type_env) else {
                panic!("No result provided")
            };
            let (y_instr, Some(y_res)) = emit(y.0.clone(), env.clone(), type_env) else {
                panic!("No result provided")
            };
            x_instr.extend(y_instr);
            let res_var = new_var();
            x_instr.push(format!("{res_var} := {x_res} == {y_res}\n"));
            (x_instr, Some(res_var))
        }
        ValueExpr::BoolNegate(expr) => {
            let res_var = new_var();
            let (mut expr_instr, Some(expr_res)) = emit(expr.0.clone(), env.clone(), type_env)
            else {
                panic!("No result provided")
            };
            expr_instr.push(format!("{res_var} := !{expr_res}\n"));
            (expr_instr, Some(res_var))
        }
        ValueExpr::Add(x, y) => {
            let (mut x_instr, Some(x_res)) = emit(x.0.clone(), env.clone(), type_env) else {
                panic!("No result provided")
            };
            let (y_instr, Some(y_res)) = emit(y.0.clone(), env.clone(), type_env) else {
                panic!("No result provided")
            };
            x_instr.extend(y_instr);
            let res_var = new_var();
            x_instr.push(format!("{res_var} := {x_res} + {y_res}\n"));
            (x_instr, Some(res_var))
        }
        ValueExpr::Mul(x, y) => {
            let (mut x_instr, Some(x_res)) = emit(x.0.clone(), env.clone(), type_env) else {
                panic!("No result provided")
            };
            let (y_instr, Some(y_res)) = emit(y.0.clone(), env.clone(), type_env) else {
                panic!("No result provided")
            };
            x_instr.extend(y_instr);
            let res_var = new_var();
            x_instr.push(format!("{res_var} := {x_res} * {y_res}\n"));
            (x_instr, Some(res_var))
        }
        ValueExpr::VarDecl(b) => {
            let (
                Declaration {
                    name,
                    type_expr: t,
                    initializer,
                },
                _,
            ) = *b;
            if let Some(initializer) = initializer {
                let (instr, Some(res_var)) = emit(initializer.0.clone(), env.clone(), type_env)
                else {
                    panic!("No result provided")
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
            let (Assignment { name, value_expr }, _) = *b;
            let (mut instr, Some(res)) = emit(value_expr.0.clone(), env.clone(), type_env) else {
                panic!("No result provided")
            };
            instr.push(format!("{name} = {res}\n"));
            (instr, Some(name))
        }
        ValueExpr::While { condition, body } => {
            let (cond_instr, Some(cond_res)) = emit(condition.0.clone(), env.clone(), type_env)
            else {
                panic!("No result provided")
            };
            let mut instr = Vec::new();

            instr.push("for {\n".to_string());
            instr.extend(cond_instr);
            instr.push(format!("if !{} {}\n", cond_res, "{"));
            instr.push("break\n".to_string());
            instr.push("}\n".to_string());

            let (body_instr, _) = emit(body.0.clone(), env.clone(), type_env);
            instr.extend(body_instr);

            instr.push("}\n".to_string());

            (instr, None)
        }
        ValueExpr::Return(expr) => {
            let mut instr = Vec::new();
            if let Some(expr) = expr {
                let (expr_instr, Some(expr_res)) = emit(expr.0.clone(), env.clone(), type_env)
                else {
                    panic!("No result provided")
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
            let (cond_instr, Some(cond_res)) = emit(condition.0.clone(), env.clone(), type_env)
            else {
                panic!("No result provided")
            };

            let mut res_instr = Vec::new();
            res_instr.extend(cond_instr);
            res_instr.push(format!("var {res_var} interface{}\n", "{}"));
            res_instr.push(format!("if {} {}\n", cond_res, "{"));
            let (then_instr, res) = emit(then.as_ref().0.clone(), env.clone(), type_env);
            res_instr.extend(then_instr);
            if let Some(res) = res {
                res_instr.push(format!("{res_var} = {res}\n"))
            }
            res_instr.push("} else {\n".to_string());
            if let Some(r#else) = r#else {
                let (r#else_instr, res) = emit(r#else.as_ref().0.clone(), env.clone(), type_env);
                res_instr.extend(r#else_instr);
                if let Some(res) = res {
                    res_instr.push(format!("{res_var} = {res}\n"))
                }
            }
            res_instr.push("\n}\n".to_string());

            (res_instr, Some(res_var))
        }
        ValueExpr::Break => no_var("break"),
        ValueExpr::Continue => no_var("continue"),
        ValueExpr::Int(i) => single(&i.to_string()),
        ValueExpr::Bool(b) => single(&b.to_string()),
        ValueExpr::Float(f) => single(&f.to_string()),
        ValueExpr::Char(c) => single(&format!("'{c}'")),
        ValueExpr::String(s) => single(&format!("\"{s}\"")),
        ValueExpr::Variable(ident, ..) => single(&ident),
        ValueExpr::Tuple(exprs) => {
            let mut instrs: Vec<String> = Vec::new();
            let mut results = Vec::new();
            for expr in exprs.into_iter() {
                let (instr, Some(res)) = emit(expr.0.clone(), env.clone(), type_env) else {
                    panic!("No result provided")
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
            let mut target = target.as_ref().0.clone();

            let mut with_result = true;
            if let ValueExpr::Variable(x, ..) = &mut target
                && x == "@println"
            {
                // TODO: adds fmt import
                let package_name = env
                    .push_import(GoImport {
                        alias: None,
                        path: "fmt".into(),
                    })
                    .unwrap_or("fmt".into());
                *x = format!("{package_name}.Println").to_string();
                with_result = false;
            }

            let (target_instr, Some(target_res_name)) = emit(target, env.clone(), type_env) else {
                panic!("No result provided")
            };
            res.extend(target_instr);

            let mut params_instructions = Vec::new();
            let mut param_results = Vec::new();

            for expr in params.into_iter() {
                let (instr, Some(res)) = emit(expr.0.clone(), env.clone(), type_env) else {
                    panic!("No result provided")
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
                let (this_field_instr, Some(this_field_res)) =
                    emit(field_init.0.clone(), env.clone(), type_env)
                else {
                    panic!("No result provided")
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
                let (this_field_instr, Some(this_field_res)) =
                    emit(field_init.0.clone(), env.clone(), type_env)
                else {
                    panic!("No result provided")
                };
                field_instr.extend(this_field_instr.into_iter());
                field_res.push(this_field_res);
                let go_type_name = "interface{}".to_string();
                type_fields.push((field_name.clone(), go_type_name.clone()));
                // TODO: types
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
                        (ValueExpr::Return(_), _) => Some(i),
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
                let (instr, res_var) = emit(expr.0.clone(), env.clone(), type_env);
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
            todo!()
        }
    }
}
