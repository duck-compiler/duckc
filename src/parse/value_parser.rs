use std::{cell::RefCell, rc::Rc};

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
    FieldAccess {
        target_obj: Box<ValueExpr>,
        field_name: String,
    },
    Return(Option<Box<ValueExpr>>),
}

impl ValueExpr {
    fn flatten_block(&self) -> ValueExpr {
        match self {
            ValueExpr::Block(x) if x.len() <= 1 => {
                if let Some(x) = x.get(0) {
                    x.clone()
                } else {
                    empty_tuple()
                }
            }
            _ => self.clone(),
        }
    }
}

pub fn emit(x: ValueExpr, var_counter: Rc<RefCell<usize>>) -> (Vec<String>, Option<String>) {
    let new_var = || {
        let x = format!("var_{}", var_counter.borrow());
        *var_counter.borrow_mut() += 1;
        x
    };

    let single = |instr: &str| {
        let r = new_var();
        (vec![format!("{r} := {instr}\n")], Some(r.to_string()))
    };

    let no_var = |instr: &str| (vec![instr.to_owned()], None);

    match x {
        ValueExpr::While { condition, body } => {
            let (cond_instr, Some(cond_res)) = emit(*condition, Rc::clone(&var_counter)) else {
                panic!()
            };
            let mut instr = Vec::new();

            instr.push("for {\n".to_string());
            instr.extend(cond_instr.into_iter());
            instr.push(format!("if !{} {}\n", cond_res, "{"));
            instr.push("break\n".to_string());
            instr.push("}\n".to_string());

            let (body_instr, _) = emit(*body, Rc::clone(&var_counter));
            instr.extend(body_instr.into_iter());

            instr.push("}\n".to_string());

            (instr, None)
        }
        ValueExpr::Return(expr) => {
            let mut instr = Vec::new();
            if let Some(expr) = expr {
                let (expr_instr, Some(expr_res)) = emit(*expr, Rc::clone(&var_counter)) else {
                    panic!()
                };
                instr.extend(expr_instr.into_iter());
                instr.push(format!("return {expr_res}"));
            } else {
                instr.push(format!("return\n"));
            }
            (instr, None)
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            let res_var = new_var();
            let (cond_instr, Some(cond_res)) = emit(
                ValueExpr::clone(condition.as_ref()),
                Rc::clone(&var_counter),
            ) else {
                panic!()
            };

            let mut res_instr = Vec::new();
            res_instr.extend(cond_instr.into_iter());
            res_instr.push(format!("var {res_var} interface{}\n", "{}"));
            res_instr.push(format!("if {} {}\n", cond_res, "{"));
            let (then_instr, res) = emit(ValueExpr::clone(then.as_ref()), Rc::clone(&var_counter));
            res_instr.extend(then_instr.into_iter());
            if let Some(res) = res {
                res_instr.push(format!("{res_var} = {res}\n"))
            }
            res_instr.push("} else {\n".to_string());
            let (r#else_instr, res) =
                emit(ValueExpr::clone(r#else.as_ref()), Rc::clone(&var_counter));
            res_instr.extend(r#else_instr.into_iter());
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
                let (instr, Some(res)) = emit(expr, Rc::clone(&var_counter)) else {
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
            instrs.extend(final_instr.into_iter());
            (instrs, Some(res))
        }
        ValueExpr::FunctionCall { target, params } => {
            let mut res = Vec::new();
            let mut target = ValueExpr::clone(target.as_ref());

            let mut with_result = true;
            if let ValueExpr::Variable(x) = &mut target {
                if x == "@println" {
                    *x = "fmt.Println".to_string();
                    with_result = false;
                }
            }

            let (target_instr, Some(target_res_name)) = emit(target, Rc::clone(&var_counter))
            else {
                panic!()
            };
            dbg!(&target_instr);
            res.extend(target_instr.into_iter());

            let mut params_instructions = Vec::new();
            let mut param_results = Vec::new();

            for expr in params.into_iter() {
                let (instr, Some(res)) = emit(expr, Rc::clone(&var_counter)) else {
                    panic!()
                };
                params_instructions.extend(instr.into_iter());

                param_results.push(res);
            }

            let result = new_var();

            let final_instr = format!(
                "{}{target_res_name}({})\n",
                if with_result { "{result} := " } else { "" },
                param_results
                    .clone()
                    .into_iter()
                    .reduce(|acc, x| format!("{acc}, {x}"))
                    .unwrap_or(String::new())
            );

            res.extend(params_instructions.into_iter());
            res.push(final_instr);

            (res, Some(result))
        }
        ValueExpr::Block(exprs) => {
            let mut instrs = Vec::new();
            let mut res = Vec::new();
            for expr in exprs {
                let (instr, res_var) = emit(expr, Rc::clone(&var_counter));
                instrs.extend(instr.into_iter());
                if let Some(res_var) = res_var {
                    res.push(res_var);
                }
            }
            (instrs, res.last().map(ToOwned::to_owned))
        }
        _ => {
            dbg!(x);
            todo!()
        }
    }
}

pub fn value_expr_parser<'src>() -> impl Parser<'src, &'src [Token], ValueExpr> {
    recursive(|e| {
        let params = e
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')));

        let tuple = (just(Token::ControlChar('('))
            .ignore_then(just(Token::ControlChar(')')))
            .to(ValueExpr::Tuple(vec![])))
        .or(e
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .at_least(1)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
            .map(|x| ValueExpr::Tuple(dbg!(x))));

        let duck_expression = select_ref! { Token::Ident(ident) => ident.to_owned() }
            .then_ignore(just(Token::ControlChar(':')))
            .then(e.clone())
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
            .map(|x| ValueExpr::Duck(x));

        let block_expression = e
            .clone()
            .then(just(Token::ControlChar(';')).or_not())
            .repeated()
            .collect::<Vec<_>>()
            .map_err(|e| {
                dbg!(e);
                todo!()
            })
            .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
            .map(|mut x| {
                if x.is_empty() || x.last().unwrap().1.is_some() {
                    x.push((empty_tuple(), None));
                }
                ValueExpr::Block(x.into_iter().map(|(t, _)| t).collect()).flatten_block()
            });

        let if_condition = e
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

        let field_access = any()
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
                let e = e.clone();
                move |(base_expr, field_accesses)| {
                    let base_expr = base_expr.leak() as &[Token];
                    dbg!(&base_expr);
                    let base = e.parse(base_expr).unwrap();
                    let r =
                        field_accesses
                            .into_iter()
                            .fold(base, |acc, x| ValueExpr::FieldAccess {
                                target_obj: acc.into(),
                                field_name: x,
                            });
                    r
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

        let atom = e
            .clone()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
            .or(choice((
                field_access,
                int,
                bool_val,
                string_val,
                var_expr,
                if_expr,
                char_expr,
                float_expr,
                tuple,
                duck_expression,
                block_expression,
                just(Token::Break).to(ValueExpr::Break),
                just(Token::Continue).to(ValueExpr::Continue),
                while_with_condition_and_body
                    .clone()
                    .map(|(cond, body)| ValueExpr::While {
                        condition: Box::new(cond),
                        body: Box::new(body),
                    }),
            )));

        choice((
            atom.clone()
                .then(params.clone())
                .map(|(target, params)| ValueExpr::FunctionCall {
                    target: target.into(),
                    params,
                }),
            just(Token::Return)
                .ignore_then(atom.clone().or_not())
                .map(|x: Option<ValueExpr>| ValueExpr::Return(x.map(|x| Box::new(x)))),
            atom,
        ))
    })
}

fn empty_tuple() -> ValueExpr {
    ValueExpr::Tuple(Vec::new())
}

fn empty_duck() -> ValueExpr {
    ValueExpr::Duck(Vec::new())
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use chumsky::Parser;

    use crate::parse::{
        lexer::lexer,
        value_parser::{emit, empty_duck, empty_tuple, value_expr_parser},
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
                    then: ValueExpr::Int(1).into(),
                    r#else: ValueExpr::Int(2).into(),
                },
            ),
            (
                "if (true) { 1 } else if (false) { 3 } else if (200) { 4 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Int(1).into(),
                    r#else: ValueExpr::If {
                        condition: ValueExpr::Bool(false).into(),
                        then: ValueExpr::Int(3).into(),
                        r#else: ValueExpr::If {
                            condition: ValueExpr::Int(200).into(),
                            then: ValueExpr::Int(4).into(),
                            r#else: ValueExpr::Int(2).into(),
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
            ("{1}", ValueExpr::Int(1)),
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
                    body: empty_tuple().into(),
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
                    body: empty_tuple().into(),
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
                            ("z".into(), ValueExpr::Bool(true)),
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
                        ]),
                    ),
                ]),
            ),
            (
                "if (true) {{}} else {{x: 1}}",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Duck(vec![]).into(),
                    r#else: ValueExpr::Duck(vec![("x".into(), ValueExpr::Int(1))]).into(),
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
        let test_cases = vec![(
            "@println(1, 2, 3, true)",
            "var_0 := fmt.Println\nvar_1 := 1\nvar_2 := 2\nvar_3 := 3\nvar_4 := true\nvar_5 := var_0(var_1, var_2, var_3, var_4)\n",
        )];

        for (src, expected_tokens) in test_cases {
            dbg!(src);
            let lex_result = lexer().parse(src).into_result().expect(&src);
            let parse_result = value_expr_parser().parse(&lex_result);

            dbg!(&lex_result, &parse_result);

            assert_eq!(parse_result.has_errors(), false, "{}", src);
            assert_eq!(parse_result.has_output(), true, "{}", src);

            let output: ValueExpr = parse_result.into_result().expect(&src);
            let output = emit(output, Rc::new(RefCell::new(0)));
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
}
