use crate::ast::{
    AstRoot, Expr, TypeExpression, builder::{
        expr, expr_stmt, field_access, field_call, fn_def, ident, int, mem_name, name_target, no_type, program, string, struct_def, struct_init, type_, use_stmt, var_decl
    }, type_expression::TypeAnnotation
};

pub fn hello_world_program<'src>() -> AstRoot<'src> {
    program(vec![
        use_stmt("fmt", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                expr_stmt(field_call("fmt", "Println", vec![string("hallo")]))
            ],
        )
    ])
}

pub fn test_struct_program<'src>() -> AstRoot<'src> {
    program(vec![
        use_stmt("fmt", None),
        struct_def(
            "MyStruct",
            vec![
                ("hallo", TypeExpression::String)
            ]
        ),
        fn_def("main", vec![], no_type(), vec![
            var_decl(
                "my_str",
                type_(TypeExpression::Ident(ident("MyStruct"))),
                Some(struct_init(
                    "MyStruct",
                    vec![
                        ("hallo", string("yoooo"))
                    ]
                ))
            ),
            expr_stmt(field_call("fmt", "Println", vec![field_access(name_target("my_str"), "hallo")]))
        ]),
    ])
}

pub fn hello_world_program_with_variable<'src>() -> AstRoot<'src> {
    program(vec![
        use_stmt("fmt", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("txt", type_(TypeExpression::String), Some(expr(Expr::StringLiteral("hello, world")))),
                expr_stmt(field_call("fmt", "Println", vec![]))
            ],
        )
    ])
}
