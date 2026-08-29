use crate::{ast::{
    AstRoot, Expr, TypeExpression, builder::{
        expr, expr_stmt, field_access, field_call, fn_def, generic_call, generic_field_access, generic_field_target, generic_fn_call, generic_method, generic_struct_def, generic_struct_init, ident, mem_name, name_target, named_type, no_type, program, pub_field, pub_struct_def, return_stmt, string, struct_init, type_, use_stmt, var_decl
    }, type_expression::TypeAnnotation
}, backend::gost::go_tree::StructField};

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
        // struct MyStruct { hallo: String }
        pub_struct_def(
            "MyStruct",
            vec![
                ("hallo", TypeExpression::String)
            ]
        ),
        // struct GenericStruct<T> { hallo: String }
        generic_struct_def(
            "GenericStruct",
            vec!["T"],
            vec![
                pub_field("hallo", named_type("T"))
            ],
            vec![
                generic_method(
                    crate::ast::struct_definition::Visibility::Public,
                    crate::ast::struct_definition::MethodKind::Instance,
                    "map",
                    vec!["A"],
                    vec![("tp", named_type("T")), ("ap", named_type("A"))],
                    type_(named_type("A")),
                    vec![
                        return_stmt(Some(mem_name("ap")))
                    ]
                )
            ],
        ),
        // fn main
        fn_def("main", vec![], no_type(), vec![
            var_decl(
                "my_str",
                type_(named_type("MyStruct")),
                Some(struct_init(
                    "MyStruct",
                    vec![
                        ("hallo", string("yoooo"))
                    ]
                ))
            ),
            var_decl(
                "whatever",
                no_type(),
                Some(generic_struct_init(
                    "GenericStruct",
                    vec![TypeExpression::String],
                    vec![("hallo", string("yoooo"))]
                )),
            ),
            expr_stmt(generic_call(
                field_access(name_target("whatever"), "map"),
                vec![],
                vec![
                    field_access(name_target("whatever"), "hallo"),
                    field_access(name_target("whatever"), "hallo"),
                ]
            )),
            expr_stmt(field_call("fmt", "Println", vec![field_access(name_target("whatever"), "hallo")]))
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
