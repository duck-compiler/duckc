use crate::ast::{
    AstRoot, Expr, TypeExpression, builder::{expr, expr_stmt, fn_call, fn_def, go_imm, no_type, program, string, type_, var_decl}, memory_target
};

pub fn hello_world_program<'src>() -> AstRoot<'src> {
    program(vec![
        fn_def(
            "println",
            vec![("msg", TypeExpression::String)],
            no_type(),
            vec![
                go_imm("fmt.Println(msg)")
            ],
        ),

        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                expr_stmt(fn_call("printl", vec![string("Hello, World!")]))
            ],
        )
    ])
}

pub fn hello_world_program_with_variable<'src>() -> AstRoot<'src> {
    program(vec![
        fn_def(
            "println",
            vec![("msg", TypeExpression::String)],
            no_type(),
            vec![
                go_imm("fmt.Println(msg)")
            ],
        ),

        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("txt", type_(TypeExpression::String), Some(expr(Expr::StringLiteral("hello, world")))),
                expr_stmt(fn_call("println", vec![]))
            ],
        )
    ])
}
