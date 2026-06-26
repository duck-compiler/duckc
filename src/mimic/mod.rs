use crate::ast::{
    AstRoot, TypeExpression, builder::{expr_stmt, fn_call, fn_def, go_imm, no_type, program, string}
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
                expr_stmt(fn_call("println", vec![string("Hello, World!")]))
            ],
        )
    ])
}
