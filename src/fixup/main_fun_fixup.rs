use crate::parse::function_parser::FunctionDefintion;

fn fixup_main_fun(fun_def: &mut FunctionDefintion) {
    if fun_def.name != "main" { return }
    // TODO: resolve returns in main function to use the exit() fn of go, or wrap the code of the main function into a second function which then get's called inside a new main which just returns exit(other_main())
}
