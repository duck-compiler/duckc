use crate::parse::function_parser::FunctionDefintion;

fn fixup_main_fun(fun_def: &mut FunctionDefintion) {
    if fun_def.name != "main" { return }

}
