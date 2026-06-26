use crate::type_resolve::{ModuleId, ModuleTables, TypeEnv};

mod ast;
mod backend;
mod type_resolve;
mod frontend;
mod mimic;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    if args.len() != 1 {
        println!("Usage: duckc <filename>");
        return;
    }

    let file_name = &args[0];
    let _src = match std::fs::read_to_string(file_name) {
        Ok(src) => src,
        Err(e) => {
            println!("Error reading {file_name}: {e:?}");
            return;
        }
    };

    let mut ast = mimic::hello_world_program();

    let count = ast::assign_generate_node_ids(&mut ast);

    let mut type_env = TypeEnv::new();
    let module = ModuleId(0);

    let root = type_env.new_scope(None);
    type_env.modules.push(ModuleTables::new(ast, count, root));

    type_resolve::collect_module(module, &mut type_env);
    type_resolve::resolve_scopes(module, &mut type_env);

    let ast = std::mem::replace(
        &mut type_env.modules[module.0 as usize].ast,
        ast::AstRoot { statements: Vec::new() }
    );

    let gost = backend::translate(ast);
    let go_src = backend::emit_gost(gost);

    println!("{go_src}");
}
