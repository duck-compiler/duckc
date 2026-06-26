use crate::backend::{gost, semantics::{self, context::SemanticsContext, module::{ModuleId, ModuleTables}}};

mod ast;
mod backend;
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

    let mut context = SemanticsContext::new();
    let module = ModuleId(0);

    let root = context.new_scope(None);
    context.modules.push(ModuleTables::new(ast, count, root));

    semantics::passes::collect_module(module, &mut context);
    semantics::passes::resolve_module(module, &mut context);

    let gost = gost::translate(&context, module);
    let go_src = gost::emit_gost(gost);

    println!("{go_src}");
}
