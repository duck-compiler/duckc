mod ast;
mod backend;
mod frontend;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    if args.len() != 1 {
        println!("Usage: duckc <filename>");
        return;
    }

    let file_name = &args[0];
    let src = match std::fs::read_to_string(file_name) {
        Ok(src) => src,
        Err(e) => {
            println!("Error reading {file_name}: {e:?}");
            return;
        }
    };
}
