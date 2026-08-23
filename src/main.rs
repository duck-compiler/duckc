mod ast;
mod backend;
mod driver;
mod frontend;
mod mimic;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    if args.len() != 1 {
        eprintln!("Usage: duckc <filename>");
        std::process::exit(1);
    }

    match driver::run(&args[0]) {
        Ok(output) => {
            println!("{}", output.go_source);
        }
        Err(driver::CompileError::Io(message)) => {
            eprintln!("{message}");
            std::process::exit(1);
        }
        Err(driver::CompileError::Diagnostics(messages)) => {
            for message in &messages {
                eprintln!("{message}");
            }
            std::process::exit(1);
        }
    }
}
