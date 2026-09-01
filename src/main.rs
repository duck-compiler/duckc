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
            let go_file = std::env::temp_dir().join(format!("duck-{}.go", std::process::id()));
            std::fs::write(&go_file, output.go_source)
                .expect("failed to write generated go source");

            let status = std::process::Command::new(driver::resolve_go_binary())
                .arg("run")
                .arg(&go_file)
                .status()
                .expect("failed to run go");

            let _ = std::fs::remove_file(&go_file);
            std::process::exit(status.code().unwrap_or(1));
        }
        Err(driver::CompileError::Io(message)) | Err(driver::CompileError::Parse(message)) => {
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
