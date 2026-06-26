#![feature(trim_prefix_suffix, is_ascii_octdigit)]

use std::io::{self, Read};

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

    let mut s = frontend::lexer::LexState::init(file_name, &src);

    loop {
        dbg!(s.lex_single());
        let mut buf = [0; 8192];
        io::stdin().read(&mut buf).unwrap();
    }
}
