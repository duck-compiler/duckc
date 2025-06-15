use std::error::Error;

use chumsky::{input::Input, Parser};
use parse::{lexer::lexer};

pub mod parse;

fn main() -> Result<(), Box<dyn Error>> {
    let source = Box::leak(Box::new("duck { x: String };"));
    let lexer_parse_result = lexer().parse(source);
    if !lexer_parse_result.has_output() {
        panic!("Error while lexing");
    }

    let tokens = lexer_parse_result.output().unwrap().as_slice();
    dbg!(tokens);

    // let type_parser_result = type_parser().parse(tokens.iter().map(|(token, span)| token).collect());
    // if type_parser_result.has_errors() {
      //  type_parser_result.errors().for_each(|err| println!("Error => {}", err));
      // }

    // if !type_parser_result.has_output() {
        //panic!("Error while type parsing");
        //}

   // let type_expressions = type_parser_result.output();

    return Ok(())
}
