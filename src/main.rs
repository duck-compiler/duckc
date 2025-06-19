use std::{cell::RefCell, error::Error, rc::Rc};

use chumsky::Parser;
use parse::lexer::lexer;

use crate::parse::value_parser::{emit, value_expr_parser};

pub mod parse;

fn main() -> Result<(), Box<dyn Error>> {
    if true {
        let src = "if (x(1,true, (1,2,3,\"ABC\", (50, 100)))) { (\"lol\", 1) } else { return; }";
        let src = "if ({@println(1); true}) {1} else {2}";
        let src = "while ({(1,2,3);x(true);false}) {@println(\"kek\");@println(\"moin\")}";
        let src = "{let x: String = 1; x = 2; let y: String;}";

        // let mut i = 0;
        // while {println!("{i}"); i+= 1; if i == 500 {return Ok(());} i < 1000} {
        //     dbg!(2);
        // }

        let lex = lexer().parse(src).unwrap();
        let parse = value_expr_parser().parse(&lex).unwrap();
        let emitted = emit(parse, Rc::new(RefCell::new(0)));
        std::fs::write("outgen.go", &emitted.0.into_iter().reduce(|acc, x| {
            format!("{acc}{x}")
        }).unwrap_or(String::new())).unwrap();
        return Ok(());
    }


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
