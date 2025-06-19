use std::{cell::RefCell, error::Error, rc::Rc};

use chumsky::Parser;
use parse::lexer::lexer;

use crate::parse::value_parser::{
    EmitEnvironment, GoMethodDef, GoTypeDef, emit, value_expr_parser,
};

pub mod parse;

fn main() -> Result<(), Box<dyn Error>> {
    if true {
        // let src = "if (x(1,true, (1,2,3,\"ABC\", (50, 100)))) { (\"lol\", 1) } else { return; }";
        // let src = "if ({@println(1); true}) {1} else {2}";
        // let src = "{ let i: Int = 0; while(!(i == 5)) {i = i + 1;@println(i);} }";
        let src = "{{ x: 1, y: \"lol\" };@println({ y: \"lol\", x: \"fisch\" });}";
        // let src = "{ let x: Int = 0; while (x ) }";
        // let src = "{i = 10;}";

        // let mut i = 0;
        // while {println!("{i}"); i+= 1; if i == 500 {return Ok(());} i < 1000} {
        //     dbg!(2);
        // }

        let lex = lexer().parse(src).unwrap();
        let parse = value_expr_parser().parse(&lex).unwrap();
        let emit_env = EmitEnvironment {
            imports: Rc::new(RefCell::new(Vec::new())),
            types: Rc::new(RefCell::new(Vec::new())),
            var_counter: Rc::new(RefCell::new(0)),
        };
        let emitted = emit(parse, emit_env.clone());

        let types = [
            GoTypeDef::Struct {
                name: "HasX".into(),
                fields: vec![("x".into(), "interface{}".into())],
                methods: vec![GoMethodDef {
                    name: "GetX".into(),
                    body: vec!["return self.x".into()],
                    params: vec![],
                    return_type: Some("interface{}".into()),
                }],
            },
            GoTypeDef::Interface {
                name: "HasX_Any".into(),
                methods: vec![GoMethodDef {
                    name: "GetX".into(),
                    body: vec!["return self.x".into()],
                    params: vec![],
                    return_type: Some("interface{}".into()),
                }],
            },
        ];
        std::fs::write(
            "outgen.go",
            types[0]
                .emit()
                .into_iter()
                .reduce(|acc, x| format!("{acc}{x}"))
                .unwrap(),
        )
        .unwrap();
        std::fs::write(
            "outgen_2.go",
            types[1]
                .emit()
                .into_iter()
                .reduce(|acc, x| format!("{acc}{x}"))
                .unwrap(),
        )
        .unwrap();

        emit_env.imports.borrow_mut().push("fmt".to_string());
        emit_env.imports.borrow_mut().push("io".to_string());
        let t = emit_env.emit_all();

        std::fs::write(
            "outgen.go",
            format!("{}{}", t, emitted.0.join(""))
        )
        .unwrap();
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

    Ok(())
}
