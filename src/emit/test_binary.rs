use crate::parse::source_file_parser::SourceFile;

fn emit_test_source(source_file: &SourceFile) {
    for test_case in &source_file.test_cases {
        println!("test case {}", test_case.name);
    }
}
