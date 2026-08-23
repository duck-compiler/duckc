use crate::{ast::Span, backend::semantics::symbol::SymbolKind};

#[derive(Debug)]
pub enum DiagnosticKind {
    Warning,
    Error,
}

#[derive(Debug)]
pub struct Diagnostic<'src> {
    pub message: Box<str>,
    pub location: Span<'src>,
    pub error_code: Box<str>,
    pub kind: DiagnosticKind,
}

impl<'src> Diagnostic<'src> {
    pub fn symbol_not_found(
        kind: SymbolKind,
        name: &'src str,
        location: Span<'src>
    ) -> Diagnostic<'src> {
        let message = match kind {
            SymbolKind::Function => format!("function not found: {}", name),
            SymbolKind::Variable => format!("variable not found: {}", name),
            SymbolKind::Struct => format!("struct not found: {}", name),
            SymbolKind::Param => format!("parameter not found: {}", name),
            SymbolKind::Module => format!("package not found: {}", name),
        };

        let error_code = format!("S0001");

        Diagnostic {
            message: message.into_boxed_str(),
            location: location,
            error_code: error_code.into_boxed_str(),
            kind: DiagnosticKind::Error
        }
    }

    pub fn type_mismatch(
        expected: &str,
        found: &str,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("type mismatch: expected {}, found {}", expected, found).into_boxed_str(),
            location,
            error_code: "T0001".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_callable(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "expression is not callable".to_string().into_boxed_str(),
            location,
            error_code: "T0002".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn wrong_arg_count(
        expected: usize,
        found: usize,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("wrong number of arguments: expected {}, found {}", expected, found).into_boxed_str(),
            location,
            error_code: "T0003".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn empty_array_literal(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "cannot infer type of empty array literal".to_string().into_boxed_str(),
            location,
            error_code: "T0005".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_indexable(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "expression is not indexable".to_string().into_boxed_str(),
            location,
            error_code: "T0006".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn unknown_package_member(
        package: &str,
        name: &str,
        reason: &str,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("unknown or unsupported member `{}.{}`: {}", package, name, reason).into_boxed_str(),
            location,
            error_code: "T0004".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_a_struct(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "field access on a value that is not a struct".to_string().into_boxed_str(),
            location,
            error_code: "T0007".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn unknown_struct_field(
        struct_name: &str,
        field_name: &str,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("struct `{}` has no field `{}`", struct_name, field_name).into_boxed_str(),
            location,
            error_code: "T0008".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn missing_struct_fields(
        struct_name: &str,
        missing: &[&str],
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("missing field(s) in initializer of `{}`: {}", struct_name, missing.join(", ")).into_boxed_str(),
            location,
            error_code: "T0009".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_yet_supported(feature: &str, location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("{} is not supported yet", feature).into_boxed_str(),
            location,
            error_code: "T0010".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_allowed_at_top_level(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "only `use`, `fn`, and `struct` declarations are allowed at the top level".to_string().into_boxed_str(),
            location,
            error_code: "T0011".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn break_or_continue_outside_loop(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "`break`/`continue` can only be used inside a `while` loop".to_string().into_boxed_str(),
            location,
            error_code: "T0012".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }
}
